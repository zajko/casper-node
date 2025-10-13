pub(crate) mod altbn128;
pub(crate) mod emit;
pub(crate) mod global_state;
use std::{borrow::Cow, collections::BTreeMap, num::NonZeroU32, sync::Arc};

use bytes::Bytes;
use casper_executor_wasm_common::{
    chain_utils,
    entry_point::{
        ENTRY_POINT_PAYMENT_CALLER, ENTRY_POINT_PAYMENT_DIRECT_INVOCATION_ONLY,
        ENTRY_POINT_PAYMENT_SELF_ONWARD,
    },
    error::{
        CallError, CALLEE_NOT_CALLABLE, CALLEE_SUCCEEDED, CALLEE_TRAPPED, HOST_ERROR_INVALID_DATA,
        HOST_ERROR_INVALID_INPUT, HOST_ERROR_MAX_MESSAGES_PER_BLOCK_EXCEEDED,
        HOST_ERROR_MESSAGE_TOPIC_FULL, HOST_ERROR_NOT_FOUND, HOST_ERROR_PAYLOAD_TOO_LONG,
        HOST_ERROR_SUCCESS, HOST_ERROR_TOO_MANY_TOPICS, HOST_ERROR_TOPIC_TOO_LONG,
    },
    flags::ReturnFlags,
    keyspace::{Keyspace, KeyspaceTag},
};
use casper_executor_wasm_interface::{
    executor::{
        ControlMethods, CryptoMethods, EmitMethods, ExecuteError, ExecuteRequestBuilder,
        ExecuteResult, ExecutionKind, Executor, GlobalStateMethods, IOMethods, SystemContractCall,
    },
    u32_from_host_result, Caller, FatalHostError, VMError, VMResult,
};
use casper_storage::{global_state::GlobalStateReader, tracking_copy::TrackingCopyExt};
use casper_types::{
    account::AccountHash,
    addressable_entity::{
        ActionThresholds, AssociatedKeys, MessageTopicError, NamedKeyAddr, NamedKeyValue,
    },
    bytesrepr::{FromBytes, ToBytes},
    contract_messages::{Message, MessageAddr, MessagePayload, MessageTopicSummary},
    execution::RetValue,
    AccessRights, AddressableEntity, BlockGlobalAddr, BlockHash, BlockTime, ByteCode, ByteCodeAddr,
    ByteCodeHash, ByteCodeKind, CLType, CLValue, Contract, ContractRuntimeTag, ContractWasmHash,
    Digest, EntityAddr, EntityKind, EntryPointPayment, EntryPointValue, HashAddr, HashAlgorithm,
    HostFFIFunctionCost, Key, NamedKeys, Package, PackageAddr, ProtocolVersion, Signature,
    StoredValue, URef,
};
use either::Either;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use tracing::{debug, error, info, warn};

use crate::{
    abi::{CreateResult, EnvInfo},
    context::Context,
    host::{
        emit::{emit, print_std},
        global_state::{host_read, host_write},
    },
    system,
};
use blake2::{
    digest::{Update, VariableOutput},
    Blake2bVar,
};
use casper_executor_wasm_common::{
    chain_utils::{compute_next_contract_hash_version, compute_wasm_bytecode_hash},
    error::{HOST_ERROR_CL_VALUE, HOST_LOCKED_PACKAGE, HOST_NO_ACTIVE_CONTRACT},
};
use casper_executor_wasm_interface::executor::{
    AuctionMethods, ExecuteRequest, FFIMenu, MintMethods,
};
use casper_types::contracts::{ContractHash, ContractPackage, ContractPackageHash, EntryPoints};
use keccak_asm::Digest as KeccakDigest;
use sha2::Sha256;

const NAME_FOR_V2_CONTRACT_MAIN_PURSE: &str = "__main_purse";

#[derive(Debug, Copy, Clone, FromPrimitive, PartialEq)]
enum EntityKindTag {
    Account = 0,
    Contract = 1,
}

pub trait FallibleInto<T> {
    fn wrapped_try_into(self) -> VMResult<T>;
}

impl<From, To> FallibleInto<To> for From
where
    To: TryFrom<From>,
{
    fn wrapped_try_into(self) -> VMResult<To> {
        To::try_from(self).map_err(|_| VMError::Fatal(FatalHostError::TypeConversion))
    }
}

/// Consumes imputed amount of gas.
fn charge_gas<S: GlobalStateReader>(
    caller: &mut impl Caller<Context = Context<S>>,
    imputed: u64,
) -> VMResult<()> {
    caller.consume_gas(imputed)?;
    Ok(())
}

/// Consumes a set amount of gas for the specified storage value.
fn charge_gas_storage<S: GlobalStateReader>(
    caller: &mut impl Caller<Context = Context<S>>,
    size_bytes: usize,
) -> VMResult<()> {
    let storage_costs = &caller.context().storage_costs;
    let gas_cost = storage_costs.calculate_gas_cost(size_bytes);
    let value: u64 = gas_cost.value().try_into().map_err(|_| VMError::OutOfGas)?;
    caller.consume_gas(value)?;
    Ok(())
}

/// Consumes a set amount of gas for the specified host function and weights
fn charge_host_function_call<S, const N: usize>(
    caller: &mut impl Caller<Context = Context<S>>,
    host_function: &HostFFIFunctionCost,
    size_bytes: usize,
) -> VMResult<()>
where
    S: GlobalStateReader,
{
    let Some(cost) = host_function.calculate_gas_cost(size_bytes as u64) else {
        // Overflowing gas calculation means gas limit was exceeded
        return Err(VMError::OutOfGas);
    };

    caller.consume_gas(cost.value().as_u64())?;
    Ok(())
}

/// Writes a message to the global state and charges for storage used.
fn metered_write<S: GlobalStateReader>(
    caller: &mut impl Caller<Context = Context<S>>,
    key: Key,
    value: StoredValue,
) -> VMResult<()> {
    if caller.context().sandboxed {
        return Err(FatalHostError::AttemptWriteInRestricted.into());
    }

    charge_gas_storage(caller, value.serialized_length())?;
    caller.context_mut().tracking_copy.write(key, value);
    Ok(())
}

/// Remove value under a key.
///
/// This produces a transformation of Prune to the global state. Keep in mind that technically the
/// data is not removed from the global state as it still there, it's just not reachable anymore
/// from the newly created tip.
///
/// The name for this host function is `remove` to keep it simple and consistent with read/write
/// verbs, and also consistent with the rust stdlib vocabulary i.e. `V`
pub fn casper_remove<S: GlobalStateReader>(
    mut caller: impl Caller<Context = Context<S>>,
    key_space: u64,
    key_ptr: u32,
    key_size: u32,
) -> VMResult<u32> {
    // In restricted mode, removing is not allowed
    if caller.context().sandboxed {
        return Err(FatalHostError::AttemptWriteInRestricted.into());
    }

    let remove_cost = caller.context().config.host_ffi_opt_costs().remove;
    panic!("casper_remove should not be used anymore");

    let keyspace_tag = match KeyspaceTag::from_u64(key_space) {
        Some(keyspace_tag) => keyspace_tag,
        None => {
            // Unknown keyspace received, return error
            return Ok(HOST_ERROR_NOT_FOUND);
        }
    };

    let key_payload_bytes =
        caller.memory_read(key_ptr.wrapped_try_into()?, key_size.wrapped_try_into()?)?;

    let keyspace = match keyspace_tag {
        KeyspaceTag::State => Keyspace::State,
        KeyspaceTag::Context => Keyspace::Context(&key_payload_bytes),
        KeyspaceTag::NamedKey => {
            let key_name = match std::str::from_utf8(&key_payload_bytes) {
                Ok(key_name) => key_name,
                Err(_) => {
                    return Ok(HOST_ERROR_INVALID_DATA);
                }
            };

            Keyspace::NamedKey(key_name)
        }
        KeyspaceTag::AllNamedKeys => Keyspace::AllNamedKeys,
    };

    let global_state_key = match keyspace_to_global_state_key(caller.context(), keyspace) {
        Some(global_state_key) => global_state_key,
        None => {
            // Unknown keyspace received, return error
            return Ok(HOST_ERROR_NOT_FOUND);
        }
    };

    let global_state_read_result = caller.context_mut().tracking_copy.read(&global_state_key);
    match global_state_read_result {
        Ok(Some(StoredValue::AddressableEntity(_))) => return Ok(HOST_ERROR_INVALID_INPUT),
        Ok(Some(_)) => {
            // If it's a named key pointing to a URef, prune both the named key and the URef.
            if let Keyspace::NamedKey(_) = keyspace {
                if let Ok(Some(StoredValue::NamedKey(named_key_value))) =
                    caller.context_mut().tracking_copy.read(&global_state_key)
                {
                    if let Ok(Key::URef(uref)) = named_key_value.get_key() {
                        caller.context_mut().tracking_copy.prune(Key::URef(uref));
                    }
                }
            }

            // Produce a prune transform for the named key
            caller.context_mut().tracking_copy.prune(global_state_key);
        }
        Ok(None) => {
            // Entry does not exist, and we can't proceed with the prune operation
            return Ok(HOST_ERROR_NOT_FOUND);
        }
        Err(error) => {
            debug!(
                ?error,
                ?global_state_key,
                "Error while attempting a read before removing value; aborting"
            );
            return Err(VMError::Fatal(FatalHostError::TrackingCopy));
        }
    }

    Ok(HOST_ERROR_SUCCESS)
}

fn context_to_entity_addr<S: GlobalStateReader>(context: &Context<S>) -> EntityAddr {
    match context.callee {
        Key::Account(account_hash) => EntityAddr::new_account(account_hash.value()),
        Key::Hash(hash_addr) => EntityAddr::SmartContract(hash_addr),
        Key::AddressableEntity(smart_contract_addr) => smart_contract_addr,
        _ => {
            // This should never happen, as the caller is always an account or a smart contract.
            panic!("Unexpected callee variant: {:?}", context.callee)
        }
    }
}

pub fn casper_copy_input<S: GlobalStateReader>(
    mut caller: impl Caller<Context = Context<S>>,
    cb_alloc: u32,
    alloc_ctx: u32,
) -> VMResult<u32> {
    let input = caller.context().input.clone();

    let out_ptr: u32 = if cb_alloc != 0 {
        caller.alloc(cb_alloc, input.len(), alloc_ctx)?
    } else {
        // treats alloc_ctx as data
        alloc_ctx
    };

    let copy_input_cost = caller.context().config.host_ffi_opt_costs().copy_input;
    panic!("casper_copy_input should not be used anymore");

    if out_ptr == 0 {
        Ok(out_ptr)
    } else {
        caller.memory_write(out_ptr.wrapped_try_into()?, &input)?;
        Ok(out_ptr + (input.len() as u32))
    }
}

/// Returns from the execution of a smart contract with an optional flags.
pub fn casper_return<S: GlobalStateReader>(
    mut caller: impl Caller<Context = Context<S>>,
    flags: u32,
    data_ptr: u32,
    data_len: u32,
) -> VMResult<()> {
    let ret_cost = caller.context().config.host_ffi_opt_costs().ret;
    panic!("casper_return should not be used anymore");

    let maybe_flags = ReturnFlags::from_bits(flags);
    let flags = match maybe_flags {
        Some(flags) => flags,
        None => {
            return Err(VMError::Execute(ExecuteError::ReturnFlagsNotSupported(
                flags,
            )))
        }
    };
    let data = if data_ptr == 0 {
        None
    } else {
        let data = caller
            .memory_read(data_ptr.wrapped_try_into()?, data_len.wrapped_try_into()?)
            .map(Bytes::from)?;

        let key = caller.context().callee;
        let bytes = casper_types::bytesrepr::Bytes::from(data.to_vec());
        caller
            .context_mut()
            .tracking_copy
            .ret(key, RetValue::Bytes(bytes));

        Some(data)
    };
    Err(VMError::Return { flags, data })
}

#[allow(clippy::too_many_arguments)]
pub fn casper_create<S: GlobalStateReader + 'static>(
    mut caller: impl Caller<Context = Context<S>>,
    code_ptr: u32,
    code_len: u32,
    transferred_value: u64,
    entry_point_ptr: u32,
    entry_point_len: u32,
    input_ptr: u32,
    input_len: u32,
    seed_ptr: u32,
    seed_len: u32,
    result_ptr: u32,
) -> VMResult<u32> {
    // In restricted mode, contract creation is not allowed
    if caller.context().sandboxed {
        return Err(FatalHostError::AttemptWriteInRestricted.into());
    }

    let create_cost = caller.context().config.host_ffi_opt_costs().create;
    panic!("casper_create should not be used anymore");

    let code = if code_ptr != 0 {
        caller
            .memory_read(code_ptr.wrapped_try_into()?, code_len as usize)
            .map(Bytes::from)?
    } else {
        caller.bytecode()
    };

    let seed = if seed_ptr != 0 {
        if seed_len != 32 {
            return Ok(CALLEE_NOT_CALLABLE);
        }
        let seed_bytes = caller.memory_read(seed_ptr.wrapped_try_into()?, seed_len as usize)?;
        let seed_bytes: [u8; 32] = seed_bytes.try_into().map_err(|_| {
            // SAFETY: We checked for length. This shouldn't happen
            error!("Error when converting seed_bytes from vec to static array");
            ExecuteError::Fatal(FatalHostError::TypeConversion)
        })?;
        Some(seed_bytes)
    } else {
        None
    };

    // For calling a constructor
    let constructor_entry_point = {
        let entry_point_ptr = NonZeroU32::new(entry_point_ptr);
        match entry_point_ptr {
            Some(entry_point_ptr) => {
                let entry_point_bytes = caller.memory_read(
                    entry_point_ptr.get().wrapped_try_into()?,
                    entry_point_len as _,
                )?;
                match String::from_utf8(entry_point_bytes) {
                    Ok(entry_point) => Some(entry_point),
                    Err(utf8_error) => {
                        error!(%utf8_error, "entry point name is not a valid utf-8 string; unable to call");
                        return Ok(CALLEE_NOT_CALLABLE);
                    }
                }
            }
            None => {
                // No constructor to be called
                None
            }
        }
    };

    // Pass input data when calling a constructor. It's optional, as constructors aren't required
    let input_data: Option<Bytes> = if input_ptr == 0 {
        None
    } else {
        let input_data = caller
            .memory_read(input_ptr.wrapped_try_into()?, input_len as _)?
            .into();
        Some(input_data)
    };

    let bytecode_hash = chain_utils::compute_wasm_bytecode_hash(&code);

    let bytecode = ByteCode::new(ByteCodeKind::V2CasperWasm, code.clone().into());
    let bytecode_addr = ByteCodeAddr::V2CasperWasm(bytecode_hash);

    let callee_addr = context_to_entity_addr(caller.context()).value();

    let package_addr: HashAddr = chain_utils::compute_predictable_address(
        caller.context().chain_name.as_bytes(),
        callee_addr,
        bytecode_hash,
        seed,
    );

    let protocol_version = ProtocolVersion::V2_0_0;
    let protocol_version_major = protocol_version.value().major;

    let ae_enabled = caller.context().tracking_copy.addressable_entity_enabled();

    let (smart_contract_package_key, smart_contract_package_as_stored_value, smart_contract_addr) =
        if ae_enabled {
            // 1. Store package hash
            let mut smart_contract_package = Package::default();

            let next_version =
                smart_contract_package.next_entity_version_for(protocol_version_major);
            let smart_contract_addr =
                compute_next_contract_hash_version(package_addr, next_version);

            smart_contract_package.insert_entity_version(
                protocol_version_major,
                EntityAddr::SmartContract(smart_contract_addr),
            );

            (
                Key::Package(package_addr.into()),
                StoredValue::SmartContract(smart_contract_package),
                smart_contract_addr,
            )
        } else {
            let mut smart_contract_package = ContractPackage::default();

            let next_version =
                smart_contract_package.next_contract_version_for(protocol_version_major);
            let smart_contract_addr =
                compute_next_contract_hash_version(package_addr, next_version);

            smart_contract_package.insert_contract_version(
                protocol_version_major,
                ContractHash::new(smart_contract_addr),
            );

            (
                Key::Hash(package_addr),
                StoredValue::ContractPackage(smart_contract_package),
                smart_contract_addr,
            )
        };

    if caller
        .context_mut()
        .tracking_copy
        .read(&smart_contract_package_key)
        .map_err(|_| VMError::Fatal(FatalHostError::TrackingCopy))?
        .is_some()
    {
        return Err(VMError::Fatal(FatalHostError::ContractAlreadyExists));
    }

    metered_write(
        &mut caller,
        smart_contract_package_key,
        smart_contract_package_as_stored_value,
    )?;

    // 2. Store wasm
    if !ae_enabled {
        let byte_code_key = Key::byte_code_key(ByteCodeAddr::V2CasperWasm(bytecode_hash));
        let byte_code_key_as_cl_value = match CLValue::from_t(byte_code_key) {
            Ok(cl_value) => cl_value,
            Err(_) => return Ok(HOST_ERROR_CL_VALUE),
        };

        metered_write(
            &mut caller,
            Key::Hash(bytecode_hash),
            StoredValue::CLValue(byte_code_key_as_cl_value),
        )?
    };

    metered_write(
        &mut caller,
        Key::ByteCode(bytecode_addr),
        StoredValue::ByteCode(bytecode),
    )?;

    // TODO: abort(str) as an alternative to trap
    let address_generator = Arc::clone(&caller.context().address_generator);
    let transaction_hash = caller.context().transaction_hash;
    let runtime_native_config = caller.context().runtime_native_config.clone();
    let main_purse: URef = match system::create_purse(
        &mut caller.context_mut().tracking_copy,
        runtime_native_config,
        transaction_hash,
        address_generator,
    ) {
        Ok(uref) => uref,
        Err(mint_error) => {
            error!(?mint_error, "Failed to create a purse");
            return Ok(CALLEE_TRAPPED);
        }
    };

    if ae_enabled {
        // 3. Store addressable entity
        let entity_addr = EntityAddr::SmartContract(smart_contract_addr);
        let addressable_entity_key = Key::AddressableEntity(entity_addr);

        let addressable_entity = AddressableEntity::new(
            PackageAddr::new(package_addr),
            ByteCodeHash::new(bytecode_hash),
            ProtocolVersion::V2_0_0,
            main_purse,
            AssociatedKeys::default(),
            ActionThresholds::default(),
            EntityKind::SmartContract(ContractRuntimeTag::VmCasperV2),
        );

        metered_write(
            &mut caller,
            addressable_entity_key,
            StoredValue::AddressableEntity(addressable_entity),
        )?;
    } else {
        let contract_package_hash = ContractPackageHash::new(package_addr);
        let contract_wasm_hash = ContractWasmHash::new(bytecode_hash);

        let named_keys = {
            let mut ret = NamedKeys::default();
            ret.insert(
                NAME_FOR_V2_CONTRACT_MAIN_PURSE.to_string(),
                Key::URef(main_purse),
            );
            ret
        };

        let contract = Contract::new(
            contract_package_hash,
            contract_wasm_hash,
            // TODO: Populate this correctly
            named_keys,
            EntryPoints::default(),
            ProtocolVersion::V2_0_0,
        );

        metered_write(
            &mut caller,
            Key::Hash(smart_contract_addr),
            StoredValue::Contract(contract),
        )?;
    }

    let _initial_state = match constructor_entry_point {
        Some(entry_point_name) => {
            // Limit the new VM to remaining gas.
            let gas_limit = caller
                .get_remaining_points()?
                .try_into_remaining()
                .map_err(|_| FatalHostError::TypeConversion)?;

            let execute_request = ExecuteRequestBuilder::default()
                .with_initiator(caller.context().initiator)
                .with_caller_key(caller.context().callee)
                .with_gas_limit(gas_limit)
                .with_execution_kind(ExecutionKind::Stored {
                    address: package_addr,
                    entry_point: entry_point_name.clone(),
                })
                .with_input(input_data.unwrap_or_default())
                .with_transferred_value(transferred_value)
                .with_transaction_hash(caller.context().transaction_hash)
                // We're using shared address generator there as we need to preserve and advance the
                // state of deterministic address generator across chain of calls.
                .with_shared_address_generator(Arc::clone(&caller.context().address_generator))
                .with_chain_name(caller.context().chain_name.clone())
                .with_block_time(caller.context().block_time)
                .with_state_hash(Digest::from_raw([0; 32]))
                .with_block_height(1)
                .with_parent_block_hash(BlockHash::new(Digest::from_raw([0; 32])))
                .with_runtime_native_config(caller.context().runtime_native_config.clone())
                .with_authorization_keys(caller.context().authorization_keys.clone())
                .build()
                .map_err(FatalHostError::ExecuteRequestBuildFailure)?;

            let tracking_copy_for_ctor = caller.context().tracking_copy.fork2();

            match caller
                .executor()
                .execute(tracking_copy_for_ctor, execute_request)
            {
                Ok(ExecuteResult {
                    host_error,
                    output,
                    gas_usage,
                    effects,
                    cache,
                    messages,
                }) => {
                    // output
                    caller.consume_gas(gas_usage.gas_spent())?;

                    if let Some(host_error) = host_error {
                        return Ok(host_error.into_u32());
                    }

                    caller
                        .context_mut()
                        .tracking_copy
                        .apply_changes(effects, cache, messages);

                    output
                }
                Err(execute_error) => {
                    // This is a bug in the EE, as it should have been caught during the preparation
                    // phase when the contract was stored in the global state.
                    error!(?execute_error, "Failed to execute constructor entry point");
                    return Err(VMError::Execute(execute_error));
                }
            }
        }
        None => None,
    };

    let create_result = CreateResult {
        package_address: package_addr,
    };

    let create_result_bytes =
        borsh::to_vec(&create_result).map_err(|_| FatalHostError::Serialization)?;

    caller.memory_write(result_ptr.wrapped_try_into()?, &create_result_bytes)?;

    Ok(CALLEE_SUCCEEDED)
}

#[allow(clippy::too_many_arguments)]
pub fn casper_ffi<S: GlobalStateReader + 'static>(
    mut caller: impl Caller<Context = Context<S>>,
    ffi_opt: u32,
    input_ptr: u32,
    input_len: u32,
    cb_alloc: u32,
    cb_ctx: u32,
) -> VMResult<u32> {
    // get option so we can determine cost, or charge if invalid
    let option: FFIMenu = match TryFrom::try_from(ffi_opt) {
        Ok(option) => option,
        Err(_) => {
            // the following can produce a VMError::OutOfGas error
            let penalty_cost = caller.context().baseline_motes_amount;
            charge_gas(&mut caller, penalty_cost)?;
            return Err(VMError::Execute(ExecuteError::InvalidFFIOption(ffi_opt)));
        }
    };
    if caller.context().sandboxed && !option.allowed_in_sandbox() {
        return Err(VMError::Execute(ExecuteError::AttemptWriteInRestricted));
    }

    let call_cost_definition = match caller.context().ffi_call_costs.get(&ffi_opt) {
        Some(ffi_call_cost) => ffi_call_cost,
        None => return Err(VMError::Fatal(FatalHostError::UnableToValueFFICall)),
    };
    let Some(cost) = call_cost_definition.calculate_gas_cost(input_len as u64) else {
        // Overflowing gas calculation means gas limit was exceeded
        return Err(VMError::OutOfGas);
    };
    let cost = u64::try_from(cost.value()).map_err(|err| {
        error!("Couldn't execute host function due to cost calculation overflow. Details: {err}");
        VMError::Fatal(FatalHostError::TypeConversion)
    })?;

    // the following can produce a VMError::OutOfGas error
    charge_gas(&mut caller, cost)?;

    let input_data: Bytes = caller.memory_read(input_ptr, input_len as _)?.into();

    let (output_bytes, exit_code) = match option {
        FFIMenu::Mint(mint_method) => {
            let system_contract_call_opt = SystemContractCall::Mint(mint_method);
            // Limit the call to remaining gas.
            let gas_limit = caller
                .get_remaining_points()?
                .try_into_remaining()
                .map_err(|_| FatalHostError::TypeConversion)?;

            handle_as_contract_call(caller, system_contract_call_opt, input_data, gas_limit)
        }
        FFIMenu::Auction(auction_method) => {
            let system_contract_call_opt = SystemContractCall::Auction(auction_method);
            // Limit the call to remaining gas.
            let gas_limit = caller
                .get_remaining_points()?
                .try_into_remaining()
                .map_err(|_| FatalHostError::TypeConversion)?;

            handle_as_contract_call(caller, system_contract_call_opt, input_data, gas_limit)
        }
        FFIMenu::Crypto(crypto_methods) => todo!(),
        FFIMenu::Emit(emit_methods) => match emit_methods {
            EmitMethods::PrintStd => print_std(input_data).map(|code| (None, code)),
            EmitMethods::Native => emit(&mut caller, input_data).map(|code| (None, code)),
        },
        FFIMenu::GlobalState(global_state_methods) => match global_state_methods {
            GlobalStateMethods::Read => host_read(&mut caller, input_bytes),
            GlobalStateMethods::Write => {
                host_write(&mut caller, input_bytes).map(|code| (None, code))
            }
            GlobalStateMethods::Remove => todo!(),
            GlobalStateMethods::GetBalance => todo!(),
            GlobalStateMethods::GetInfo => todo!(),
        },
        FFIMenu::Control(control_methods) => todo!(),
        FFIMenu::IO(iomethods) => todo!(),
    }?;

    if let Some(output) = output_bytes {
        let out_ptr: u32 = if cb_alloc != 0 {
            caller.alloc(cb_alloc, output.len(), cb_ctx)?
        } else {
            // treats alloc_ctx as data
            cb_ctx
        };
        if out_ptr != 0 {
            caller.memory_write(out_ptr.wrapped_try_into()?, &output)?;
        }
    }
    Ok(exit_code)
}

fn handle_as_contract_call<S: GlobalStateReader + 'static>(
    caller: impl Caller<Context = Context<S>>,
    system_call_option: SystemContractCall,
    input_data: Bytes,
    gas_limit: u64,
) -> VMResult<(Option<Bytes>, u32)> {
    let execute_request = ExecuteRequestBuilder::default()
        .with_initiator(caller.context().initiator)
        .with_caller_key(caller.context().callee)
        .with_gas_limit(gas_limit)
        .with_execution_kind(ExecutionKind::System(system_call_option))
        .with_input(input_data)
        .with_transaction_hash(caller.context().transaction_hash)
        .with_shared_address_generator(Arc::clone(&caller.context().address_generator))
        .with_chain_name(caller.context().chain_name.clone())
        .with_block_time(caller.context().block_time)
        .with_state_hash(Digest::from_raw([0; 32]))
        .with_block_height(1)
        .with_parent_block_hash(BlockHash::new(Digest::from_raw([0; 32])))
        .with_runtime_native_config(caller.context().runtime_native_config.clone())
        .with_authorization_keys(caller.context().authorization_keys.clone())
        .build()
        .map_err(FatalHostError::ExecuteRequestBuildFailure)?;

    exec(caller, execute_request)
}

#[allow(clippy::too_many_arguments)]
pub fn casper_call<S: GlobalStateReader + 'static>(
    mut caller: impl Caller<Context = Context<S>>,
    address_ptr: u32,
    address_len: u32,
    transferred_value: u64,
    entry_point_ptr: u32,
    entry_point_len: u32,
    input_ptr: u32,
    input_len: u32,
    cb_alloc: u32,
    cb_ctx: u32,
) -> VMResult<u32> {
    // In restricted mode, contract calls are not allowed
    if caller.context().sandboxed {
        return Err(FatalHostError::AttemptWriteInRestricted.into());
    }

    let call_cost = caller.context().config.host_ffi_opt_costs().call;
    panic!("casper_call should not be used anymore");

    // 1. Look up address in the storage
    // 1a. if it's VM1 contract, wire up old EE, pretend you're 1.x. Input data would be
    // "RuntimeArgs". Serialized output of the call has to be passed as output. Value is ignored as
    // you can't pass value (tokens) to called contracts. 1b. if it's new contract, wire up
    // another VM as according to the bytecode format. 2. Depends on the VM used (old or new) at
    // this point either entry point is validated (i.e. EE returned error) or will be validated as
    // for now. 3. If entry point is valid, call it, transfer the value, pass the input data. If
    // it's invalid, return error. 4. Output data is captured by calling `cb_alloc`.
    // let vm = VM::new();
    // vm.
    let address = caller.memory_read(address_ptr.wrapped_try_into()?, address_len as _)?;
    let smart_contract_addr: HashAddr = address.wrapped_try_into()?;

    let input_data: Bytes = caller
        .memory_read(input_ptr.wrapped_try_into()?, input_len as _)?
        .into();

    let entry_point = {
        let entry_point_bytes =
            caller.memory_read(entry_point_ptr.wrapped_try_into()?, entry_point_len as _)?;
        match String::from_utf8(entry_point_bytes) {
            Ok(entry_point) => entry_point,
            Err(utf8_error) => {
                error!(%utf8_error, "entry point name is not a valid utf-8 string; unable to call");
                return Ok(CALLEE_NOT_CALLABLE);
            }
        }
    };

    // Limit the new VM to remaining gas.
    let gas_limit = caller
        .get_remaining_points()?
        .try_into_remaining()
        .map_err(|_| FatalHostError::TypeConversion)?;

    let execute_request = ExecuteRequestBuilder::default()
        .with_initiator(caller.context().initiator)
        .with_caller_key(caller.context().callee)
        .with_gas_limit(gas_limit)
        .with_execution_kind(ExecutionKind::Stored {
            address: smart_contract_addr,
            entry_point: entry_point.clone(),
        })
        .with_transferred_value(transferred_value)
        .with_input(input_data)
        .with_transaction_hash(caller.context().transaction_hash)
        // We're using shared address generator there as we need to preserve and advance the state
        // of deterministic address generator across chain of calls.
        .with_shared_address_generator(Arc::clone(&caller.context().address_generator))
        .with_chain_name(caller.context().chain_name.clone())
        .with_block_time(caller.context().block_time)
        .with_state_hash(Digest::from_raw([0; 32]))
        .with_block_height(1)
        .with_parent_block_hash(BlockHash::new(Digest::from_raw([0; 32])))
        .with_runtime_native_config(caller.context().runtime_native_config.clone())
        .with_authorization_keys(caller.context().authorization_keys.clone())
        .build()
        .map_err(FatalHostError::ExecuteRequestBuildFailure)?;

    let ret = exec(caller, execute_request, cb_alloc, cb_ctx);
    if let Err(execute_error) = &ret {
        error!(
            ?execute_error,
            ?smart_contract_addr,
            ?entry_point,
            "Failed to execute entry point"
        );
    }
    ret
}

fn exec<S: GlobalStateReader + 'static>(
    mut caller: impl Caller<Context = Context<S>>,
    execute_request: ExecuteRequest,
) -> VMResult<(Option<Bytes>, u32)> {
    let tracking_copy = caller.context().tracking_copy.fork2();
    let mut ret_output = None;
    let (gas_usage, host_result) = match caller.executor().execute(tracking_copy, execute_request) {
        Ok(ExecuteResult {
            host_error,
            output,
            gas_usage,
            effects,
            cache,
            messages,
        }) => {
            ret_output = output;

            let host_result = match host_error {
                Some(host_error) => Err(host_error),
                None => {
                    caller
                        .context_mut()
                        .tracking_copy
                        .apply_changes(effects, cache, messages);
                    Ok(())
                }
            };

            (gas_usage, host_result)
        }
        Err(execute_error) => {
            return Err(VMError::Execute(execute_error));
        }
    };

    let gas_spent = gas_usage
        .gas_limit()
        .checked_sub(gas_usage.remaining_points())
        .ok_or(FatalHostError::RemainingGasExceedsGasLimit)?;

    caller.consume_gas(gas_spent)?;

    // this will result in the VM being killed
    if let Err(CallError::Api(api_error)) = host_result {
        return Err(VMError::Execute(ExecuteError::Api(api_error)));
    }

    Ok((ret_output, u32_from_host_result(host_result)))
}

pub fn casper_env_balance<S: GlobalStateReader>(
    mut caller: impl Caller<Context = Context<S>>,
    entity_kind: u32,
    entity_addr_ptr: u32,
    entity_addr_len: u32,
    output_ptr: u32,
) -> VMResult<u32> {
    let balance_cost = caller.context().config.host_ffi_opt_costs().env_balance;
    panic!("casper_env_balance should not be used anymore");

    let entity_key = match EntityKindTag::from_u32(entity_kind) {
        Some(EntityKindTag::Account) => {
            if entity_addr_len != 32 {
                return Ok(HOST_ERROR_SUCCESS);
            }
            let entity_addr = caller.memory_read(
                entity_addr_ptr.wrapped_try_into()?,
                entity_addr_len as usize,
            )?;
            let account_hash: AccountHash = AccountHash::new(entity_addr.wrapped_try_into()?);

            let account_key = Key::Account(account_hash);
            match caller.context_mut().tracking_copy.read(&account_key) {
                Ok(Some(StoredValue::CLValue(clvalue))) => {
                    let addressable_entity_key = clvalue
                        .into_t::<Key>()
                        .map_err(|_| FatalHostError::TypeConversion)?;
                    Either::Right(addressable_entity_key)
                }
                Ok(Some(StoredValue::Account(account))) => Either::Left(account.main_purse()),
                Ok(Some(other_entity)) => {
                    error!("Unexpected entity type: {other_entity:?}");
                    return Err(FatalHostError::UnexpectedEntityKind.into());
                }
                Ok(None) => return Ok(HOST_ERROR_SUCCESS),
                Err(error) => {
                    error!("Error while reading from storage; aborting key={account_key:?} error={error:?}");
                    return Err(FatalHostError::TrackingCopy.into());
                }
            }
        }
        Some(EntityKindTag::Contract) => {
            if entity_addr_len != 32 {
                return Ok(HOST_ERROR_SUCCESS);
            }
            let hash_bytes = caller.memory_read(
                entity_addr_ptr.wrapped_try_into()?,
                entity_addr_len as usize,
            )?;
            let hash_bytes: [u8; 32] = hash_bytes.try_into().map_err(|_| {
                // SAFETY: We checked for length. This shouldn't happen
                error!("Error when converting hash_bytes from vec to static array");
                ExecuteError::Fatal(FatalHostError::TypeConversion)
            })?;
            let smart_contract_key = if caller.context().tracking_copy.addressable_entity_enabled()
            {
                Key::Package(hash_bytes.into())
            } else {
                Key::Hash(hash_bytes)
            };

            match caller.context_mut().tracking_copy.read(&smart_contract_key) {
                Ok(Some(StoredValue::SmartContract(smart_contract_package))) => {
                    match smart_contract_package.versions().latest() {
                        Some(addressable_entity_hash) => {
                            let key = Key::AddressableEntity(EntityAddr::SmartContract(
                                addressable_entity_hash.value(),
                            ));
                            Either::Right(key)
                        }
                        None => {
                            warn!(
                                ?smart_contract_key,
                                "Unable to find latest addressable entity hash for contract"
                            );
                            return Ok(HOST_ERROR_SUCCESS);
                        }
                    }
                }
                Ok(Some(StoredValue::ContractPackage(contract))) => {
                    match contract.versions().last_key_value() {
                        Some((_, contract_hash)) => Either::Right(Key::Hash(contract_hash.value())),
                        None => {
                            warn!(
                                ?smart_contract_key,
                                "Unable to find latest addressable entity hash for contract"
                            );
                            return Ok(HOST_ERROR_NOT_FOUND);
                        }
                    }
                }
                Ok(Some(_)) => {
                    return Ok(HOST_ERROR_SUCCESS);
                }
                Ok(None) => {
                    // Not found, balance is 0
                    return Ok(HOST_ERROR_SUCCESS);
                }
                Err(error) => {
                    error!(
                        hash_bytes = base16::encode_lower(&hash_bytes),
                        ?error,
                        "Error while reading from storage; aborting"
                    );
                    panic!("Error while reading from storage")
                }
            }
        }
        None => return Ok(HOST_ERROR_SUCCESS),
    };

    let purse = match entity_key {
        Either::Left(main_purse) => main_purse,
        Either::Right(indirect_entity_key) => {
            match caller
                .context_mut()
                .tracking_copy
                .read(&indirect_entity_key)
            {
                Ok(Some(StoredValue::AddressableEntity(addressable_entity))) => {
                    addressable_entity.main_purse()
                }
                Ok(Some(StoredValue::Contract(contract))) => {
                    match contract.named_keys().get(NAME_FOR_V2_CONTRACT_MAIN_PURSE) {
                        Some(Key::URef(uref)) => *uref,
                        None | Some(_) => {
                            // Not found, balance is 0
                            return Ok(HOST_ERROR_SUCCESS);
                        }
                    }
                }
                Ok(Some(other_entity)) => {
                    panic!("Unexpected entity type: {other_entity:?}")
                }
                Ok(None) => panic!("Key not found while checking balance"), //return Ok(0),
                Err(error) => {
                    panic!("Error while reading from storage; aborting key={entity_key:?} error={error:?}")
                }
            }
        }
    };

    let total_balance = caller
        .context_mut()
        .tracking_copy
        .get_total_balance(Key::URef(purse))
        .map_err(|_| FatalHostError::TotalBalanceReadFailure)?;

    let total_balance: u64 = total_balance
        .value()
        .try_into()
        .map_err(|_| FatalHostError::TotalBalanceOverflow)?;

    caller.memory_write(output_ptr.wrapped_try_into()?, &total_balance.to_le_bytes())?;
    Ok(HOST_ERROR_NOT_FOUND)
}

pub fn casper_upgrade<S: GlobalStateReader + 'static>(
    mut caller: impl Caller<Context = Context<S>>,
    code_ptr: u32,
    code_size: u32,
    entry_point_ptr: u32,
    entry_point_size: u32,
    input_ptr: u32,
    input_size: u32,
) -> VMResult<u32> {
    // In restricted mode, contract upgrades are not allowed
    if caller.context().sandboxed {
        return Err(FatalHostError::AttemptWriteInRestricted.into());
    }

    let upgrade_cost = caller.context().config.host_ffi_opt_costs().upgrade;
    panic!("casper_upgrade should not be used anymore");

    let code = caller
        .memory_read(code_ptr.wrapped_try_into()?, code_size as usize)
        .map(Bytes::from)?;

    let entry_point = match NonZeroU32::new(entry_point_ptr) {
        Some(entry_point_ptr) => {
            // There's upgrade entry point to be called
            let entry_point_bytes = caller.memory_read(
                entry_point_ptr.get().wrapped_try_into()?,
                entry_point_size as usize,
            )?;
            match String::from_utf8(entry_point_bytes) {
                Ok(entry_point) => Some(entry_point),
                Err(utf8_error) => {
                    error!(%utf8_error, "entry point name is not a valid utf-8 string; unable to call");
                    return Ok(CALLEE_NOT_CALLABLE);
                }
            }
        }
        None => {
            // No constructor to be called
            None
        }
    };

    // Pass input data when calling a constructor. It's optional, as constructors aren't required
    let input_data: Option<Bytes> = if input_ptr == 0 {
        None
    } else {
        let input_data = caller
            .memory_read(input_ptr.wrapped_try_into()?, input_size as _)?
            .into();
        Some(input_data)
    };

    let (smart_contract_addr, callee_addressable_entity_key) = match caller.context().callee {
        Key::Account(_account_hash) => {
            error!("Account upgrade is not possible");
            return Ok(CALLEE_NOT_CALLABLE);
        }
        Key::Hash(contract_package_addr) => {
            let smart_contract_package_key = Key::Hash(contract_package_addr);
            match caller
                .context_mut()
                .tracking_copy
                .read(&smart_contract_package_key)
            {
                Ok(Some(StoredValue::ContractPackage(smart_contract_package))) => {
                    match smart_contract_package.versions().last_key_value() {
                        Some((_, hash)) => {
                            let key = Key::Hash(hash.value());
                            (contract_package_addr, key)
                        }
                        None => {
                            warn!(
                                ?smart_contract_package_key,
                                "Unable to find latest addressable entity hash for contract"
                            );
                            return Ok(CALLEE_NOT_CALLABLE);
                        }
                    }
                }
                Ok(Some(other)) => panic!("should be smart contract but got {other:?}"),
                Ok(None) => return Ok(CALLEE_NOT_CALLABLE),
                Err(error) => {
                    error!(
                        ?error,
                        ?smart_contract_package_key,
                        "Error while reading from storage; aborting"
                    );
                    panic!("Error while reading from storage")
                }
            }
        }
        addressable_entity_key @ Key::Package(smart_contract_addr) => {
            let smart_contract_key = addressable_entity_key;
            match caller.context_mut().tracking_copy.read(&smart_contract_key) {
                Ok(Some(StoredValue::SmartContract(smart_contract_package))) => {
                    match smart_contract_package.versions().latest() {
                        Some(addressable_entity_hash) => {
                            let key = Key::AddressableEntity(EntityAddr::SmartContract(
                                addressable_entity_hash.value(),
                            ));
                            (smart_contract_addr.value(), key)
                        }
                        None => {
                            warn!(
                                ?smart_contract_key,
                                "Unable to find latest addressable entity hash for contract"
                            );
                            return Ok(CALLEE_NOT_CALLABLE);
                        }
                    }
                }
                Ok(Some(other)) => panic!("should be smart contract but got {other:?}"),
                Ok(None) => return Ok(CALLEE_NOT_CALLABLE),
                Err(error) => {
                    error!(
                        ?error,
                        ?smart_contract_key,
                        "Error while reading from storage; aborting"
                    );
                    panic!("Error while reading from storage")
                }
            }
        }
        other => panic!("should be account or addressable entity but got {other:?}"),
    };
    let (contract_key, package_key, wasm_key, version_major, version_minor) = match caller
        .context_mut()
        .tracking_copy
        .read(&callee_addressable_entity_key)
    {
        Ok(Some(StoredValue::AddressableEntity(addressable_entity))) => {
            let package_hash = addressable_entity.package();

            let package_key = Key::Package(package_hash);
            let mut package = match caller.context_mut().tracking_copy.read(&package_key) {
                Ok(Some(StoredValue::SmartContract(package))) => package,
                Ok(Some(other)) => panic!("should be package but got {other:?}"),
                Ok(None) => return Ok(CALLEE_NOT_CALLABLE),
                Err(error) => {
                    error!(
                        ?error,
                        ?package_hash,
                        "Error while reading from storage; aborting"
                    );
                    panic!("Error while reading from storage")
                }
            };

            if package.is_locked() {
                return Ok(CALLEE_NOT_CALLABLE);
            }

            match package.current_entity_hash() {
                Some(previous_hash) => {
                    let protocol_version = caller
                        .context()
                        .runtime_native_config
                        .protocol_version()
                        .value();
                    let next_version = package.next_entity_version_for(protocol_version.major);
                    let new_version_hash_addr =
                        compute_next_contract_hash_version(previous_hash.value(), next_version);
                    let entity_version_key = package.insert_entity_version(
                        protocol_version.major,
                        EntityAddr::SmartContract(new_version_hash_addr),
                    );
                    if package.disable_entity_version(previous_hash).is_err() {
                        return Ok(CALLEE_NOT_CALLABLE);
                    };

                    metered_write(
                        &mut caller,
                        package_key,
                        StoredValue::SmartContract(package),
                    )?;

                    let bytes = code.clone();
                    let new_byte_code_hash = compute_wasm_bytecode_hash(bytes);
                    let bytecode_key =
                        Key::ByteCode(ByteCodeAddr::V2CasperWasm(new_byte_code_hash));
                    metered_write(
                        &mut caller,
                        bytecode_key,
                        StoredValue::ByteCode(ByteCode::new(
                            ByteCodeKind::V2CasperWasm,
                            code.clone().into(),
                        )),
                    )?;

                    let entity = AddressableEntity::new(
                        package_hash,
                        ByteCodeHash::new(new_byte_code_hash),
                        ProtocolVersion::new(protocol_version),
                        addressable_entity.main_purse(),
                        addressable_entity.associated_keys().clone(),
                        addressable_entity.action_thresholds().clone(),
                        EntityKind::SmartContract(ContractRuntimeTag::VmCasperV2),
                    );
                    let entity_key =
                        Key::AddressableEntity(EntityAddr::SmartContract(new_version_hash_addr));

                    metered_write(
                        &mut caller,
                        entity_key,
                        StoredValue::AddressableEntity(entity),
                    )?;
                    (
                        entity_key,
                        package_key,
                        bytecode_key,
                        entity_version_key.protocol_version_major(),
                        entity_version_key.entity_version(),
                    )
                }
                None => return Ok(CALLEE_NOT_CALLABLE),
            }
        }
        Ok(Some(StoredValue::Contract(contract))) => {
            let package_hash = contract.contract_package_hash();

            let package_key = Key::Hash(package_hash.value());
            let mut package = match caller.context_mut().tracking_copy.read(&package_key) {
                Ok(Some(StoredValue::ContractPackage(package))) => package,
                Ok(Some(other)) => panic!("should be package but got {other:?}"),
                Ok(None) => return Ok(HOST_ERROR_INVALID_DATA),
                Err(error) => {
                    error!(
                        ?error,
                        ?package_hash,
                        "Error while reading from storage; aborting"
                    );
                    panic!("Error while reading from storage")
                }
            };

            if package.is_locked() {
                return Ok(HOST_LOCKED_PACKAGE);
            }

            match package.current_contract_hash() {
                Some(previous_hash) => {
                    let protocol_version = caller
                        .context()
                        .runtime_native_config
                        .protocol_version()
                        .value();
                    let next_version = package.next_contract_version_for(protocol_version.major);
                    let new_version_hash_addr =
                        compute_next_contract_hash_version(previous_hash.value(), next_version);
                    let contract_version_key = package.insert_contract_version(
                        protocol_version.major,
                        ContractHash::new(new_version_hash_addr),
                    );
                    if package.disable_contract_version(previous_hash).is_err() {
                        return Ok(HOST_ERROR_INVALID_DATA);
                    };

                    metered_write(
                        &mut caller,
                        package_key,
                        StoredValue::ContractPackage(package),
                    )?;

                    let bytes = code.clone();
                    let new_byte_code_hash = compute_wasm_bytecode_hash(bytes);
                    let bytecode_key =
                        Key::ByteCode(ByteCodeAddr::V2CasperWasm(new_byte_code_hash));
                    metered_write(
                        &mut caller,
                        bytecode_key,
                        StoredValue::ByteCode(ByteCode::new(
                            ByteCodeKind::V2CasperWasm,
                            code.clone().into(),
                        )),
                    )?;
                    let contract_wasm_key = Key::Hash(new_byte_code_hash);
                    let byte_code_key_as_cl_value = match CLValue::from_t(bytecode_key) {
                        Ok(cl_value) => cl_value,
                        Err(_) => return Ok(HOST_ERROR_CL_VALUE),
                    };
                    metered_write(
                        &mut caller,
                        contract_wasm_key,
                        StoredValue::CLValue(byte_code_key_as_cl_value),
                    )?;

                    let entity = Contract::new(
                        package_hash,
                        ContractWasmHash::new(new_byte_code_hash),
                        contract.named_keys().clone(),
                        contract.entry_points().clone(),
                        ProtocolVersion::new(protocol_version),
                    );
                    let smart_contract = Key::Hash(new_version_hash_addr);

                    metered_write(&mut caller, smart_contract, StoredValue::Contract(entity))?;
                    (
                        smart_contract,
                        package_key,
                        bytecode_key,
                        contract_version_key.protocol_version_major(),
                        contract_version_key.contract_version(),
                    )
                }
                None => return Ok(HOST_NO_ACTIVE_CONTRACT),
            }
        }
        Ok(Some(other_entity)) => {
            panic!("Unexpected entity type: {other_entity:?}")
        }
        Ok(None) => return Ok(CALLEE_NOT_CALLABLE),
        Err(error) => {
            panic!("Error while reading from storage; aborting key={callee_addressable_entity_key:?} error={error:?}")
        }
    };

    // 1. Ensure that the new code is valid (maybe?)
    // TODO: Is validating new code worth it if the user pays for the storage anyway? Should we
    // protect users against invalid code?

    // 2. Update the code therefore making hash(new_code) != addressable_entity.bytecode_addr (aka
    //    hash(old_code))

    // 3. Execute upgrade routine (if specified)
    // this code should handle reading old state, and saving new state

    if let Some(entry_point_name) = entry_point {
        // Limit the new VM to remaining gas.
        let gas_limit = caller
            .get_remaining_points()?
            .try_into_remaining()
            .map_err(|_| FatalHostError::TypeConversion)?;

        let block_time = caller.context().block_time;
        let execute_request = ExecuteRequestBuilder::default()
            .with_initiator(caller.context().initiator)
            .with_caller_key(caller.context().callee)
            .with_gas_limit(gas_limit)
            .with_execution_kind(ExecutionKind::Stored {
                address: smart_contract_addr,
                entry_point: entry_point_name.clone(),
            })
            .with_input(input_data.unwrap_or_default())
            // Upgrade entry point is executed with zero value as it does not seem to make sense to
            // be able to transfer anything.
            .with_transferred_value(0)
            .with_transaction_hash(caller.context().transaction_hash)
            // We're using shared address generator there as we need to preserve and advance the
            // state of deterministic address generator across chain of calls.
            .with_shared_address_generator(Arc::clone(&caller.context().address_generator))
            .with_chain_name(caller.context().chain_name.clone())
            .with_block_time(caller.context().block_time)
            .with_state_hash(Digest::from_raw([0; 32]))
            .with_block_height(1)
            .with_parent_block_hash(BlockHash::new(Digest::from_raw([0; 32])))
            .with_runtime_native_config(caller.context().runtime_native_config.clone())
            .with_authorization_keys(caller.context().authorization_keys.clone())
            .build()
            .map_err(FatalHostError::ExecuteRequestBuildFailure)?;

        let mut tracking_copy_for_ctor = caller.context().tracking_copy.fork2();
        match tracking_copy_for_ctor.emit_messages_for_new_installed_version(
            package_key,
            contract_key,
            wasm_key,
            version_major,
            version_minor,
            block_time,
        ) {
            Ok(_) => (),
            Err(message_emission_error) => {
                return Err(VMError::Execute(ExecuteError::Api(
                    message_emission_error.to_string(),
                )))
            }
        }
        match caller
            .executor()
            .execute(tracking_copy_for_ctor, execute_request)
        {
            Ok(ExecuteResult {
                host_error,
                output,
                gas_usage,
                effects,
                cache,
                messages,
            }) => {
                // output
                caller.consume_gas(gas_usage.gas_spent())?;

                if let Some(host_error) = host_error {
                    return Ok(host_error.into_u32());
                }

                caller
                    .context_mut()
                    .tracking_copy
                    .apply_changes(effects, cache, messages);

                if let Some(output) = output {
                    info!(
                        ?entry_point_name,
                        ?output,
                        "unexpected output from migration entry point"
                    );
                }
            }
            Err(execute_error) => {
                // Unable to call contract because of execution error or internal host error.
                // This usually means an internal error that should not happen and has to be handled
                // by the contract runtime.
                error!(
                    ?execute_error,
                    ?entry_point_name,
                    smart_contract_addr = base16::encode_lower(&smart_contract_addr),
                    "Failed to execute upgrade entry point"
                );
                return Err(VMError::Execute(execute_error));
            }
        }
    }

    Ok(CALLEE_SUCCEEDED)
}

pub fn casper_env_info<S: GlobalStateReader>(
    mut caller: impl Caller<Context = Context<S>>,
    info_ptr: u32,
    info_size: u32,
) -> VMResult<u32> {
    let block_time_cost = caller.context().config.host_ffi_opt_costs().env_info;
    panic!("casper_env_info should not be used anymore");

    let (caller_kind, caller_addr) = match &caller.context().caller {
        Key::Account(account_hash) => (EntityKindTag::Account as u32, account_hash.value()),
        Key::Package(smart_contract_addr) => {
            (EntityKindTag::Contract as u32, smart_contract_addr.value())
        }
        Key::Hash(hash_addr) => (EntityKindTag::Contract as u32, *hash_addr),
        other => panic!("Unexpected caller: {other:?}"),
    };

    let (callee_kind, callee_addr) = match &caller.context().callee {
        Key::Account(initiator_addr) => (EntityKindTag::Account as u32, initiator_addr.value()),
        Key::Package(smart_contract_addr) => {
            (EntityKindTag::Contract as u32, smart_contract_addr.value())
        }
        Key::Hash(hash_addr) => (EntityKindTag::Contract as u32, *hash_addr),
        other => panic!("Unexpected callee: {other:?}"),
    };

    let transferred_value = caller.context().transferred_value;

    let block_time = caller.context().block_time.value();
    let protocol_version = caller
        .context()
        .runtime_native_config
        .protocol_version()
        .value();
    let parent_block_hash = caller.context().parent_block_hash;
    let block_height = caller.context().block_height;
    // `EnvInfo` in little-endian representation.
    let env_info = EnvInfo {
        caller_addr,
        caller_kind,
        callee_addr,
        callee_kind: callee_kind.to_le(),
        transferred_value: transferred_value.to_le(),
        block_time: block_time.to_le(),
        protocol_version_major: protocol_version.major.to_le(),
        protocol_version_minor: protocol_version.minor.to_le(),
        protocol_version_patch: protocol_version.patch.to_le(),
        parent_block_hash,
        block_height,
    };

    let env_info_bytes = borsh::to_vec(&env_info).map_err(|_| FatalHostError::Serialization)?;
    let write_len = env_info_bytes.len().min(info_size as usize);
    caller.memory_write(info_ptr.wrapped_try_into()?, &env_info_bytes[..write_len])?;

    Ok(HOST_ERROR_SUCCESS)
}

/// Computes digest hash, using provided algorithm type.
///
/// # Arguments
///
/// * `in_ptr` - pointer to the location where argument bytes will be copied from the host side
/// * `in_size` - size of output pointer
/// * `hash_algo_type` - integer representation of HashAlgorithm enum variant
/// * `out_ptr` - pointer to the location where argument bytes will be copied to the host side
pub fn casper_generic_hash<S: GlobalStateReader>(
    mut caller: impl Caller<Context = Context<S>>,
    in_ptr: u32,
    in_size: u32,
    hash_algorithm: u32,
    out_ptr: u32,
) -> VMResult<u32> {
    const DIGEST_LENGTH: usize = 32;

    let in_bytes: Vec<u8> = caller.memory_read(in_ptr.wrapped_try_into()?, in_size as usize)?;

    // Charge for parameter weights.
    let generic_hash_cost = caller.context().config.host_ffi_opt_costs().generic_hash;

    panic!("casper_generic_hash should not be used anymore");

    let hash_algorithm =
        HashAlgorithm::from_u32(hash_algorithm).ok_or(FatalHostError::TypeConversion)?;

    let hashed_bytes = match hash_algorithm {
        HashAlgorithm::Blake2b => {
            let mut result = [0; DIGEST_LENGTH];
            let mut hasher = Blake2bVar::new(DIGEST_LENGTH).map_err(|_| {
                ExecuteError::Fatal(FatalHostError::CorruptExecutionState(
                    "Error when creating instance of Blake2bVar hashing".to_owned(),
                ))
            })?;
            hasher.update(in_bytes.as_ref());
            hasher.finalize_variable(&mut result).ok();
            result
        }
        HashAlgorithm::Blake3 => {
            let mut result = [0; DIGEST_LENGTH];
            let mut hasher = blake3::Hasher::new();
            hasher.update(in_bytes.as_ref());
            let hash = hasher.finalize();
            let hash_bytes: &[u8; DIGEST_LENGTH] = hash.as_bytes();
            result.copy_from_slice(hash_bytes);
            result
        }
        HashAlgorithm::Sha256 => Sha256::digest(in_bytes).into(),
        HashAlgorithm::Keccak256 => {
            use keccak_asm::Keccak256;
            let mut result = [0u8; DIGEST_LENGTH];
            let mut hasher = Keccak256::new();
            KeccakDigest::update(&mut hasher, &in_bytes);
            let hash = KeccakDigest::finalize(hasher);
            result.copy_from_slice(&hash);
            result
        }
    };

    caller.memory_write(out_ptr.wrapped_try_into()?, &hashed_bytes)?;

    Ok(HOST_ERROR_SUCCESS)
}

/// Recovers a Secp256k1 public key from a signed message
/// and a signature used in the process of signing.
///
/// # Arguments
///
/// * `message_ptr` - pointer to the signed data
/// * `message_size` - length of the signed data in bytes
/// * `signature_ptr` - pointer to byte-encoded signature
/// * `signature_size` - length of the byte-encoded signature
/// * `public_key_ptr` - pointer to a buffer of size PublicKey::SECP256K1_LENGTH which will be
///   populated with the recovered key's bytes representation
/// * `recovery_id` - an integer value 0, 1, 2, or 3 used to select the correct public key from the
///   signature:
///   - Low bit (0/1): was the y-coordinate of the affine point resulting from the fixed-base
///     multiplication 𝑘×𝑮 odd?
///   - Hi bit (3/4): did the affine x-coordinate of 𝑘×𝑮 overflow the order of the scalar field,
///     requiring a reduction when computing r?
pub fn casper_recover_secp256k1<S: GlobalStateReader>(
    mut caller: impl Caller<Context = Context<S>>,
    message_ptr: u32,
    message_size: u32,
    signature_ptr: u32,
    signature_size: u32,
    public_key_ptr: u32,
    recovery_id: u32,
) -> VMResult<u32> {
    let recover_secp256k1_cost = caller
        .context()
        .config
        .host_ffi_opt_costs()
        .recover_secp256k1;

    panic!("casper_recover_secp256k1 should not be used anymore");

    if recovery_id >= 4 {
        return Ok(HOST_ERROR_INVALID_INPUT);
    }

    let message = caller.memory_read(message_ptr.wrapped_try_into()?, message_size as usize)?;
    let signature_bytes =
        caller.memory_read(signature_ptr.wrapped_try_into()?, signature_size as usize)?;
    let Ok((signature, _)) = Signature::from_bytes(&signature_bytes) else {
        return Ok(HOST_ERROR_INVALID_DATA);
    };

    let Ok(public_key) =
        casper_types::crypto::recover_secp256k1(message, &signature, recovery_id as u8)
    else {
        return Ok(HOST_ERROR_INVALID_INPUT);
    };

    let Ok(key_bytes) = public_key.to_bytes() else {
        return Ok(HOST_ERROR_PAYLOAD_TOO_LONG);
    };

    caller.memory_write(public_key_ptr.wrapped_try_into()?, &key_bytes)?;

    Ok(HOST_ERROR_SUCCESS)
}
