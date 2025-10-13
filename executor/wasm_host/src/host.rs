pub(crate) mod control;
pub(crate) mod crypto;
pub(crate) mod emit;
pub(crate) mod global_state;
pub(crate) mod io;
use std::sync::Arc;

use bytes::Bytes;
use casper_executor_wasm_common::error::CallError;

use casper_executor_wasm_interface::{
    executor::{
        ControlMethods, CryptoMethods, EmitMethods, ExecuteError, ExecuteRequestBuilder,
        ExecuteResult, ExecutionKind, Executor, GlobalStateMethods, IOMethods, SystemContractCall,
    },
    u32_from_host_result, Caller, FatalHostError, VMError, VMResult,
};
use casper_storage::global_state::GlobalStateReader;
use casper_types::{bytesrepr::ToBytes, BlockHash, Digest, EntityAddr, Key, StoredValue};
use num_derive::FromPrimitive;
use tracing::error;

use crate::{
    context::Context,
    host::{
        control::{host_call, host_upgrade},
        crypto::{
            host_alt_bn128_add, host_alt_bn128_mul, host_alt_bn128_pairing, host_generic_hash,
            host_recover_secp256k1,
        },
        emit::{emit, print_std},
        global_state::{
            host_create, host_env_balance, host_env_info, host_read, host_remove, host_write,
        },
        io::{host_copy_input, host_ret},
    },
};
use casper_executor_wasm_interface::executor::{ExecuteRequest, FFIMenu};

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

/// Writes a message to the global state and charges for storage used.
fn metered_write<S: GlobalStateReader>(
    caller: &mut impl Caller<Context = Context<S>>,
    key: Key,
    value: StoredValue,
) -> VMResult<()> {
    if caller.context().sandboxed {
        return Err(VMError::Execute(ExecuteError::AttemptWriteInRestricted));
    }

    charge_gas_storage(caller, value.serialized_length())?;
    caller.context_mut().tracking_copy.write(key, value);
    Ok(())
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

/// Single ffi function that exposes functionality of the host to wasm clients.
///
/// # Arguments
/// - `ffi_opt`: number which will be interpreted as [FFIMenu]
/// - `input_ptr`: pointer in the wasm execution memory space to the input data.
/// - `input_len`: number of bytes to pass
/// - `cb_alloc`: pointer to function in the wasm code which should be used to allocate in-wasm
///   memory for output data.
/// - `cb_ctx`: If the wasm a-priori knows what will be the size of the output data it can
///   pre-allocate and pass the pointer in this variable. In this case we won't call function under
///   `cb_alloc` to allocate the memory. This can be a cheaper (less gas consuming) option if the
///   wasm creator knows what the memory outpu is.
///
/// # Output:
/// - The return value is either:
///     - Ok(0): The function call was successfull
///     - Ok(err_code): The function call itself was successfull, but there was an error with the
///       input data for the specific host functionality defined by `ffi_opt`
///     - Err(vm_err): There was an error with executing the vm call
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

    let input_data = if input_ptr == 0 {
        // If the user didn't pass a input data pointer default to empty data
        Bytes::default()
    } else {
        caller.memory_read(input_ptr, input_len as _)?.into()
    };

    let (output_bytes, exit_code) = match option {
        FFIMenu::Mint(mint_method) => {
            let system_contract_call_opt = SystemContractCall::Mint(mint_method);
            // Limit the call to remaining gas.
            let gas_limit = caller
                .get_remaining_points()?
                .try_into_remaining()
                .map_err(|_| FatalHostError::TypeConversion)?;

            handle_as_contract_call(&mut caller, system_contract_call_opt, input_data, gas_limit)
        }
        FFIMenu::Auction(auction_method) => {
            let system_contract_call_opt = SystemContractCall::Auction(auction_method);
            // Limit the call to remaining gas.
            let gas_limit = caller
                .get_remaining_points()?
                .try_into_remaining()
                .map_err(|_| FatalHostError::TypeConversion)?;

            handle_as_contract_call(&mut caller, system_contract_call_opt, input_data, gas_limit)
        }
        FFIMenu::Crypto(crypto_methods) => match crypto_methods {
            CryptoMethods::AltBn128Add => host_alt_bn128_add(input_data),
            CryptoMethods::AltBn128Multiply => host_alt_bn128_mul(input_data),
            CryptoMethods::AltBn128Pairing => host_alt_bn128_pairing(input_data),
            CryptoMethods::GenericHash => host_generic_hash(input_data),
            CryptoMethods::RecoverSecp256K1 => host_recover_secp256k1(input_data),
        },
        FFIMenu::Emit(emit_methods) => match emit_methods {
            EmitMethods::PrintStd => print_std(input_data).map(|code| (None, code)),
            EmitMethods::Native => emit(&mut caller, input_data).map(|code| (None, code)),
        },
        FFIMenu::GlobalState(global_state_methods) => match global_state_methods {
            GlobalStateMethods::Read => host_read(&mut caller, input_data),
            GlobalStateMethods::Write => {
                host_write(&mut caller, input_data).map(|code| (None, code))
            }
            GlobalStateMethods::Remove => {
                host_remove(&mut caller, input_data).map(|code| (None, code))
            }
            GlobalStateMethods::GetBalance => host_env_balance(&mut caller, input_data),
            GlobalStateMethods::GetInfo => host_env_info(&mut caller),
            GlobalStateMethods::Create => host_create(&mut caller, input_data),
        },
        FFIMenu::Control(control_methods) => match control_methods {
            ControlMethods::Call => host_call(&mut caller, input_data),
            ControlMethods::Upgrade => {
                host_upgrade(&mut caller, input_data).map(|code| (None, code))
            }
        },
        FFIMenu::IO(io_methods) => match io_methods {
            IOMethods::Return => host_ret(input_data).map(|code| (None, code)),
            IOMethods::CopyInput => host_copy_input(&mut caller),
        },
    }?;

    if let Some(output) = output_bytes {
        let out_ptr: u32 = if cb_alloc != 0 {
            caller.alloc(cb_alloc, output.len(), cb_ctx)?
        } else {
            // treats cb_ctx as data
            cb_ctx
        };
        if out_ptr != 0 {
            caller.memory_write(out_ptr.wrapped_try_into()?, &output)?;
        }
    }
    Ok(exit_code)
}

fn handle_as_contract_call<S: GlobalStateReader + 'static>(
    caller: &mut impl Caller<Context = Context<S>>,
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

fn exec<S: GlobalStateReader + 'static>(
    caller: &mut impl Caller<Context = Context<S>>,
    execute_request: ExecuteRequest,
) -> VMResult<(Option<Bytes>, u32)> {
    let tracking_copy = caller.context().tracking_copy.fork2();
    let (gas_usage, host_result, output) =
        match caller.executor().execute(tracking_copy, execute_request) {
            Ok(ExecuteResult {
                host_error,
                output,
                gas_usage,
                effects,
                cache,
                messages,
            }) => {
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

                (gas_usage, host_result, output)
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
