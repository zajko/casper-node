use std::borrow::Cow;
use std::collections::BTreeMap;

use bytes::Bytes;
use casper_executor_wasm_common::entry_point::{
    ENTRY_POINT_PAYMENT_CALLER, ENTRY_POINT_PAYMENT_DIRECT_INVOCATION_ONLY,
    ENTRY_POINT_PAYMENT_SELF_ONWARD,
};
use casper_executor_wasm_common::error::{
    HOST_ERROR_INVALID_DATA, HOST_ERROR_INVALID_INPUT, HOST_ERROR_NOT_FOUND, HOST_ERROR_SUCCESS,
};
use casper_executor_wasm_common::keyspace::{Keyspace, KeyspaceTag};
use casper_executor_wasm_interface::{
    executor::ExecuteError, Caller, FatalHostError, VMError, VMResult,
};
use casper_storage::{global_state::GlobalStateReader, tracking_copy::TrackingCopyExt};
use casper_types::addressable_entity::NamedKeyValue;
use casper_types::{
    addressable_entity::MessageTopicError,
    bytesrepr::ToBytes,
    contract_messages::{Message, MessageAddr, MessagePayload, MessageTopicSummary},
    BlockGlobalAddr, BlockTime, CLValue, Digest, EntityAddr, Key, StoredValue,
};
use casper_types::{
    bytesrepr, AccessRights, CLType, EntryPointPayment, EntryPointValue, NamedKeys,
};
use num_traits::FromPrimitive;
use tracing::error;

use crate::context::Context;
use crate::host::{context_to_entity_addr, metered_write};

/// Read value under from global state under a key.
pub(crate) fn host_read<S: GlobalStateReader + 'static>(
    caller: &mut impl Caller<Context = Context<S>>,
    input: Bytes,
) -> VMResult<(Option<Bytes>, u32)> {
    let (key_tag, key_payload_bytes) =
        match bytesrepr::deserialize_from_slice::<&Bytes, (u64, Vec<u8>)>(&input) {
            Ok(res) => res,
            Err(_) => {
                return Ok((None, HOST_ERROR_INVALID_INPUT));
            }
        };
    let keyspace_tag = match KeyspaceTag::from_u64(key_tag) {
        Some(keyspace_tag) => keyspace_tag,
        None => {
            // Unknown keyspace received, return error
            return Ok((None, HOST_ERROR_INVALID_INPUT));
        }
    };

    let keyspace = match keyspace_tag {
        KeyspaceTag::State => Keyspace::State,
        KeyspaceTag::Context => Keyspace::Context(&key_payload_bytes),
        KeyspaceTag::NamedKey => {
            let key_name = match std::str::from_utf8(&key_payload_bytes) {
                Ok(key_name) => key_name,
                Err(_) => {
                    return Ok((None, HOST_ERROR_INVALID_DATA));
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
            return Ok((None, HOST_ERROR_NOT_FOUND));
        }
    };

    let global_state_read_result = caller.context_mut().tracking_copy.read(&global_state_key);
    let global_state_raw_bytes: Cow<[u8]> = match global_state_read_result {
        Ok(Some(StoredValue::CLValue(cl_value))) => {
            let CLType::Any = cl_value.cl_type() else {
                return Err(FatalHostError::TypeConversion)?;
            };
            Cow::Owned(cl_value.inner_bytes().to_owned())
        }
        Ok(Some(StoredValue::NamedKey(named_key_value))) => {
            // Dereference named key to its URef and return the underlying Any bytes
            let Ok(Key::URef(uref)) = named_key_value.get_key() else {
                return Ok((None, HOST_ERROR_INVALID_DATA));
            };

            match caller.context_mut().tracking_copy.read(&Key::URef(uref)) {
                Ok(Some(StoredValue::CLValue(cl_value))) => {
                    let CLType::Any = cl_value.cl_type() else {
                        return Ok((None, HOST_ERROR_INVALID_DATA));
                    };
                    Cow::Owned(cl_value.inner_bytes().to_owned())
                }
                Ok(Some(_)) => {
                    return Ok((None, HOST_ERROR_INVALID_DATA));
                }
                Ok(None) => {
                    return Ok((None, HOST_ERROR_NOT_FOUND));
                }
                Err(_error) => {
                    return Err(FatalHostError::TrackingCopy.into());
                }
            }
        }
        Ok(Some(StoredValue::Contract(contract))) => match keyspace {
            Keyspace::NamedKey(name) => {
                let Some(Key::URef(uref)) = contract.named_keys().get(name) else {
                    return Ok((None, HOST_ERROR_INVALID_DATA));
                };

                match caller.context_mut().tracking_copy.read(&Key::URef(*uref)) {
                    Ok(Some(StoredValue::CLValue(cl_value))) => {
                        let CLType::Any = cl_value.cl_type() else {
                            return Ok((None, HOST_ERROR_INVALID_DATA));
                        };
                        Cow::Owned(cl_value.inner_bytes().to_owned())
                    }
                    Ok(Some(_)) => {
                        return Ok((None, HOST_ERROR_INVALID_DATA));
                    }
                    Ok(None) => {
                        return Ok((None, HOST_ERROR_NOT_FOUND));
                    }
                    Err(_error) => {
                        return Err(FatalHostError::TrackingCopy.into());
                    }
                }
            }
            Keyspace::AllNamedKeys => match contract.take_named_keys().to_bytes() {
                Ok(bytes) => Cow::Owned(bytes),
                Err(_) => return Ok((None, HOST_ERROR_INVALID_INPUT)),
            },
            _ => {
                error!(?keyspace, "unsupported keyspace");
                return Ok((None, HOST_ERROR_INVALID_INPUT));
            }
        },
        Ok(Some(StoredValue::AddressableEntity(_))) => {
            if let Keyspace::AllNamedKeys = keyspace {
                let entity_addr = context_to_entity_addr(caller.context());

                let named_keys = caller
                    .context_mut()
                    .tracking_copy
                    .get_named_keys(entity_addr)
                    .map(|named_keys| named_keys.to_bytes());

                match named_keys {
                    Ok(Ok(bytes)) => Cow::Owned(bytes),
                    Ok(_) | Err(_) => return Ok((None, HOST_ERROR_INVALID_INPUT)),
                }
            } else {
                return Ok((None, HOST_ERROR_INVALID_INPUT));
            }
        }
        Ok(Some(StoredValue::EntryPoint(EntryPointValue::V1CasperVm(entry_point)))) => {
            match entry_point.entry_point_payment() {
                EntryPointPayment::Caller => Cow::Borrowed(&[ENTRY_POINT_PAYMENT_CALLER]),
                EntryPointPayment::DirectInvocationOnly => {
                    Cow::Borrowed(&[ENTRY_POINT_PAYMENT_DIRECT_INVOCATION_ONLY])
                }
                EntryPointPayment::SelfOnward => Cow::Borrowed(&[ENTRY_POINT_PAYMENT_SELF_ONWARD]),
            }
        }
        Ok(Some(stored_value)) => {
            // TODO: Backwards compatibility with old EE, although it's not clear if we should
            // do it at the storage level. Since new VM has storage isolated
            // from the Wasm (i.e. we have Keyspace on the wasm which gets
            // converted to a global state `Key`). I think if we were to pursue
            // this we'd add a new `Keyspace` enum variant for each old
            // VM supported Key types (i.e. URef, Dictionary perhaps) for some period of time,
            // then deprecate this.
            todo!("Unsupported {stored_value:?}")
        }
        Ok(None) => return Ok((None, HOST_ERROR_NOT_FOUND)), // Entry does not exist
        Err(error) => {
            // To protect the network against potential non-determinism (i.e. one validator runs
            // out of space or just faces I/O issues that other validators may
            // not have) we're simply aborting the process, hoping that once the
            // node goes back online issues are resolved on the validator side.
            // TODO: We should signal this to the contract runtime somehow, and
            // let validator nodes skip execution.
            error!(?error, "Error while reading from storage; aborting");
            panic!("Error while reading from storage; aborting key={global_state_key:?} error={error:?}")
        }
    };
    Ok((
        Some(Bytes::from(global_state_raw_bytes.to_vec())),
        HOST_ERROR_SUCCESS,
    ))
}

/// Write value under a key.
pub(crate) fn host_write<S: GlobalStateReader + 'static>(
    caller: &mut impl Caller<Context = Context<S>>,
    input: Bytes,
) -> VMResult<u32> {
    let (key_space, key_payload_bytes, value) =
        match bytesrepr::deserialize_from_slice::<&Bytes, (u64, Vec<u8>, Vec<u8>)>(&input) {
            Ok(res) => res,
            Err(_) => {
                return Ok(HOST_ERROR_INVALID_INPUT);
            }
        };
    let keyspace_tag = match KeyspaceTag::from_u64(key_space) {
        Some(keyspace_tag) => keyspace_tag,
        None => {
            // Unknown keyspace received, return error
            return Ok(HOST_ERROR_INVALID_INPUT);
        }
    };

    let keyspace = match keyspace_tag {
        KeyspaceTag::State => Keyspace::State,
        KeyspaceTag::Context => Keyspace::Context(&key_payload_bytes),
        KeyspaceTag::NamedKey => {
            let key_name = match std::str::from_utf8(&key_payload_bytes) {
                Ok(key_name) => key_name,
                Err(_) => {
                    return Ok(HOST_ERROR_INVALID_INPUT);
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

    let stored_value = match keyspace {
        Keyspace::State | Keyspace::Context(_) => {
            let cl_value_any = CLValue::from_components(CLType::Any, value);
            StoredValue::CLValue(cl_value_any)
        }
        Keyspace::NamedKey(name) => {
            // NamedKey points to a URef which holds CLValue::Any bytes
            let maybe_stored_value = caller
                .context_mut()
                .tracking_copy
                .read(&global_state_key)
                .map_err(|_| FatalHostError::TrackingCopy)?;

            let stored_value = match maybe_stored_value {
                Some(StoredValue::NamedKey(existing_named_key)) => {
                    let uref_to_use =
                        if let Ok(Key::URef(existing_uref)) = existing_named_key.get_key() {
                            existing_uref
                        } else {
                            let mut address_generator = caller.context().address_generator.write();
                            address_generator.new_uref(AccessRights::NONE)
                        };

                    // Point the named key to the URef
                    let named_key = Key::URef(uref_to_use);
                    let key_name = name.to_string();
                    let Ok(named_key_value) =
                        NamedKeyValue::from_concrete_values(named_key, key_name)
                    else {
                        return Ok(HOST_ERROR_INVALID_DATA);
                    };

                    StoredValue::NamedKey(named_key_value)
                }
                Some(StoredValue::Contract(mut contract)) => {
                    let uref = match contract.named_keys().get(name) {
                        Some(Key::URef(uref)) => *uref,
                        Some(_) => return Ok(HOST_ERROR_INVALID_INPUT),
                        None => {
                            let mut address_generator = caller.context().address_generator.write();
                            address_generator.new_uref(AccessRights::NONE)
                        }
                    };

                    // Write payload bytes under the URef as CLValue::Any
                    let cl_value_any = CLValue::from_components(CLType::Any, value.clone());
                    metered_write(caller, Key::URef(uref), StoredValue::CLValue(cl_value_any))?;

                    let named_keys = {
                        let mut ret = BTreeMap::new();
                        ret.insert(name.to_string(), Key::URef(uref));
                        NamedKeys::from(ret)
                    };
                    contract.named_keys_append(named_keys);

                    StoredValue::Contract(contract)
                }
                Some(_) => return Ok(HOST_ERROR_NOT_FOUND),
                None => {
                    let uref = {
                        let mut address_generator = caller.context().address_generator.write();
                        address_generator.new_uref(AccessRights::NONE)
                    };
                    // Write payload bytes under the URef as CLValue::Any
                    let cl_value_any = CLValue::from_components(CLType::Any, value.clone());
                    metered_write(caller, Key::URef(uref), StoredValue::CLValue(cl_value_any))?;

                    // Point the named key to the URef
                    let named_key = Key::URef(uref);
                    let key_name = name.to_string();
                    let Ok(named_key_value) =
                        NamedKeyValue::from_concrete_values(named_key, key_name)
                    else {
                        return Ok(HOST_ERROR_INVALID_DATA);
                    };

                    StoredValue::NamedKey(named_key_value)
                }
            };

            stored_value
        }
        Keyspace::AllNamedKeys => return Ok(HOST_ERROR_INVALID_INPUT),
    };

    metered_write(caller, global_state_key, stored_value)?;

    Ok(HOST_ERROR_SUCCESS)
}

fn keyspace_to_global_state_key<S: GlobalStateReader>(
    context: &Context<S>,
    keyspace: Keyspace<'_>,
) -> Option<Key> {
    let entity_addr = context_to_entity_addr(context);
    let ae_enabled = context.tracking_copy.addressable_entity_enabled();

    match keyspace {
        Keyspace::State => Some(Key::State(entity_addr)),
        Keyspace::Context(bytes) => {
            let digest = Digest::hash(bytes);
            Some(Key::NamedKey(NamedKeyAddr::new_named_key_entry(
                entity_addr,
                digest.value(),
            )))
        }
        Keyspace::NamedKey(payload) => {
            let digest = Digest::hash(payload.as_bytes());
            Some(Key::NamedKey(NamedKeyAddr::new_named_key_entry(
                entity_addr,
                digest.value(),
            )))
        }
        Keyspace::AllNamedKeys => {
            if ae_enabled {
                Some(Key::AddressableEntity(entity_addr))
            } else {
                match entity_addr {
                    EntityAddr::Account(hash_addr) => {
                        Some(Key::Account(AccountHash::new(hash_addr)))
                    }
                    EntityAddr::SmartContract(hash_addr) => Some(Key::Hash(hash_addr)),
                    _ => None,
                }
            }
        }
    }
}
