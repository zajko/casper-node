use casper_types::{
    account::AccountHash,
    bytesrepr::{self, Bytes, FromBytes, ToBytes, U8_SERIALIZED_LENGTH},
    BlockIdentifier, Gas, TransactionArgs, TransactionEntryPoint, TransactionTarget,
};
use core::convert::TryFrom;
use std::collections::BTreeSet;

/// Errors that can occur during sandboxed execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SandboxedExecutionError {
    /// The contract rolled back execution.
    CalleeRolledBack,
    /// The contract trapped during execution.
    CalleeTrapped,
    /// The contract ran out of gas.
    CalleeGasDepleted,
    /// The contract is not callable (missing export).
    NotCallable,
    /// The contract code was not found.
    CodeNotFound,
    /// An internal host error occurred.
    InternalHostError,
    /// No active contract in a package.
    NoActiveContract,
    /// Entity not found
    EntityNotFound,
    /// Tried to upgrade a contract in a locked package.
    LockedPackage,
    /// Input invalid
    InputInvalid,
    /// V1 execution engine error
    V1EngineError(String),
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SandboxedExecutionErrorTag {
    CalleeRolledBack = 0,
    CalleeTrapped = 1,
    CalleeGasDepleted = 2,
    NotCallable = 3,
    CodeNotFound = 4,
    InternalHostError = 5,
    NoActiveContract = 6,
    EntityNotFound = 7,
    LockedPackage = 8,
    InputInvalid = 9,
    V1EngineError = 10,
}

impl TryFrom<u8> for SandboxedExecutionErrorTag {
    type Error = bytesrepr::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            x if x == SandboxedExecutionErrorTag::CalleeRolledBack as u8 => {
                Ok(SandboxedExecutionErrorTag::CalleeRolledBack)
            }
            x if x == SandboxedExecutionErrorTag::CalleeTrapped as u8 => {
                Ok(SandboxedExecutionErrorTag::CalleeTrapped)
            }
            x if x == SandboxedExecutionErrorTag::CalleeGasDepleted as u8 => {
                Ok(SandboxedExecutionErrorTag::CalleeGasDepleted)
            }
            x if x == SandboxedExecutionErrorTag::NotCallable as u8 => {
                Ok(SandboxedExecutionErrorTag::NotCallable)
            }
            x if x == SandboxedExecutionErrorTag::CodeNotFound as u8 => {
                Ok(SandboxedExecutionErrorTag::CodeNotFound)
            }
            x if x == SandboxedExecutionErrorTag::InternalHostError as u8 => {
                Ok(SandboxedExecutionErrorTag::InternalHostError)
            }
            x if x == SandboxedExecutionErrorTag::NoActiveContract as u8 => {
                Ok(SandboxedExecutionErrorTag::NoActiveContract)
            }
            x if x == SandboxedExecutionErrorTag::EntityNotFound as u8 => {
                Ok(SandboxedExecutionErrorTag::EntityNotFound)
            }
            x if x == SandboxedExecutionErrorTag::LockedPackage as u8 => {
                Ok(SandboxedExecutionErrorTag::LockedPackage)
            }
            x if x == SandboxedExecutionErrorTag::InputInvalid as u8 => {
                Ok(SandboxedExecutionErrorTag::InputInvalid)
            }
            x if x == SandboxedExecutionErrorTag::V1EngineError as u8 => {
                Ok(SandboxedExecutionErrorTag::V1EngineError)
            }
            _ => Err(bytesrepr::Error::Formatting),
        }
    }
}

impl SandboxedExecutionError {
    fn tag(&self) -> SandboxedExecutionErrorTag {
        match self {
            SandboxedExecutionError::CalleeRolledBack => {
                SandboxedExecutionErrorTag::CalleeRolledBack
            }
            SandboxedExecutionError::CalleeTrapped => SandboxedExecutionErrorTag::CalleeTrapped,
            SandboxedExecutionError::CalleeGasDepleted => {
                SandboxedExecutionErrorTag::CalleeGasDepleted
            }
            SandboxedExecutionError::NotCallable => SandboxedExecutionErrorTag::NotCallable,
            SandboxedExecutionError::CodeNotFound => SandboxedExecutionErrorTag::CodeNotFound,
            SandboxedExecutionError::InternalHostError => {
                SandboxedExecutionErrorTag::InternalHostError
            }
            SandboxedExecutionError::NoActiveContract => {
                SandboxedExecutionErrorTag::NoActiveContract
            }
            SandboxedExecutionError::EntityNotFound => SandboxedExecutionErrorTag::EntityNotFound,
            SandboxedExecutionError::LockedPackage => SandboxedExecutionErrorTag::LockedPackage,
            SandboxedExecutionError::InputInvalid => SandboxedExecutionErrorTag::InputInvalid,
            SandboxedExecutionError::V1EngineError(_) => SandboxedExecutionErrorTag::V1EngineError,
        }
    }
}

impl core::fmt::Display for SandboxedExecutionError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            SandboxedExecutionError::CalleeRolledBack => write!(f, "contract rolled back"),
            SandboxedExecutionError::CalleeTrapped => write!(f, "contract trapped"),
            SandboxedExecutionError::CalleeGasDepleted => write!(f, "contract gas depleted"),
            SandboxedExecutionError::NotCallable => write!(f, "contract not callable"),
            SandboxedExecutionError::CodeNotFound => write!(f, "contract code not found"),
            SandboxedExecutionError::InternalHostError => write!(f, "internal host error"),
            SandboxedExecutionError::NoActiveContract => write!(f, "no active contract"),
            SandboxedExecutionError::EntityNotFound => write!(f, "entity not found"),
            SandboxedExecutionError::LockedPackage => write!(f, "locked package"),
            SandboxedExecutionError::InputInvalid => write!(f, "input invalid"),
            SandboxedExecutionError::V1EngineError(v1_engine_error) => {
                write!(f, "{}", v1_engine_error)
            }
        }
    }
}

const SANDBOXED_EXECUTION_REQUEST_V1_TAG: u8 = 0;

#[derive(Debug, PartialEq)]
pub enum SandboxedExecutionRequest {
    V1(SandboxedExecutionRequestV1),
}

impl ToBytes for SandboxedExecutionRequest {
    fn to_bytes(&self) -> Result<Vec<u8>, bytesrepr::Error> {
        let mut writer = bytesrepr::allocate_buffer(self)?;
        match self {
            SandboxedExecutionRequest::V1(sandboxed_execution_request_v1) => {
                SANDBOXED_EXECUTION_REQUEST_V1_TAG.write_bytes(&mut writer)?;
                sandboxed_execution_request_v1.write_bytes(&mut writer)?;
            }
        }
        Ok(writer)
    }

    fn serialized_length(&self) -> usize {
        U8_SERIALIZED_LENGTH
            + match self {
                SandboxedExecutionRequest::V1(sandboxed_execution_request_v1) => {
                    sandboxed_execution_request_v1.serialized_length()
                }
            }
    }
}

impl FromBytes for SandboxedExecutionRequest {
    fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), bytesrepr::Error> {
        let (enum_tag, remainder) = u8::from_bytes(bytes)?;
        match enum_tag {
            SANDBOXED_EXECUTION_REQUEST_V1_TAG => {
                let (v1, remainder) = SandboxedExecutionRequestV1::from_bytes(remainder)?;
                Ok((SandboxedExecutionRequest::V1(v1), remainder))
            }
            _ => Err(bytesrepr::Error::Formatting),
        }
    }
}

/// A request to execute a sandboxed contract. Pure functions, read-only getters, beacons,
/// sentinels, and similar functionality that does not require invocation of other contracts or
/// mutation of state are supported.
#[derive(Debug, PartialEq)]
pub struct SandboxedExecutionRequestV1 {
    /// Block identifier. None means "tip"
    pub block_identifier: Option<BlockIdentifier>,
    /// Transaction target
    pub target: TransactionTarget,
    /// Entry point
    pub entry_point: TransactionEntryPoint,
    /// The address of the account that would initiate the contract call.
    pub initiator: AccountHash,
    /// Input data for the query.
    pub args: TransactionArgs,
    /// Authorization keys
    pub authorization_keys: BTreeSet<AccountHash>,
}

impl ToBytes for SandboxedExecutionRequestV1 {
    fn to_bytes(&self) -> Result<Vec<u8>, bytesrepr::Error> {
        let mut writer = bytesrepr::allocate_buffer(self)?;
        self.write_bytes(&mut writer)?;
        Ok(writer)
    }

    fn serialized_length(&self) -> usize {
        self.block_identifier.serialized_length()
            + self.target.serialized_length()
            + self.initiator.serialized_length()
            + self.entry_point.serialized_length()
            + self.args.serialized_length()
            + self.authorization_keys.serialized_length()
    }

    fn write_bytes(&self, writer: &mut Vec<u8>) -> Result<(), bytesrepr::Error> {
        self.block_identifier.write_bytes(writer)?;
        self.target.write_bytes(writer)?;
        self.entry_point.write_bytes(writer)?;
        self.initiator.write_bytes(writer)?;
        self.args.write_bytes(writer)?;
        self.authorization_keys.write_bytes(writer)
    }
}

impl FromBytes for SandboxedExecutionRequestV1 {
    fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), bytesrepr::Error> {
        let (block_identifier, remainder) = FromBytes::from_bytes(bytes)?;
        let (target, remainder) = FromBytes::from_bytes(remainder)?;
        let (entry_point, remainder) = FromBytes::from_bytes(remainder)?;
        let (initiator, remainder) = FromBytes::from_bytes(remainder)?;
        let (args, remainder) = FromBytes::from_bytes(remainder)?;
        let (authorization_keys, remainder) = FromBytes::from_bytes(remainder)?;
        Ok((
            SandboxedExecutionRequestV1 {
                block_identifier,
                target,
                entry_point,
                initiator,
                args,
                authorization_keys,
            },
            remainder,
        ))
    }
}

#[cfg(test)]
impl SandboxedExecutionRequest {
    /// Generates a random request for testing.
    pub fn random(rng: &mut casper_types::testing::TestRng) -> Self {
        use std::iter::FromIterator;

        use rand::Rng;
        let block_identifier = if rng.gen_bool(0.5) {
            Some(BlockIdentifier::random(rng))
        } else {
            None
        };

        SandboxedExecutionRequest::V1(SandboxedExecutionRequestV1 {
            block_identifier,
            target: TransactionTarget::random(rng),
            entry_point: TransactionEntryPoint::random(rng),
            initiator: AccountHash::new(rng.gen()),
            args: TransactionArgs::Bytesrepr(Bytes::from(vec![0, 1, 2])),
            authorization_keys: BTreeSet::from_iter(rng.random_vec(0..10)),
        })
    }
}

/// Result of a sandboxed execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SandboxedExecutionResult {
    /// Error while executing, if any.
    pub error: Option<SandboxedExecutionError>,
    /// Output data returned by the contract.
    pub output: Option<Bytes>,
    /// Gas usage tracked during execution.
    pub gas_usage: Gas,
}

impl SandboxedExecutionResult {
    /// Returns the error if the execution failed.
    pub fn error(&self) -> Option<&SandboxedExecutionError> {
        self.error.as_ref()
    }

    /// Returns the output data if the execution succeeded.
    pub fn output(&self) -> Option<&Bytes> {
        self.output.as_ref()
    }

    /// Returns the gas spent.
    pub fn gas_usage(&self) -> &Gas {
        &self.gas_usage
    }

    /// Returns true if the query was successful.
    pub fn is_success(&self) -> bool {
        self.error.is_none()
    }
}

impl ToBytes for SandboxedExecutionError {
    fn to_bytes(&self) -> Result<Vec<u8>, bytesrepr::Error> {
        let mut writer = bytesrepr::allocate_buffer(self)?;
        let tag: u8 = self.tag() as u8;
        tag.write_bytes(&mut writer)?;
        if let SandboxedExecutionError::V1EngineError(msg) = self {
            msg.write_bytes(&mut writer)?;
        }
        Ok(writer)
    }

    fn serialized_length(&self) -> usize {
        let base = bytesrepr::U8_SERIALIZED_LENGTH; // tag
        match self {
            SandboxedExecutionError::V1EngineError(msg) => base + msg.serialized_length(),
            _ => base,
        }
    }

    fn write_bytes(&self, writer: &mut Vec<u8>) -> Result<(), bytesrepr::Error> {
        writer.extend(self.to_bytes()?);
        Ok(())
    }
}

impl FromBytes for SandboxedExecutionError {
    fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), bytesrepr::Error> {
        let (tag_u8, remainder) = u8::from_bytes(bytes)?;
        let tag = SandboxedExecutionErrorTag::try_from(tag_u8)?;
        match tag {
            SandboxedExecutionErrorTag::CalleeRolledBack => {
                Ok((SandboxedExecutionError::CalleeRolledBack, remainder))
            }
            SandboxedExecutionErrorTag::CalleeTrapped => {
                Ok((SandboxedExecutionError::CalleeTrapped, remainder))
            }
            SandboxedExecutionErrorTag::CalleeGasDepleted => {
                Ok((SandboxedExecutionError::CalleeGasDepleted, remainder))
            }
            SandboxedExecutionErrorTag::NotCallable => {
                Ok((SandboxedExecutionError::NotCallable, remainder))
            }
            SandboxedExecutionErrorTag::CodeNotFound => {
                Ok((SandboxedExecutionError::CodeNotFound, remainder))
            }
            SandboxedExecutionErrorTag::InternalHostError => {
                Ok((SandboxedExecutionError::InternalHostError, remainder))
            }
            SandboxedExecutionErrorTag::NoActiveContract => {
                Ok((SandboxedExecutionError::NoActiveContract, remainder))
            }
            SandboxedExecutionErrorTag::EntityNotFound => {
                Ok((SandboxedExecutionError::EntityNotFound, remainder))
            }
            SandboxedExecutionErrorTag::LockedPackage => {
                Ok((SandboxedExecutionError::LockedPackage, remainder))
            }
            SandboxedExecutionErrorTag::InputInvalid => {
                Ok((SandboxedExecutionError::InputInvalid, remainder))
            }
            SandboxedExecutionErrorTag::V1EngineError => {
                let (msg, rem) = String::from_bytes(remainder)?;
                Ok((SandboxedExecutionError::V1EngineError(msg), rem))
            }
        }
    }
}

impl ToBytes for SandboxedExecutionResult {
    fn to_bytes(&self) -> Result<Vec<u8>, bytesrepr::Error> {
        let mut writer = bytesrepr::allocate_buffer(self)?;
        self.write_bytes(&mut writer)?;
        Ok(writer)
    }

    fn serialized_length(&self) -> usize {
        self.error.serialized_length()
            + self.output.serialized_length()
            + self.gas_usage.serialized_length()
    }

    fn write_bytes(&self, writer: &mut Vec<u8>) -> Result<(), bytesrepr::Error> {
        self.error.write_bytes(writer)?;
        self.output.write_bytes(writer)?;
        self.gas_usage.write_bytes(writer)
    }
}

impl FromBytes for SandboxedExecutionResult {
    fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), bytesrepr::Error> {
        let (error, bytes) = Option::<SandboxedExecutionError>::from_bytes(bytes)?;
        let (output, bytes) = Option::<Bytes>::from_bytes(bytes)?;
        let (gas_usage, bytes) = Gas::from_bytes(bytes)?;
        Ok((
            SandboxedExecutionResult {
                error,
                output,
                gas_usage,
            },
            bytes,
        ))
    }
}
