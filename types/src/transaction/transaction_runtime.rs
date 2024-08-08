use super::serialization::{BinaryPayload, CalltableFromBytes, CalltableToBytes};
use alloc::vec::Vec;
use core::fmt::{self, Display, Formatter};
#[cfg(feature = "datasize")]
use datasize::DataSize;
use macros::{CalltableFromBytes, CalltableToBytes};
#[cfg(any(feature = "testing", test))]
use rand::{
    distributions::{Distribution, Standard},
    Rng,
};
#[cfg(feature = "json-schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[cfg(doc)]
use super::Transaction;
use crate::bytesrepr;

/// The runtime used to execute a [`Transaction`].
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "datasize", derive(DataSize))]
#[cfg_attr(
    feature = "json-schema",
    derive(JsonSchema),
    schemars(description = "Runtime used to execute a Transaction.")
)]
#[serde(deny_unknown_fields)]
#[derive(CalltableToBytes, CalltableFromBytes)]
pub enum TransactionRuntime {
    /// The Casper Version 1 Virtual Machine.
    #[calltable(variant_index = 0)]
    VmCasperV1,
    /// The Casper Version 2 Virtual Machine.
    #[calltable(variant_index = 1)]
    VmCasperV2,
}

impl Display for TransactionRuntime {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            TransactionRuntime::VmCasperV1 => write!(formatter, "vm-casper-v1"),
            TransactionRuntime::VmCasperV2 => write!(formatter, "vm-casper-v2"),
        }
    }
}

#[cfg(any(feature = "testing", test))]
impl Distribution<TransactionRuntime> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TransactionRuntime {
        match rng.gen_range(0..=1) {
            0 => TransactionRuntime::VmCasperV1,
            1 => TransactionRuntime::VmCasperV2,
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bytesrepr_roundtrip() {
        for transaction_runtime in [
            TransactionRuntime::VmCasperV1,
            TransactionRuntime::VmCasperV2,
        ] {
            bytesrepr::test_serialization_roundtrip(&transaction_runtime);
        }
    }
}
