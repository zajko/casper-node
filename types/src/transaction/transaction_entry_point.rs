use alloc::{string::String, vec::Vec};
use core::fmt::{self, Display, Formatter};

use super::serialization::{BinaryPayload, CalltableFromBytes, CalltableToBytes};
#[cfg(any(feature = "testing", test))]
use crate::testing::TestRng;
use crate::{
    alloc::string::ToString,
    bytesrepr::{self, ToBytes},
    system::{auction, mint},
};
#[cfg(feature = "datasize")]
use datasize::DataSize;
use macros::{CalltableFromBytes, CalltableToBytes};
#[cfg(any(feature = "testing", test))]
use rand::Rng;
#[cfg(feature = "json-schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// The entry point of a [`Transaction`].
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "datasize", derive(DataSize))]
#[cfg_attr(
    feature = "json-schema",
    derive(JsonSchema),
    schemars(description = "Entry point of a Transaction.")
)]
#[serde(deny_unknown_fields)]
#[derive(CalltableToBytes, CalltableFromBytes)]
pub enum TransactionEntryPoint {
    /// The standard `call` entry point used in session code.
    #[calltable(variant_index = 0)]
    Call,
    /// A non-native, arbitrary entry point.
    #[calltable(variant_index = 1)]
    Custom(#[calltable(field_index = 1)] String),
    /// The `transfer` native entry point, used to transfer `Motes` from a source purse to a target
    /// purse.
    ///
    /// Requires the following runtime args:
    ///   * "source": `URef`
    ///   * "target": `URef`
    ///   * "amount": `U512`
    ///
    /// The following optional runtime args can also be provided:
    ///   * "to": `Option<AccountHash>`
    ///   * "id": `Option<u64>`
    #[cfg_attr(
        feature = "json-schema",
        schemars(
            description = "The `transfer` native entry point, used to transfer `Motes` from a \
            source purse to a target purse."
        )
    )]
    #[calltable(variant_index = 2)]
    Transfer,
    /// The `add_bid` native entry point, used to create or top off a bid purse.
    ///
    /// Requires the following runtime args:
    ///   * "public_key": `PublicKey`
    ///   * "delegation_rate": `u8`
    ///   * "amount": `U512`
    ///   * "minimum_delegation_amount": `u64`
    ///   * "maximum_delegation_amount": `u64`
    #[cfg_attr(
        feature = "json-schema",
        schemars(
            description = "The `add_bid` native entry point, used to create or top off a bid purse."
        )
    )]
    #[calltable(variant_index = 3)]
    AddBid,
    /// The `withdraw_bid` native entry point, used to decrease a stake.
    ///
    /// Requires the following runtime args:
    ///   * "public_key": `PublicKey`
    ///   * "amount": `U512`
    #[cfg_attr(
        feature = "json-schema",
        schemars(description = "The `withdraw_bid` native entry point, used to decrease a stake.")
    )]
    #[calltable(variant_index = 4)]
    WithdrawBid,

    /// The `delegate` native entry point, used to add a new delegator or increase an existing
    /// delegator's stake.
    ///
    /// Requires the following runtime args:
    ///   * "delegator": `PublicKey`
    ///   * "validator": `PublicKey`
    ///   * "amount": `U512`
    #[cfg_attr(
        feature = "json-schema",
        schemars(
            description = "The `delegate` native entry point, used to add a new delegator or \
            increase an existing delegator's stake."
        )
    )]
    #[calltable(variant_index = 5)]
    Delegate,

    /// The `undelegate` native entry point, used to reduce a delegator's stake or remove the
    /// delegator if the remaining stake is 0.
    ///
    /// Requires the following runtime args:
    ///   * "delegator": `PublicKey`
    ///   * "validator": `PublicKey`
    ///   * "amount": `U512`
    #[cfg_attr(
        feature = "json-schema",
        schemars(
            description = "The `undelegate` native entry point, used to reduce a delegator's \
            stake or remove the delegator if the remaining stake is 0."
        )
    )]
    #[calltable(variant_index = 6)]
    Undelegate,

    /// The `redelegate` native entry point, used to reduce a delegator's stake or remove the
    /// delegator if the remaining stake is 0, and after the unbonding delay, automatically
    /// delegate to a new validator.
    ///
    /// Requires the following runtime args:
    ///   * "delegator": `PublicKey`
    ///   * "validator": `PublicKey`
    ///   * "amount": `U512`
    ///   * "new_validator": `PublicKey`
    #[cfg_attr(
        feature = "json-schema",
        schemars(
            description = "The `redelegate` native entry point, used to reduce a delegator's stake \
            or remove the delegator if the remaining stake is 0, and after the unbonding delay, \
            automatically delegate to a new validator."
        )
    )]
    #[calltable(variant_index = 7)]
    Redelegate,

    /// The `activate bid` native entry point, used to reactivate an inactive bid.
    ///
    /// Requires the following runtime args:
    ///   * "validator_public_key": `PublicKey`
    #[cfg_attr(
        feature = "json-schema",
        schemars(
            description = "The `activate_bid` native entry point, used to used to reactivate an \
            inactive bid."
        )
    )]
    #[calltable(variant_index = 8)]
    ActivateBid,

    /// The `change_bid_public_key` native entry point, used to change a bid's public key.
    ///
    /// Requires the following runtime args:
    ///   * "public_key": `PublicKey`
    ///   * "new_public_key": `PublicKey`
    #[cfg_attr(
        feature = "json-schema",
        schemars(
            description = "The `change_bid_public_key` native entry point, used to change a bid's public key."
        )
    )]
    #[calltable(variant_index = 9)]
    ChangeBidPublicKey,
}

impl TransactionEntryPoint {
    /// Returns a random `TransactionEntryPoint`.
    #[cfg(any(feature = "testing", test))]
    pub fn random(rng: &mut TestRng) -> Self {
        match rng.gen_range(0..10) {
            0 => TransactionEntryPoint::Custom(rng.random_string(1..21)),
            1 => TransactionEntryPoint::Transfer,
            2 => TransactionEntryPoint::AddBid,
            3 => TransactionEntryPoint::WithdrawBid,
            4 => TransactionEntryPoint::Delegate,
            5 => TransactionEntryPoint::Undelegate,
            6 => TransactionEntryPoint::Redelegate,
            7 => TransactionEntryPoint::ActivateBid,
            8 => TransactionEntryPoint::ChangeBidPublicKey,
            9 => TransactionEntryPoint::Call,
            _ => unreachable!(),
        }
    }

    /// Does this entry point kind require holds epoch?
    pub fn requires_holds_epoch(&self) -> bool {
        match self {
            TransactionEntryPoint::AddBid
            | TransactionEntryPoint::Delegate
            | TransactionEntryPoint::Custom(_)
            | TransactionEntryPoint::Call
            | TransactionEntryPoint::Transfer => true,
            TransactionEntryPoint::WithdrawBid
            | TransactionEntryPoint::Undelegate
            | TransactionEntryPoint::Redelegate
            | TransactionEntryPoint::ActivateBid
            | TransactionEntryPoint::ChangeBidPublicKey => false,
        }
    }
}

impl Display for TransactionEntryPoint {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            TransactionEntryPoint::Call => write!(formatter, "call"),
            TransactionEntryPoint::Custom(entry_point) => {
                write!(formatter, "custom({entry_point})")
            }
            TransactionEntryPoint::Transfer => write!(formatter, "transfer"),
            TransactionEntryPoint::AddBid => write!(formatter, "add_bid"),
            TransactionEntryPoint::WithdrawBid => write!(formatter, "withdraw_bid"),
            TransactionEntryPoint::Delegate => write!(formatter, "delegate"),
            TransactionEntryPoint::Undelegate => write!(formatter, "undelegate"),
            TransactionEntryPoint::Redelegate => write!(formatter, "redelegate"),
            TransactionEntryPoint::ActivateBid => write!(formatter, "activate_bid"),
            TransactionEntryPoint::ChangeBidPublicKey => write!(formatter, "change_bid_public_key"),
        }
    }
}

impl From<&str> for TransactionEntryPoint {
    fn from(value: &str) -> Self {
        if value.to_lowercase() == mint::METHOD_TRANSFER {
            return TransactionEntryPoint::Transfer;
        }
        if value.to_lowercase() == auction::METHOD_ACTIVATE_BID {
            return TransactionEntryPoint::ActivateBid;
        }
        if value.to_lowercase() == auction::METHOD_ADD_BID {
            return TransactionEntryPoint::AddBid;
        }
        if value.to_lowercase() == auction::METHOD_WITHDRAW_BID {
            return TransactionEntryPoint::WithdrawBid;
        }
        if value.to_lowercase() == auction::METHOD_DELEGATE {
            return TransactionEntryPoint::Delegate;
        }
        if value.to_lowercase() == auction::METHOD_UNDELEGATE {
            return TransactionEntryPoint::Undelegate;
        }
        if value.to_lowercase() == auction::METHOD_REDELEGATE {
            return TransactionEntryPoint::Redelegate;
        }
        if value.to_lowercase() == auction::METHOD_CHANGE_BID_PUBLIC_KEY {
            return TransactionEntryPoint::ChangeBidPublicKey;
        }
        TransactionEntryPoint::Custom(value.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{bytesrepr::test_serialization_roundtrip, gens::transaction_entry_point_arb};
    use proptest::prelude::*;

    #[test]
    fn bytesrepr_roundtrip() {
        let rng = &mut TestRng::new();
        for _ in 0..10 {
            test_serialization_roundtrip(&TransactionEntryPoint::random(rng));
        }
    }

    proptest! {
        #[test]
        fn bytesrepr_roundtrip_from_arb(entry_point in transaction_entry_point_arb()) {
            test_serialization_roundtrip(&entry_point);
        }
    }
}
