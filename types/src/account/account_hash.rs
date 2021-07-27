use super::FromStrError;
use crate::{PublicKey, BLAKE2B_DIGEST_LENGTH};
use alloc::{string::String, vec::Vec};
use core::convert::TryFrom;
use datasize::DataSize;
#[cfg(feature = "std")]
use schemars::{gen::SchemaGenerator, schema::Schema, JsonSchema};
use serde::{de::Error as SerdeError, Deserialize, Deserializer, Serialize, Serializer};

/// The length in bytes of a [`AccountHash`].
pub const ACCOUNT_HASH_LENGTH: usize = 32;
/// TODO: Document this.
pub const ACCOUNT_HASH_FORMATTED_STRING_PREFIX: &str = "account-hash-";

/// A type alias for the raw bytes of an Account Hash.
pub type AccountHashBytes = [u8; ACCOUNT_HASH_LENGTH];

/// A newtype wrapping a [`AccountHashBytes`] which is the raw bytes of
/// the AccountHash, a hash of Public Key and Algorithm
#[derive(DataSize, Default, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy)]
pub struct AccountHash(pub AccountHashBytes);

impl AccountHash {
    /// Constructs a new `AccountHash` instance from the raw bytes of an Public Key Account Hash.
    pub const fn new(value: AccountHashBytes) -> AccountHash {
        AccountHash(value)
    }

    /// Returns the raw bytes of the account hash as an array.
    pub fn value(&self) -> AccountHashBytes {
        self.0
    }

    /// Returns the raw bytes of the account hash as a `slice`.
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    /// Formats the `AccountHash` for users getting and putting.
    pub fn to_formatted_string(self) -> String {
        format!(
            "{}{}",
            ACCOUNT_HASH_FORMATTED_STRING_PREFIX,
            base16::encode_lower(&self.0),
        )
    }

    /// Parses a string formatted as per `Self::to_formatted_string()` into an `AccountHash`.
    pub fn from_formatted_str(input: &str) -> Result<Self, FromStrError> {
        let remainder = input
            .strip_prefix(ACCOUNT_HASH_FORMATTED_STRING_PREFIX)
            .ok_or(FromStrError::InvalidPrefix)?;
        let bytes = AccountHashBytes::try_from(base16::decode(remainder)?.as_ref())?;
        Ok(AccountHash(bytes))
    }

    #[doc(hidden)]
    pub fn from_public_key(
        public_key: &PublicKey,
        blake2b_hash_fn: impl Fn(Vec<u8>) -> [u8; BLAKE2B_DIGEST_LENGTH],
    ) -> Self {
        const SYSTEM_LOWERCASE: &str = "system";
        const ED25519_LOWERCASE: &str = "ed25519";
        const SECP256K1_LOWERCASE: &str = "secp256k1";

        let algorithm_name = match public_key {
            PublicKey::System => SYSTEM_LOWERCASE,
            PublicKey::Ed25519(_) => ED25519_LOWERCASE,
            PublicKey::Secp256k1(_) => SECP256K1_LOWERCASE,
        };
        let public_key_bytes: Vec<u8> = public_key.into();

        // Prepare preimage based on the public key parameters.
        let preimage = {
            let mut data = Vec::with_capacity(algorithm_name.len() + public_key_bytes.len() + 1);
            data.extend(algorithm_name.as_bytes());
            data.push(0);
            data.extend(public_key_bytes);
            data
        };
        // Hash the preimage data using blake2b256 and return it.
        let digest = blake2b_hash_fn(preimage);
        Self::new(digest)
    }
}

#[cfg(feature = "std")]
impl JsonSchema for AccountHash {
    fn schema_name() -> String {
        String::from("AccountHash")
    }

    fn json_schema(gen: &mut SchemaGenerator) -> Schema {
        let schema = gen.subschema_for::<String>();
        let mut schema_object = schema.into_object();
        schema_object.metadata().description = Some("Hex-encoded account hash.".to_string());
        schema_object.into()
    }
}

impl Serialize for AccountHash {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if serializer.is_human_readable() {
            self.to_formatted_string().serialize(serializer)
        } else {
            self.0.serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for AccountHash {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        if deserializer.is_human_readable() {
            let formatted_string = String::deserialize(deserializer)?;
            AccountHash::from_formatted_str(&formatted_string).map_err(SerdeError::custom)
        } else {
            let bytes = AccountHashBytes::deserialize(deserializer)?;
            Ok(AccountHash(bytes))
        }
    }
}
