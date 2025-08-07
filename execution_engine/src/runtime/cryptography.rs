//! Cryptography module containing hashing functions used internally
//! by the execution engine

use std::fmt::{Display, Formatter, Result as DisplayResult};

use blake2::{
    digest::{Update, VariableOutput},
    Blake2bVar,
};
use casper_wasmi::HostError;
use keccak_asm::Digest as KeccakDigest;
use sha2::Sha256;

/// The number of bytes in a hash.
/// All hash functions in this module have a digest length of 32.
pub const DIGEST_LENGTH: usize = 32;

#[derive(Debug)]
pub enum CryptographyError {
    InvalidOutputSize,
}

impl Display for CryptographyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> DisplayResult {
        match self {
            CryptographyError::InvalidOutputSize => write!(f, "invalid output size"),
        }
    }
}

impl HostError for CryptographyError {}

/// The 32-byte digest blake2b hash function
pub fn blake2b<T: AsRef<[u8]>>(data: T) -> Result<[u8; DIGEST_LENGTH], CryptographyError> {
    let mut result = [0; DIGEST_LENGTH];
    // NOTE: Assumed safe as `BLAKE2B_DIGEST_LENGTH` is a valid value for a hasher
    let mut hasher =
        Blake2bVar::new(DIGEST_LENGTH).map_err(|_| CryptographyError::InvalidOutputSize)?;

    hasher.update(data.as_ref());

    // NOTE: This should never fail, because result is exactly DIGEST_LENGTH long
    hasher.finalize_variable(&mut result).ok();

    result
}

/// The 32-byte digest blake3 hash function
pub fn blake3<T: AsRef<[u8]>>(data: T) -> [u8; DIGEST_LENGTH] {
    let mut result = [0; DIGEST_LENGTH];
    let mut hasher = blake3::Hasher::new();

    hasher.update(data.as_ref());
    let hash = hasher.finalize();
    let hash_bytes: &[u8; DIGEST_LENGTH] = hash.as_bytes();
    result.copy_from_slice(hash_bytes);
    result
}

/// The 32-byte digest sha256 hash function
pub fn sha256<T: AsRef<[u8]>>(data: T) -> [u8; DIGEST_LENGTH] {
    Sha256::digest(data).into()
}

/// The 32-byte digest keccak256 hash function
pub fn keccak256<T: AsRef<[u8]>>(data: T) -> [u8; DIGEST_LENGTH] {
    use keccak_asm::Keccak256;

    let mut h = Keccak256::new();
    KeccakDigest::update(&mut h, &data);
    let mut out = [0u8; 32];
    let result = KeccakDigest::finalize(h);
    out.copy_from_slice(&result);
    out
}
