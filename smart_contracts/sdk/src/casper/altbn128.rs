//! Host-optimized support for pairing cryptography with the Barreto-Naehrig curve
use core::array::TryFromSliceError;

use borsh::{BorshDeserialize, BorshSerialize};

use crate::{
    casper::{self, casper_system},
    types::{CryptoFunctionOption, U256},
};

/// A point on the alt_bn128 curve.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, BorshSerialize, BorshDeserialize)]
#[repr(C, packed)]
pub struct G1([u8; 32]);

impl G1 {
    pub const SIZE_IN_BYTES: u32 = core::mem::size_of::<G1>() as u32;
    /// Returns a point with all bytes set to zero.
    pub const fn zero() -> Self {
        G1([0; 32])
    }

    /// Returns the inner byte array.
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

impl From<[u8; 32]> for G1 {
    fn from(value: [u8; 32]) -> Self {
        G1(value)
    }
}

impl From<U256> for G1 {
    fn from(value: U256) -> Self {
        let bytes = u256_to_le_bytes(value);
        G1(bytes)
    }
}

/// A scalar on the alt_bn128 curve.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, BorshSerialize, BorshDeserialize)]
#[repr(C, packed)]
pub struct Fr([u8; 32]);

impl Fr {
    pub const SIZE_IN_BYTES: u32 = core::mem::size_of::<Fr>() as u32;

    /// Returns a point with all bytes set to zero.
    pub const fn zero() -> Self {
        Fr([0; 32])
    }

    /// Returns the inner byte array.
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

impl From<U256> for Fr {
    fn from(value: U256) -> Self {
        let bytes = u256_to_le_bytes(value);
        Self(bytes)
    }
}

impl From<[u8; 32]> for Fr {
    fn from(value: [u8; 32]) -> Self {
        Fr(value)
    }
}

/// A field element on the alt_bn128 curve.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, BorshSerialize, BorshDeserialize)]
#[repr(C, packed)]
pub struct Fq([u8; 32]);

impl Fq {
    /// Returns a point with all bytes set to zero.
    pub const fn zero() -> Self {
        Fq([0; 32])
    }

    /// Returns the inner byte array.
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

impl From<U256> for Fq {
    fn from(value: U256) -> Self {
        let bytes = u256_to_le_bytes(value);
        Self(bytes)
    }
}

impl TryFrom<&[u8]> for Fq {
    type Error = TryFromSliceError;

    fn try_from(value: &[u8]) -> core::result::Result<Self, Self::Error> {
        Ok(Fq(value.try_into()?))
    }
}

impl From<[u8; 32]> for Fq {
    fn from(value: [u8; 32]) -> Self {
        Fq(value)
    }
}

/// Error type for the alt_bn128 module.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, BorshDeserialize, BorshSerialize)]
#[borsh(crate = "crate::serializers::borsh", use_discriminant = true)]
#[repr(u32)]
pub enum AltBn128Error {
    /// Invalid length.
    InvalidLength = 1,
    /// Invalid point x coordinate.
    InvalidXCoordinate = 2,
    /// Invalid point y coordinate.
    InvalidYCoordinate = 3,
    /// Invalid point.
    InvalidPoint = 4,
    /// Invalid A.
    InvalidA = 5,
    /// Invalid B.
    InvalidB = 6,
    /// Invalid Ax.
    InvalidAx = 7,
    /// Invalid Ay.
    InvalidAy = 8,
    /// Invalid Bay.
    InvalidBay = 9,
    /// Invalid Bax.
    InvalidBax = 10,
    /// Invalid Bby.
    InvalidBby = 11,
    /// Invalid Bbx.
    InvalidBbx = 12,
    /// No return value or error
    NoValueNorError = 13,
    /// Call error
    CallError = 14,
    /// Couldnt deserialize return value
    ReturnNotDeserializable = 15,
    /// Unknown error.
    Unknown(u32),
}

impl From<AltBn128Error> for u32 {
    fn from(value: AltBn128Error) -> Self {
        match value {
            AltBn128Error::InvalidLength => 1,
            AltBn128Error::InvalidXCoordinate => 2,
            AltBn128Error::InvalidYCoordinate => 3,
            AltBn128Error::InvalidPoint => 4,
            AltBn128Error::InvalidA => 5,
            AltBn128Error::InvalidB => 6,
            AltBn128Error::InvalidAx => 7,
            AltBn128Error::InvalidAy => 8,
            AltBn128Error::InvalidBay => 9,
            AltBn128Error::InvalidBax => 10,
            AltBn128Error::InvalidBby => 11,
            AltBn128Error::InvalidBbx => 12,
            AltBn128Error::NoValueNorError => 13,
            AltBn128Error::CallError => 14,
            AltBn128Error::ReturnNotDeserializable => 15,
            AltBn128Error::Unknown(catch_all) => catch_all,
        }
    }
}
impl From<u32> for AltBn128Error {
    fn from(value: u32) -> Self {
        match value {
            1 => AltBn128Error::InvalidLength,
            2 => AltBn128Error::InvalidXCoordinate,
            3 => AltBn128Error::InvalidYCoordinate,
            4 => AltBn128Error::InvalidPoint,
            5 => AltBn128Error::InvalidA,
            6 => AltBn128Error::InvalidB,
            7 => AltBn128Error::InvalidAx,
            8 => AltBn128Error::InvalidAy,
            9 => AltBn128Error::InvalidBay,
            10 => AltBn128Error::InvalidBax,
            11 => AltBn128Error::InvalidBby,
            12 => AltBn128Error::InvalidBbx,
            13 => AltBn128Error::NoValueNorError,
            14 => AltBn128Error::CallError,
            15 => AltBn128Error::ReturnNotDeserializable,
            value => AltBn128Error::Unknown(value),
        }
    }
}

/// Result type for the alt_bn128 module.
pub type Result<T> = core::result::Result<T, AltBn128Error>;

/// Adds two points on the alt_bn128 curve.
pub fn alt_bn128_add(x1: &G1, y1: &G1, x2: &G1, y2: &G1) -> Result<(Fq, Fq)> {
    casper::print("q1");
    let input = borsh::to_vec(&(x1, y1, x2, y2)).expect("Serialization to succeed");
    casper::print(&format!("q2 {}", input.len()));
    casper::print(&format!("q2-1 {:?}", input));
    let option = CryptoFunctionOption::AltBn128Add;
    casper::print("q3");
    let (output, result) = casper_system(option.into(), &input);
    casper::print("q4");
    let _ = result.map_err(|_err| AltBn128Error::CallError)?;
    casper::print("q5");
    match output {
        Some(raw) => {
            casper::print("q6");
            let (x, y): (U256, U256) =
                borsh::from_slice(&raw).map_err(|_err| AltBn128Error::ReturnNotDeserializable)?;
            Ok((Fq(u256_to_le_bytes(x)), Fq(u256_to_le_bytes(y))))
        }
        None => {
            casper::print("q7");
            Err(AltBn128Error::NoValueNorError)
        }
    }
}

/// Multiplies a point on the alt_bn128 curve by a scalar.
pub fn alt_bn128_mul(x: &G1, y: &G1, scalar: &Fr) -> Result<(Fq, Fq)> {
    let input = borsh::to_vec(&(x, y, scalar)).expect("Serialization to succeed");
    let option = CryptoFunctionOption::AltBn128Multiply;

    let (output, result) = casper_system(option.into(), &input);
    let _ = result.map_err(|_err| AltBn128Error::CallError)?;
    match output {
        Some(raw) => {
            let val: Result<(U256, U256)> = borsh::from_slice(&raw).unwrap();
            val.map(|(x, y)| (Fq(u256_to_le_bytes(x)), Fq(u256_to_le_bytes(y))))
        }
        None => Err(AltBn128Error::NoValueNorError),
    }
}

/// A pairing of points on the alt_bn128 curve.
#[derive(Copy, Clone, BorshSerialize, BorshDeserialize)]
#[repr(C, packed)]
pub struct Pair {
    /// G1 point
    pub ax: Fq,
    /// G1 point
    pub ay: Fq,
    /// G2 point
    pub bax: Fq,
    /// G2 point
    pub bay: Fq,
    /// G1 point
    pub bbx: Fq,
    /// G1 point
    pub bby: Fq,
}

const _: () = assert!(
    core::mem::size_of::<Pair>() == 192,
    "Pair size is not correct",
);

/// Performs a pairing of points on the alt_bn128 curve.
pub fn alt_bn128_pairing(points: &[Pair]) -> Result<bool> {
    let input = borsh::to_vec(points).expect("Serialization to succeed");
    let option = CryptoFunctionOption::AltBn128Pairing;

    let (output, result) = casper_system(option.into(), &input);
    let _ = result.map_err(|_err| AltBn128Error::CallError)?;
    match output {
        Some(raw) => {
            let val: Result<bool> = borsh::from_slice(&raw).unwrap();
            val
        }
        None => Err(AltBn128Error::NoValueNorError),
    }
}

fn u256_to_le_bytes(value: U256) -> [u8; 32] {
    // For some reason bnum::to_le_bytes is behind "nightly" flag,
    // this is a helper method o achieve the same functionality.
    // Should be replaced with bnum functionality once it evolves past nightly
    let digits = value.digits();
    let mut bytes = [0; 32];
    let mut i = 0;
    while i < 4 {
        let digit_bytes = digits[i].to_le_bytes();
        let mut j = 0;
        while j < 8 {
            bytes[(i << 3) + j] = digit_bytes[j];
            j += 1;
        }
        i += 1;
    }
    bytes
}

#[cfg(test)]
mod tests {
    use crate::casper::altbn128::u256_to_le_bytes;
    use bnum::types::U256;
    use casper_types::{testing::TestRng, U256 as CasperU256};
    use rand::Rng;

    #[test]
    fn test_u256_to_le_bytes() {
        let mut test_rng = TestRng::new();
        for _ in 0..50 {
            let u: CasperU256 = test_rng.gen();
            let base_10_str = format!("{}", u);
            let mut expected_le_bytes = [0; 32];
            u.to_little_endian(&mut expected_le_bytes);
            let u256 = U256::parse_str_radix(&base_10_str, 10);
            let bytes = u256_to_le_bytes(u256);
            assert_eq!(bytes, expected_le_bytes);
        }
    }
}
