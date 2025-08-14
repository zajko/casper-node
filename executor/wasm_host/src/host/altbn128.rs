use bn::{AffineG1, FieldError, Fq, Fr, Group, G1};
use casper_types::U256;
use thiserror::Error as ThisError;

/// Errors that can occur when working with alt_bn128 curve.
#[derive(Debug, ThisError, PartialEq, Eq, PartialOrd, Ord)]
pub enum AltBN128Error {
    /// Invalid length.
    #[error("Invalid length")]
    InvalidLength = 1,
    /// Invalid point x coordinate.
    #[error("Invalid point x coordinate")]
    InvalidXCoordinate = 2,
    /// Invalid point y coordinate.
    #[error("Invalid point y coordinate")]
    InvalidYCoordinate = 3,
    /// Invalid point.
    #[error("Invalid point")]
    InvalidPoint = 4,
    /// Invalid A.
    #[error("Invalid A")]
    InvalidA = 5,
    /// Invalid B.
    #[error("Invalid B")]
    InvalidB = 6,
    /// Invalid Ax.
    #[error("Invalid Ax")]
    InvalidAx = 7,
    /// Invalid Ay.
    #[error("Invalid Ay")]
    InvalidAy = 8,
    /// Invalid Bay.
    #[error("Invalid Bay")]
    InvalidBay = 9,
    /// Invalid Bax.
    #[error("Invalid Bax")]
    InvalidBax = 10,
    /// Invalid Bby.
    #[error("Invalid Bby")]
    InvalidBby = 11,
    /// Invalid Bbx.
    #[error("Invalid Bbx")]
    InvalidBbx = 12,
}

pub(super) fn point_from_coords(x: U256, y: U256) -> Result<G1, AltBN128Error> {
    let px = Fq::from_slice(&u256_to_bytes(x)).map_err(|_| AltBN128Error::InvalidXCoordinate)?;
    let py = Fq::from_slice(&u256_to_bytes(y)).map_err(|_| AltBN128Error::InvalidYCoordinate)?;

    Ok(if px == Fq::zero() && py == Fq::zero() {
        G1::zero()
    } else {
        AffineG1::new(px, py)
            .map_err(|_| AltBN128Error::InvalidPoint)?
            .into()
    })
}

fn u256_to_bytes(value: U256) -> [u8; 256] {
    let mut buf = [0u8; 256];
    value.to_little_endian(&mut buf);
    buf
}
