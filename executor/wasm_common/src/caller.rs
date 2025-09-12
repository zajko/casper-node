use bytes::Bytes;
use thiserror::Error;

use crate::flags::ReturnFlags;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MeteringPoints {
    Remaining(u64),
    Exhausted,
}

impl MeteringPoints {
    pub fn try_into_remaining(self) -> Result<u64, Self> {
        if let Self::Remaining(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

/// An abstraction over the 'caller' object of a host function that works for any VM.
///
/// This allows access for important instances such as the context object that was passed to the
/// instance, wasm linear memory access, etc.
pub trait Caller {
    type Context;
    type Error;

    fn context(&self) -> &Self::Context;
    fn context_mut(&mut self) -> &mut Self::Context;
    /// Returns currently running *unmodified* bytecode.
    fn bytecode(&self) -> Bytes;

    /// Check if an export is present in the module.
    fn has_export(&self, name: &str) -> Result<bool, Self::Error>;

    fn memory_read(&self, offset: u32, size: usize) -> Result<Vec<u8>, Self::Error>;
    fn memory_read_into(&self, offset: u32, output: &mut [u8]) -> Result<(), Self::Error>;
    fn memory_write(&self, offset: u32, data: &[u8]) -> Result<(), Self::Error>;
    /// Allocates memory inside the Wasm VM by calling an export.
    ///
    /// Error is a type-erased error coming from the VM itself.
    fn alloc(&mut self, idx: u32, size: usize, ctx: u32) -> Result<u32, Self::Error>;
    /// Returns the amount of gas remaining.
    fn get_remaining_points(&mut self) -> Result<MeteringPoints, Self::Error>;
    /// Check for gas exhaustion, then reduce remaining by amount if able.
    fn consume_gas(&mut self, value: u64) -> Result<(), Self::Error>;
}
