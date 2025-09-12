use std::sync::Mutex;

use bytes::Bytes;
use casper_executor_wasm_common::caller::{Caller, MeteringPoints};

struct NativeContext {}
#[derive(Error, Debug)]
enum NativeExecError {
    // ran out of gas during execution
    OutOfGas,
    //attempted to read from or write to memory which was not allocated
    Segfault,
}

/// An implementation of a VM caller to enable native code to execute host-side functions in a
/// semi-stubbed environment. This is NOT designed to work in a production environment and should be
/// ONLY used in tests of smart contracts.
pub(crate) struct NativeCaller {
    context: NativeContext,
    bytecode: Bytes,
    points: u64,
    memory_stub: Mutex<Vec<u8>>,
}

impl Caller for NativeCaller {
    type Context = NativeContext;
    type Error = NativeExecError;

    fn memory_write(&self, offset: u32, data: &[u8]) -> Result<(), Self::Error> {
        //The unwrap here is OK, since if the lock is poisoned we are already
        // beyond recovering and panicing seems reasonable
        let mut guard = self.memory_stub.lock().unwrap();
        let memory_length = guard.len();
        if memory_length < (offset + data.len()) {
            Err(NativeExecError::Segfault)
        } else {
            for (i, b) in data.iter().enumerate() {
                *guard[offset + *i] = *b;
            }
        }
    }

    fn context(&self) -> &Self::Context {
        &self.context
    }

    fn context_mut(&mut self) -> &mut Self::Context {
        &mut self.context
    }

    fn bytecode(&self) -> Bytes {
        self.bytecode.clone()
    }

    fn memory_read(&self, offset: u32, size: usize) -> Result<Vec<u8>, Self::Error> {
        let start = offset;
        let end = offset + size;
        let guard = self.memory_stub.lock().unwrap();
        let memory_length = guard.len();
        if memory_length < (offset + data.len()) {
            Err(NativeExecError::Segfault)
        } else {
            Ok(Vec::from(guard[start..end].clone()))
        }
    }

    fn memory_read_into(&self, offset: u32, output: &mut [u8]) -> Result<(), Self::Error> {
        let buffer_length = output.len();
        let start = offset;
        let end = offset + size;
        let guard = self.memory_stub.lock().unwrap();
        let memory_length = guard.len();
        if memory_length < (offset + data.len()) {
            Err(NativeExecError::Segfault)
        } else {
            let memory_window = guard[start..end];
            for (i, b) in memory_window.iter().enumerate() {
                output[i] = *b;
            }
        }
    }

    fn alloc(&mut self, idx: u32, size: usize, ctx: u32) -> Result<u32, Self::Error> {
        todo!()
    }

    /// Returns the amount of gas remaining.
    #[inline]
    fn get_remaining_points(&mut self) -> Result<MeteringPoints, Self::Error> {
        Ok(MeteringPoints::Remaining(self.points))
    }

    /// Check for exhaustion, then deduct amount from remaining if able.
    ///
    /// This method will cause the VM engine to stop in case remaining gas points are depleted.
    fn consume_gas(&mut self, amount: u64) -> Result<(), Self::Error> {
        match self.get_remaining_points()? {
            MeteringPoints::Remaining(remaining_points) => {
                let remaining_points = remaining_points
                    .checked_sub(amount)
                    .ok_or(NativeExecError::OutOfGas)?;
                self.set_remaining_points(remaining_points)?;
                Ok(())
            }
            MeteringPoints::Exhausted => Err(NativeExecError::OutOfGas),
        }
    }

    #[inline]
    fn has_export(&self, _name: &str) -> Result<bool, NativeExecError> {
        todo!()
    }
}

impl NativeCaller {
    fn set_remaining_points(&mut self, points: u64) -> Result<(), NativeExecError> {
        self.points = points;
        Ok(())
    }
}
