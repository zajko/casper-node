pub mod transaction_v1;
use alloc::vec::Vec;
pub use transaction_v1::{TransactionV1BinaryToBytes, TransactionV1FromBytes};

use crate::bytesrepr::{
    self, Bytes, Error, FromBytes, ToBytes, U16_SERIALIZED_LENGTH, U32_SERIALIZED_LENGTH,
    U8_SERIALIZED_LENGTH,
};
pub const TRANSACTION_V1_SERIALIZATION_VERSION: u8 = 1;

pub struct Field {
    pub index: u16,
    pub offset: u32,
}

impl ToBytes for Field {
    fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut buffer = bytesrepr::allocate_buffer(self)?;
        self.write_bytes(&mut buffer)?;
        Ok(buffer)
    }

    fn write_bytes(&self, writer: &mut Vec<u8>) -> Result<(), Error> {
        self.index.write_bytes(writer)?;
        self.offset.write_bytes(writer)
    }

    fn serialized_length(&self) -> usize {
        U16_SERIALIZED_LENGTH + U32_SERIALIZED_LENGTH
    }
}

impl FromBytes for Field {
    fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), Error> {
        let (index, remainder) = u16::from_bytes(bytes)?;
        let (offset, remainder) = u32::from_bytes(remainder)?;
        Ok((Field { index, offset }, remainder))
    }
}

impl Field {
    pub fn serialized_vec_size(number_of_fields: usize) -> usize {
        let mut size = U32_SERIALIZED_LENGTH; // Overhead of the vec itself
        size += number_of_fields * Field::serialized_length();
        size
    }

    pub fn serialized_length() -> usize {
        U16_SERIALIZED_LENGTH + U32_SERIALIZED_LENGTH
    }
}

pub trait CalltableToBytes {
    fn serialized_field_lengths(&self) -> Vec<usize>;
    fn serialize(&self) -> Result<Vec<u8>, crate::bytesrepr::Error>;
}

pub trait CalltableFromBytes: Sized {
    fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), Error>;
}

pub struct BinaryPayloadBuilder {
    fields: Vec<Field>,
    expected_payload_sizes: Vec<usize>,
    bytes: Vec<u8>,
    current_field_index: usize,
    current_offset: usize,
    written_bytes: usize,
}

impl BinaryPayloadBuilder {
    pub fn new(expected_payload_sizes: Vec<usize>) -> Result<BinaryPayloadBuilder, Error> {
        let number_of_fields = expected_payload_sizes.len();
        let fields_size = Field::serialized_vec_size(number_of_fields);
        let bytes_of_payload_size = expected_payload_sizes.iter().sum::<usize>();
        let payload_and_vec_overhead: usize = U32_SERIALIZED_LENGTH + bytes_of_payload_size; // u32 for the overhead of serializing a vec
        let mut payload_buffer =
            bytesrepr::allocate_buffer_for_size(fields_size + payload_and_vec_overhead)?;
        payload_buffer.extend(vec![0; fields_size]); // Making room for the call table
        payload_buffer.extend((bytes_of_payload_size as u32).to_bytes()?); // Writing down number of bytes that are in the payload
        Ok(BinaryPayloadBuilder {
            fields: Vec::with_capacity(number_of_fields),
            expected_payload_sizes,
            bytes: payload_buffer,
            current_field_index: 0,
            current_offset: 0,
            written_bytes: (fields_size + U32_SERIALIZED_LENGTH) as usize,
        })
    }

    pub fn add_field<T: ToBytes + ?Sized>(
        mut self,
        field_index: u16,
        value: &T,
    ) -> Result<Self, Error> {
        let current_field_index = self.current_field_index;
        if current_field_index >= self.expected_payload_sizes.len() {
            //We wrote more fields than expected
            return Err(Error::NotRepresentable);
        }
        let fields = &mut self.fields;
        if current_field_index > 0 && fields[current_field_index - 1].index >= field_index {
            //Need to make sure we write fields in ascending order of tab index
            return Err(Error::NotRepresentable);
        }
        let size = self.expected_payload_sizes[current_field_index];
        value.write_bytes(&mut self.bytes)?;
        fields.push(Field {
            index: field_index,
            offset: self.current_offset as u32,
        });
        self.current_field_index += 1;
        self.current_offset += size;
        self.written_bytes += size;
        Ok(self)
    }

    pub fn to_binary_payload_bytes(mut self) -> Result<Vec<u8>, Error> {
        if self.current_field_index != self.expected_payload_sizes.len() {
            //We didn't write all the fields we expected
            return Err(Error::NotRepresentable);
        }
        let write_at_slice = &mut self.bytes[0..];
        let calltable_bytes = self.fields.to_bytes()?;
        for (pos, byte) in calltable_bytes.into_iter().enumerate() {
            write_at_slice[pos] = byte;
        }
        Ok(self.bytes)
    }
}

pub struct BinaryPayload {
    fields: Vec<Field>,
    bytes: Bytes,
}

impl BinaryPayload {
    pub fn estimate_size(field_sizes: Vec<usize>) -> usize {
        let number_of_fields = field_sizes.len();
        let payload_in_bytes: usize = field_sizes.iter().sum();
        let mut size = U32_SERIALIZED_LENGTH + U32_SERIALIZED_LENGTH; // Overhead of the fields vec and bytes vec
        size += number_of_fields * Field::serialized_length();
        size += payload_in_bytes * U8_SERIALIZED_LENGTH;
        size
    }

    pub fn start_consuming(&self) -> Result<Option<BinaryPayloadWindow>, Error> {
        if self.fields.is_empty() {
            return Ok(None);
        }
        let field = &self.fields[0];
        let expected_size = if self.fields.len() == 1 {
            self.bytes.len()
        } else {
            self.fields[1].offset as usize
        };
        Ok(Some(BinaryPayloadWindow {
            index_in_fields_vec: 0,
            expected_size,
            field,
            bytes: &self.bytes,
            parent: &self,
        }))
    }

    pub fn from_bytes(
        max_expected_fields: u32,
        input_bytes: &[u8],
    ) -> Result<(BinaryPayload, &[u8]), Error> {
        if input_bytes.len() < U32_SERIALIZED_LENGTH {
            //The first "thing" in the bytes of the payload should be a `fields` vector. We want to
            // check the number of entries in that vector to avoid field pumping. If the
            // payload doesn't have u32 size of bytes in it, then it's malformed.
            return Err(Error::Formatting);
        }
        let (number_of_fields, _) = u32::from_bytes(input_bytes)?;
        if number_of_fields > max_expected_fields {
            return Err(Error::Formatting);
        }
        let (fields, remainder) = Vec::<Field>::from_bytes(input_bytes)?;
        let (bytes, remainder) = Bytes::from_bytes(remainder)?;
        Ok((BinaryPayload { fields, bytes }, remainder))
    }
}

pub struct BinaryPayloadWindow<'a> {
    index_in_fields_vec: usize,
    expected_size: usize,
    field: &'a Field,
    bytes: &'a [u8],
    parent: &'a BinaryPayload,
}

impl<'a> BinaryPayloadWindow<'a> {
    pub fn verify_index(&self, expected_index: u16) -> Result<(), Error> {
        let field = self.field;
        if field.index != expected_index {
            return Err(Error::Formatting);
        }
        Ok(())
    }
    pub fn deserialize_and_next<T: FromBytes>(&self) -> Result<(T, BinaryPayloadWindow), Error> {
        let (t, maybe_window) = self.step()?;
        let window = maybe_window.ok_or(Error::Formatting)?;
        Ok((t, window))
    }

    pub fn deserialize_and_maybe_next<T: FromBytes>(
        &self,
    ) -> Result<(T, Option<BinaryPayloadWindow>), Error> {
        let (t, maybe_window) = self.step()?;
        Ok((t, maybe_window))
    }

    pub fn deserialize_and_stop<T: FromBytes>(&self) -> Result<T, Error> {
        let (t, maybe_window) = self.step()?;
        if maybe_window.is_some() {
            return Err(Error::Formatting);
        }
        Ok(t)
    }
    fn step<T: FromBytes>(&self) -> Result<(T, Option<BinaryPayloadWindow>), Error> {
        let (t, remainder) = T::from_bytes(self.bytes)?;
        let parent_fields = &self.parent.fields;
        let parent_fields_len = parent_fields.len();
        let is_last_field = self.index_in_fields_vec == parent_fields_len - 1;
        if remainder.len() + self.expected_size != self.bytes.len() {
            //The field occupied different amount of bytes than expected
            return Err(Error::Formatting);
        }
        if !is_last_field {
            let next_field_index = self.index_in_fields_vec + 1;
            let next_field = &parent_fields[next_field_index]; // We already checked that this index exists
            let is_next_field_last = next_field_index == parent_fields_len - 1;
            let expected_size = if is_next_field_last {
                remainder.len()
            } else {
                (parent_fields[next_field_index + 1].offset
                    - parent_fields[next_field_index].offset) as usize
            };
            let next_window = BinaryPayloadWindow {
                index_in_fields_vec: next_field_index,
                expected_size,
                field: next_field,
                bytes: remainder,
                parent: self.parent,
            };
            Ok((t, Some(next_window)))
        } else {
            if !remainder.is_empty() {
                //The payload of BinaryPayload should contain only the serialized, there should be
                // no trailing bytes after consuming all the fields.
                return Err(Error::Formatting);
            }
            Ok((t, None))
        }
    }
}
