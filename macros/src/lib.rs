mod error;
mod from_bytes;
mod to_bytes;
mod types;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

pub(crate) const CALLTABLE_ATTRIBUTE: &str = "calltable";
pub(crate) const FIELD_INDEX_ATTRIBUTE: &str = "field_index";
pub(crate) const VARIANT_INDEX_ATTRIBUTE: &str = "variant_index";

#[proc_macro_derive(CalltableToBytes, attributes(calltable))]
pub fn generate_to_bytes_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let data: syn::Data = input.data;
    to_bytes::internal_derive_trait(struct_name, &data)
}

#[proc_macro_derive(CalltableFromBytes, attributes(calltable))]
pub fn generate_from_bytes_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let data: syn::Data = input.data;
    from_bytes::internal_derive_trait(struct_name, &data)
}
