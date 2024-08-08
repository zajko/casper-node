mod calltable_from_bytes;
mod calltable_to_bytes;
mod error;
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

    match calltable_to_bytes::internal_derive_trait(struct_name, &data) {
        Err(parsing_error) => panic!(
            "Error while applying CalltableToBytes derive to {}. {}",
            struct_name, parsing_error
        ),
        Ok(token_stream) => return token_stream,
    }
}

#[proc_macro_derive(CalltableFromBytes, attributes(calltable))]
pub fn generate_from_bytes_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let data: syn::Data = input.data;
    match calltable_from_bytes::internal_derive_trait(struct_name, &data) {
        Err(parsing_error) => panic!(
            "Error while applying CalltableFromBytes derive to {}. {}",
            struct_name, parsing_error
        ),
        Ok(token_stream) => return token_stream,
    }
}
