use proc_macro2::TokenStream;
use quote::quote;
use std::collections::BTreeMap;
use syn::{Data, Ident};

use crate::{
    error::ParsingError,
    types::{
        build_type_description, validate_enum_variants, validate_struct_fields, EnumVariant,
        FieldDefinitions,
    },
};

pub fn internal_derive_trait(
    struct_name: &Ident,
    data: &Data,
) -> Result<proc_macro::TokenStream, ParsingError> {
    let type_description = build_type_description(data)?;

    let (serialization_length_method, serialization_method) = match type_description {
        crate::types::TypeDescription::Struct(field_definitions) => {
            if let Err(e) = validate_struct_fields(struct_name.to_string(), &field_definitions) {
                panic!("{}", e)
            }
            (
                generate_serialized_field_lengths(&field_definitions),
                generate_serialize(&field_definitions),
            )
        }
        crate::types::TypeDescription::Enum(enum_variants) => {
            if let Err(e) = validate_enum_variants(struct_name.to_string(), &enum_variants) {
                panic!("{}", e)
            }
            (
                generate_serialized_field_lengths_for_enum(struct_name, &enum_variants),
                generate_serialize_for_enum(struct_name, &enum_variants),
            )
        }
    };

    let expanded = quote! {
        impl CalltableToBytes for #struct_name  {
            #serialization_length_method
            #serialization_method
        }
        impl crate::bytesrepr::ToBytes for #struct_name {
            fn to_bytes(&self) -> Result<Vec<u8>, crate::bytesrepr::Error> {
                CalltableToBytes::serialize(self)
            }

            fn serialized_length(&self) -> usize {
                BinaryPayload::estimate_size(self.serialized_field_lengths())
            }
        }
    };

    Ok(proc_macro::TokenStream::from(expanded))
}

fn destructure_arg_list(field_definitions: &FieldDefinitions) -> TokenStream {
    match field_definitions {
        FieldDefinitions::Unnamed(fields) => {
            let mut destructured = quote!();
            for (_position, field_definition) in fields.iter() {
                let field_name = &field_definition.name_stub;
                destructured.extend(quote! {
                    #field_name,
                });
            }
            quote! {
                (#destructured)
            }
        }
        FieldDefinitions::Named(fields) => {
            let mut destructured = quote!();
            for (_position, field_definition) in fields.iter() {
                let field_name = &field_definition.name;
                destructured.extend(quote! {
                    #field_name,
                });
            }
            quote! {
                {#destructured}
            }
        }
        FieldDefinitions::Unit => quote!(),
    }
}

fn generate_serialized_field_lengths_for_enum(
    enum_name: &Ident,
    variant_definitions: &BTreeMap<u8, EnumVariant>,
) -> TokenStream {
    let mut variants = quote!();
    for (_, enum_variant_definition) in variant_definitions.iter() {
        let field_definitions = &enum_variant_definition.field_definitions;
        let mut field_list = quote!();
        let variant_args = destructure_arg_list(field_definitions);

        match field_definitions {
            FieldDefinitions::Unnamed(fields) => {
                for (_position, field_definition) in fields.iter() {
                    if field_definition.index.is_some() {
                        let field_name = &field_definition.name_stub;
                        field_list.extend(quote! {
                            #field_name.serialized_length(),
                        });
                    }
                }
            }
            FieldDefinitions::Named(fields) => {
                for (_position, field_definition) in fields.iter() {
                    if field_definition.index.is_some() {
                        let field_name = &field_definition.name;
                        field_list.extend(quote! {
                            #field_name.serialized_length(),
                        });
                    }
                }
            }
            FieldDefinitions::Unit => {}
        }
        let variant_name = &enum_variant_definition.variant_name;
        variants.extend(quote! {
            #enum_name::#variant_name #variant_args => {
                vec![crate::bytesrepr::U8_SERIALIZED_LENGTH, #field_list]
            },
        });
    }
    quote! {
        fn serialized_field_lengths(&self) -> Vec<usize> {
            match self {
                #variants
            }
        }
    }
}

fn generate_serialize_for_enum(
    enum_name: &Ident,
    definitions: &BTreeMap<u8, EnumVariant>,
) -> TokenStream {
    let mut variants = quote!();
    for (_, definition) in definitions.iter() {
        let field_definitions = &definition.field_definitions;
        let variant_args = destructure_arg_list(field_definitions);
        let mut serialize_field_by_field = quote!();
        match field_definitions {
            FieldDefinitions::Unnamed(fields) => {
                for (_position, field_definition) in fields.iter() {
                    if let Some(index) = &field_definition.index {
                        let field_name = &field_definition.name_stub;
                        serialize_field_by_field.extend(quote! {
                            .add_field(#index, &#field_name)?
                        });
                    }
                }
            }
            FieldDefinitions::Named(fields) => {
                for (_position, field_definition) in fields.iter() {
                    if let Some(index) = &field_definition.index {
                        let field_name = &field_definition.name;
                        serialize_field_by_field.extend(quote! {
                            .add_field(#index, &#field_name)?
                        });
                    }
                }
            }
            FieldDefinitions::Unit => {}
        }
        let variant_index = definition.variant_index as u8;
        let variant_name = &definition.variant_name;
        variants.extend(quote! {
            #enum_name::#variant_name #variant_args => {
                crate::transaction::serialization::BinaryPayloadBuilder::new(self.serialized_field_lengths())?
                .add_field(0, &#variant_index)?
                #serialize_field_by_field
                .binary_payload_bytes()
            },
        });
    }
    quote! {
        fn serialize(&self) -> Result<Vec<u8>, crate::bytesrepr::Error> {
            match self {
                #variants
            }
        }
    }
}

fn generate_serialize(definitions: &FieldDefinitions) -> TokenStream {
    let mut serialize_field_by_field = quote!();
    match definitions {
        FieldDefinitions::Unnamed(fields) => {
            for (position, definition) in fields.iter() {
                if let Some(index) = &definition.index {
                    let name = format!("{}", position);
                    serialize_field_by_field.extend(quote! {
                        .add_field(#index, &self.#name)?
                    });
                }
            }
        }
        FieldDefinitions::Named(fields) => {
            for (_position, definition) in fields.iter() {
                if let Some(index) = &definition.index {
                    let name = &definition.name;
                    serialize_field_by_field.extend(quote! {
                        .add_field(#index, &self.#name)?
                    });
                }
            }
        }
        FieldDefinitions::Unit => todo!(),
    }
    quote! {
        fn serialize(&self) -> Result<Vec<u8>, crate::bytesrepr::Error> {
            crate::transaction::serialization::BinaryPayloadBuilder::new(self.serialized_field_lengths())?
            #serialize_field_by_field
            .binary_payload_bytes()
        }
    }
}

fn generate_serialized_field_lengths(definitions: &FieldDefinitions) -> TokenStream {
    let mut serialized_field_lengths = quote!();
    match definitions {
        FieldDefinitions::Unnamed(fields) => {
            for (idx, _) in fields.iter() {
                let name = format!("{}", idx);
                serialized_field_lengths.extend(quote! {
                    self.#name.serialized_length(),
                });
            }
        }
        FieldDefinitions::Named(fields) => {
            for (_, definition) in fields.iter() {
                let name = &definition.name;
                serialized_field_lengths.extend(quote! {
                    self.#name.serialized_length(),
                });
            }
        }
        FieldDefinitions::Unit => {}
    }
    quote! {
        fn serialized_field_lengths(&self) -> Vec<usize> {
            vec![#serialized_field_lengths]
        }
    }
}
