use proc_macro2::TokenStream;
use quote::quote;
use std::collections::BTreeMap;
use syn::{Data, Ident};

use crate::types::{
    build_type_description, validate_enum_variants, validate_struct_fields, EnumVariant,
    FieldDefinitions,
};

pub fn internal_derive_trait(struct_name: &Ident, data: &Data) -> proc_macro::TokenStream {
    let maybe_type_description = build_type_description(data);
    let type_description = match maybe_type_description {
        Err(parsing_error) => panic!(
            "Error while applying derive to {}. {}",
            struct_name, parsing_error
        ),
        Ok(binary_indexes) => binary_indexes,
    };
    let from_bytes_method = match type_description {
        crate::types::TypeDescription::Struct(field_definitions) => {
            if let Err(e) = validate_struct_fields(struct_name.to_string(), &field_definitions) {
                panic!("{}", e)
            }
            generate_from_bytes(&struct_name, &field_definitions)
        }
        crate::types::TypeDescription::Enum(enum_variants) => {
            if let Err(e) = validate_enum_variants(struct_name.to_string(), &enum_variants) {
                panic!("{}", e)
            }
            generate_from_bytes_for_variants(&struct_name, &enum_variants)
        }
    };

    let expanded = quote! {
        impl CalltableFromBytes for #struct_name  {
            #from_bytes_method
        }
        impl crate::bytesrepr::FromBytes for #struct_name {
            fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), crate::bytesrepr::Error> {
                CalltableFromBytes::from_bytes(bytes)
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn generate_variant_code(enum_name: &Ident, variant: &EnumVariant) -> TokenStream {
    let variant_name = &variant.variant_name;
    let field_definitions = &variant.field_definitions;
    let (deserialize_fragment, fields) =
        build_deserialization_for_field_definitions(field_definitions);
    quote! {
        #deserialize_fragment
        if window.is_some() {
            return Err(bytesrepr::Error::Formatting);
        }
        Ok(#enum_name::#variant_name #fields)
    }
}
fn generate_from_bytes_for_variants(
    enum_name: &Ident,
    definitions: &BTreeMap<u8, EnumVariant>,
) -> TokenStream {
    let max_number_of_fields = (definitions.iter().fold(0, |acum, (_, definition)| {
        let cur_size = definition.field_definitions.number_of_fields();
        if cur_size > acum {
            cur_size
        } else {
            acum
        }
    }) + 1) as u32; //+1 for the tag index
    let mut enum_variants = quote!();
    for (key, variant) in definitions.iter() {
        let variant_definition = generate_variant_code(enum_name, variant);
        enum_variants.extend(quote! {
            #key => {
                #variant_definition
            },
        });
    }
    let deserialize_variant_by_variant = quote! {
        let (binary_payload, remainder) = BinaryPayload::from_bytes(#max_number_of_fields, bytes)?;
        let window = binary_payload
        .start_consuming()?
        .ok_or(bytesrepr::Error::Formatting)?;

        window.verify_index(0)?; //Tag of variant is always serialized with index 0
        let (tag, window) = window.deserialize_and_maybe_next::<u8>()?;
        let to_ret = match tag {
            #enum_variants
            _ => Err(bytesrepr::Error::Formatting),
        };
        to_ret.map(|endpoint| (endpoint, remainder))
    };

    quote! {
        fn from_bytes(bytes: &[u8]) -> Result<(#enum_name, &[u8]), crate::bytesrepr::Error> {
            #deserialize_variant_by_variant
        }
    }
}

fn generate_from_bytes(struct_name: &Ident, definitions: &FieldDefinitions) -> TokenStream {
    let (deserialize_field_by_field, new_struct_body) =
        build_deserialization_for_field_definitions(definitions);
    let max_number_of_fields = definitions.number_of_fields() as u32;
    quote! {
            fn from_bytes(bytes: &[u8]) -> Result<(#struct_name, &[u8]), crate::bytesrepr::Error> {
            let (binary_payload, remainder) = crate::transaction::serialization::BinaryPayload::from_bytes(#max_number_of_fields, bytes)?;
            let window = binary_payload
                .start_consuming()?;
            #deserialize_field_by_field
            if window.is_some() {
                return Err(bytesrepr::Error::Formatting);
            }
            let from_bytes = #struct_name #new_struct_body;
            Ok((from_bytes, remainder))
        }
    }
}

fn build_deserialization_for_field_definitions(
    definitions: &FieldDefinitions,
) -> (TokenStream, TokenStream) {
    let mut deserialize_field_by_field = quote!();
    let mut new_struct_body = quote!();
    let mut new_struct_body_inner = quote!();
    match definitions {
        FieldDefinitions::UnnamedFieldDefinitions(fields) => {
            let mut new_struct_body_inner = quote!();
            for (_idx, definition) in fields.iter() {
                let name = &definition.name_stub;
                let ty: &syn::Type = &definition.ty;
                if let Some(index) = definition.index {
                    deserialize_field_by_field.extend(quote! {
                        let window = window.ok_or(bytesrepr::Error::Formatting)?;
                        window.verify_index(#index)?;
                        let (#name, window) = window.deserialize_and_maybe_next::<#ty>()?;
                    });
                    new_struct_body_inner.extend(quote! {
                        #name,
                    });
                } else {
                    new_struct_body_inner.extend(quote! {
                        #ty::default(),
                    });
                }
            }
            new_struct_body = quote! {
                (#new_struct_body_inner)
            };
        }
        FieldDefinitions::NamedFieldDefinitions(fields) => {
            for (_idx, definition) in fields.iter() {
                let name = &definition.name;
                let ty: &syn::Type = &definition.ty;
                if let Some(index) = definition.index {
                    deserialize_field_by_field.extend(quote! {
                        let window = window.ok_or(bytesrepr::Error::Formatting)?;
                        window.verify_index(#index)?;
                        let (#name, window) = window.deserialize_and_maybe_next::<#ty>()?;
                    });
                    new_struct_body_inner.extend(quote! {
                        #name,
                    });
                } else {
                    new_struct_body_inner.extend(quote! {
                        #name: #ty::default(),
                    });
                }
            }
            new_struct_body = quote! {
                {#new_struct_body_inner}
            };
        }
        FieldDefinitions::Unit => {}
    }

    (deserialize_field_by_field, new_struct_body)
}
