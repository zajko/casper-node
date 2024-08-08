use proc_macro2::Span;
use quote::ToTokens;
use std::{collections::BTreeMap, u16};
use syn::{punctuated::Punctuated, Attribute, Fields, Ident, Meta, Token, Type};

use crate::{
    error::ParsingError, CALLTABLE_ATTRIBUTE, FIELD_INDEX_ATTRIBUTE, VARIANT_INDEX_ATTRIBUTE,
};
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum IndexDataOfField {
    BinaryIndex(u16),
    SkipBinarySerialization,
    NoIndexData,
}
pub(crate) enum FieldDefinitions {
    Unnamed(BTreeMap<u16, UnnamedFieldDefinition>),
    Named(BTreeMap<u16, NamedFieldDefinition>),
    Unit,
}

impl FieldDefinitions {
    pub(crate) fn number_of_fields(&self) -> usize {
        match self {
            FieldDefinitions::Unnamed(fields) => fields.len(),
            FieldDefinitions::Named(fields) => fields.len(),
            FieldDefinitions::Unit => 0,
        }
    }
    pub(crate) fn indexes(&self) -> Vec<u16> {
        let mut to_ret = match self {
            FieldDefinitions::Unnamed(fields) => fields
                .iter()
                .filter(|(_, v)| v.index.is_some())
                .map(|(_, v)| v.index.unwrap())
                .collect(),
            FieldDefinitions::Named(fields) => fields
                .iter()
                .filter(|(_, v)| v.index.is_some())
                .map(|(_, v)| v.index.unwrap())
                .collect(),
            FieldDefinitions::Unit => Vec::new(),
        };
        to_ret.sort();
        to_ret
    }
}
pub(crate) struct EnumVariant {
    pub(crate) variant_name: Ident,
    pub(crate) variant_index: u16,
    pub(crate) field_definitions: FieldDefinitions,
}

pub(crate) enum TypeDescription {
    Struct(FieldDefinitions),
    Enum(BTreeMap<u8, EnumVariant>),
}

pub(crate) struct UnnamedFieldDefinition {
    pub(crate) index: Option<u16>, //This will be None if the field is skipped
    pub(crate) ty: Type,
    pub(crate) name_stub: Ident,
}

pub(crate) struct NamedFieldDefinition {
    pub(crate) name: Ident,
    pub(crate) index: Option<u16>, //This will be None if the field is skipped
    pub(crate) ty: Type,
}

pub(crate) fn build_type_description(data: &syn::Data) -> Result<TypeDescription, ParsingError> {
    match data {
        syn::Data::Struct(syn::DataStruct {
            struct_token: _,
            fields,
            semi_token: _,
        }) => Ok(TypeDescription::Struct(build_index_map(fields)?)),
        syn::Data::Enum(enum_data) => {
            let mut variants = BTreeMap::new();
            for variant in enum_data.variants.iter() {
                let variant_name = variant.ident.clone();
                let maybe_index_data = get_index_attribute(
                    VARIANT_INDEX_ATTRIBUTE,
                    &variant.attrs,
                    variant_name.to_string(),
                )?;

                match maybe_index_data {
                    IndexDataOfField::SkipBinarySerialization => {
                        return Err(ParsingError::SkipNotAllowedForEnum {
                            variant_name: variant_name.to_string(),
                        })
                    }
                    IndexDataOfField::NoIndexData => {
                        return Err(ParsingError::BinaryAttributeMissing {
                            field_name: variant_name.to_string(),
                        })
                    }
                    IndexDataOfField::BinaryIndex(idx) => {
                        variants.insert(
                            idx as u8,
                            EnumVariant {
                                variant_name,
                                variant_index: idx,
                                field_definitions: build_index_map(&variant.fields)?,
                            },
                        );
                    }
                }
            }
            Ok(TypeDescription::Enum(variants))
        }
        syn::Data::Union(_) => Err(ParsingError::UnsupportedDataType {
            data_type: "Union".to_string(),
        }),
    }
}

fn build_index_map(fields: &Fields) -> Result<FieldDefinitions, ParsingError> {
    let mut field_index = 0;
    match fields {
        syn::Fields::Named(named) => {
            let mut binary_indexes: BTreeMap<u16, NamedFieldDefinition> = BTreeMap::new();
            for field in named.named.iter() {
                let name = field.ident.as_ref().unwrap();
                let ty = &field.ty;
                let attrs = &field.attrs;
                let field_definition = build_field_definition(attrs, Some(name), ty, field_index)?;
                if let Some(idx) = field_definition.index {
                    if binary_indexes
                        .iter()
                        .any(|(_, v)| v.index.is_some() && v.index.unwrap() == idx)
                    {
                        return Err(ParsingError::MultipleIndexUsages {
                            field_name: name.to_string(),
                            index: field_definition.index.unwrap(),
                        });
                    }
                }
                binary_indexes.insert(field_index, field_definition);
                field_index += 1;
            }
            Ok(FieldDefinitions::Named(binary_indexes))
        }
        syn::Fields::Unnamed(unnamed) => {
            let mut binary_indexes: BTreeMap<u16, UnnamedFieldDefinition> = BTreeMap::new();
            for field in unnamed.unnamed.iter() {
                let ty = &field.ty;
                let attrs = &field.attrs;
                let field_definition = build_field_definition(attrs, None, ty, field_index)?;
                if let Some(idx) = field_definition.index {
                    if binary_indexes
                        .iter()
                        .any(|(_, v)| v.index.is_some() && v.index.unwrap() == idx)
                    {
                        return Err(ParsingError::MultipleIndexUsages {
                            field_name: field_definition.name.to_string(),
                            index: field_definition.index.unwrap(),
                        });
                    }
                }
                binary_indexes.insert(
                    field_index,
                    UnnamedFieldDefinition {
                        index: field_definition.index,
                        name_stub: field_definition.name,
                        ty: field_definition.ty,
                    },
                );
                field_index += 1;
            }
            Ok(FieldDefinitions::Unnamed(binary_indexes))
        }
        syn::Fields::Unit => Ok(FieldDefinitions::Unit),
    }
}

fn build_field_definition(
    attrs: &[Attribute],
    name: Option<&Ident>,
    ty: &Type,
    field_position: u16,
) -> Result<NamedFieldDefinition, ParsingError> {
    let field_ident = field_name_or_field_position(name, field_position);
    let field_name = field_ident.to_string();
    let index = get_index_attribute(FIELD_INDEX_ATTRIBUTE, attrs, field_name.clone())?;

    match index {
        IndexDataOfField::BinaryIndex(idx) => Ok(NamedFieldDefinition {
            name: field_ident,
            index: Some(idx),
            ty: ty.clone(),
        }),
        IndexDataOfField::SkipBinarySerialization => {
            //We need to keep data about skipped fields, because for unnamed fields we need to keep
            // the order
            Ok(NamedFieldDefinition {
                name: field_ident,
                index: None,
                ty: ty.clone(),
            })
        }
        IndexDataOfField::NoIndexData => Err(ParsingError::BinaryAttributeMissing { field_name }),
    }
}

fn get_index_attribute(
    attribute_name: &str,
    attrs: &[Attribute],
    name: String,
) -> Result<IndexDataOfField, ParsingError> {
    let mut index: Option<IndexDataOfField> = None;
    for attr in attrs.iter() {
        match parse_single_attribute(attr, attribute_name, &name)? {
            IndexDataOfField::BinaryIndex(_) if index.is_some() => {
                return Err(ParsingError::MultipleBinaryAttributes { field_name: name })
            }
            val @ IndexDataOfField::BinaryIndex(_) => {
                index = Some(val);
            }
            val @ IndexDataOfField::SkipBinarySerialization => return Ok(val),
            IndexDataOfField::NoIndexData => {}
        }
    }
    Ok(index.unwrap_or(IndexDataOfField::NoIndexData))
}

fn parse_single_attribute(
    attr: &Attribute,
    attribute_name: &str,
    field_name: &str,
) -> Result<IndexDataOfField, ParsingError> {
    if attr.path().is_ident(CALLTABLE_ATTRIBUTE) {
        let meta = &attr.meta;
        match meta {
            Meta::List(list) => {
                let fetched_attributes = list
                    .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
                    .map_err(|e| ParsingError::MalfrormedCalltableAttribute {
                        attribute_name: attribute_name.to_string(),
                        field_name: field_name.to_string(),
                        got: e.to_string(),
                    })?;
                if fetched_attributes.len() != 1 {
                    return Err(ParsingError::MalfrormedCalltableAttribute {
                        attribute_name: attribute_name.to_string(),
                        field_name: field_name.to_string(),
                        got: list.to_token_stream().to_string(),
                    });
                }
                let meta = fetched_attributes.get(0).unwrap();
                match meta {
                    Meta::NameValue(nv) => match &nv.value {
                        syn::Expr::Lit(lit) => match &lit.lit {
                            syn::Lit::Str(s) if s.value() == "skip" => {
                                Ok(IndexDataOfField::SkipBinarySerialization)
                            }
                            syn::Lit::Int(i) => {
                                let v: u16 = i.base10_parse().map_err(|e| {
                                    ParsingError::IndexValueNotU16 {
                                        field_name: field_name.to_string(),
                                        got: i.to_string(),
                                        err: e.to_string(),
                                    }
                                })?;
                                Ok(IndexDataOfField::BinaryIndex(v))
                            }
                            _ => Err(ParsingError::UnexpectedBinaryIndexDefinition {
                                attribute_name: attribute_name.to_string(),
                                field_name: field_name.to_string(),
                                got: lit.into_token_stream().to_string(),
                            }),
                        },
                        _ => Err(ParsingError::UnexpectedBinaryIndexDefinition {
                            attribute_name: attribute_name.to_string(),
                            field_name: field_name.to_string(),
                            got: (&nv.value).into_token_stream().to_string(),
                        }),
                    },
                    Meta::Path(path) if path.is_ident("skip") => {
                        path.segments.len();
                        Ok(IndexDataOfField::SkipBinarySerialization)
                    }
                    _ => Err(ParsingError::MalfrormedCalltableAttribute {
                        attribute_name: attribute_name.to_string(),
                        field_name: field_name.to_string(),
                        got: list.to_token_stream().to_string(),
                    }),
                }
            }
            _ => Err(ParsingError::MalfrormedCalltableAttribute {
                attribute_name: attribute_name.to_string(),
                field_name: field_name.to_string(),
                got: attr.to_token_stream().to_string(),
            }),
        }
    } else {
        Ok(IndexDataOfField::NoIndexData)
    }
}

pub(crate) fn field_name_or_field_position(name: Option<&Ident>, position: u16) -> Ident {
    name.cloned().unwrap_or(Ident::new_raw(
        format!("field_{}", position).as_str(),
        Span::call_site(),
    ))
}

fn get_indexes(enum_variants: &BTreeMap<u8, EnumVariant>) -> Vec<u8> {
    let mut to_ret: Vec<u8> = enum_variants.iter().map(|(k, _)| *k).collect();
    to_ret.sort();
    to_ret
}

pub(crate) fn validate_enum_variants(
    enum_name: String,
    enum_variants: &BTreeMap<u8, EnumVariant>,
) -> Result<(), ParsingError> {
    if enum_variants.is_empty() {
        return Err(ParsingError::EnumCannotBeEmpty { enum_name });
    }
    let expected_indexes: Vec<u8> = (0..(enum_variants.len() as u8)).collect();
    let found_indexes = get_indexes(enum_variants);
    if found_indexes != expected_indexes {
        return Err(ParsingError::EnumVariantIndexesNotSequential {
            enum_name,
            found_indexes,
            expected_indexes,
        });
    }
    for (_, value) in enum_variants.iter() {
        let variant_name = value.variant_name.to_string();
        let fields_definition = &value.field_definitions;
        let expected_indexes: Vec<u16> =
            (1..(fields_definition.number_of_fields() as u16 + 1)).collect();
        let found_indexes: Vec<u16> = fields_definition.indexes();
        if found_indexes != expected_indexes {
            return Err(ParsingError::EnumVariantFieldIndexesNotSequential {
                enum_name,
                variant_name,
                found_indexes,
                expected_indexes,
            });
        }
    }
    Ok(())
}

pub(crate) fn validate_struct_fields(
    struct_name: String,
    fields: &FieldDefinitions,
) -> Result<(), ParsingError> {
    let expected_indexes: Vec<u16> = (0..fields.number_of_fields() as u16).collect();
    let found_indexes = fields.indexes();
    if found_indexes != expected_indexes {
        return Err(ParsingError::StructFieldIndexesNotSequential {
            struct_name,
            found_indexes,
            expected_indexes,
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{parse_quote, Attribute};

    #[test]
    fn parse_single_attribute_should_fetch_skip_from_calltable_property() {
        let attr: Attribute = parse_quote! {
            #[calltable(skip)]
        };
        assert_eq!(
            parse_single_attribute(&attr, "field_index", "xyz"),
            Ok(IndexDataOfField::SkipBinarySerialization)
        );
    }

    #[test]
    fn given_malformed_skip_when_calling_parse_single_attribute_then_should_fail() {
        let attr: Attribute = parse_quote! {
            #[calltable(skip = 1)]
        };
        let a = parse_single_attribute(&attr, "field_index", "xyz");
        assert!(matches!(
            a,
            Err(ParsingError::MalfrormedCalltableAttribute { .. })
        ));

        let attr: Attribute = parse_quote! {
            #[calltable(skip = "a")]
        };
        assert!(matches!(
            parse_single_attribute(&attr, "field_index", "xyz"),
            Err(ParsingError::MalfrormedCalltableAttribute { .. })
        ));

        let attr: Attribute = parse_quote! {
            #[calltable(skip = a)]
        };
        assert!(matches!(
            parse_single_attribute(&attr, "field_index", "xyz"),
            Err(ParsingError::MalfrormedCalltableAttribute { .. })
        ));
    }

    #[test]
    fn parse_single_attribute_should_fetch_index_from_calltable_property() {
        let attr: Attribute = parse_quote! {
            #[calltable(field_index = 0)]
        };
        assert_eq!(
            parse_single_attribute(&attr, "field_index", "xyz"),
            Ok(IndexDataOfField::BinaryIndex(0))
        );
    }

    #[test]
    fn parse_single_attribute_should_fetch_index_from_calltable_property_other_value() {
        let attr: Attribute = parse_quote! {
            #[calltable(field_index = 5)]
        };
        assert_eq!(
            parse_single_attribute(&attr, "field_index", "xyz"),
            Ok(IndexDataOfField::BinaryIndex(5))
        );
    }

    #[test]
    fn parse_single_attribute_should_fetch_index_from_calltable_property_other_attribute_name() {
        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5)]
        };
        assert_eq!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Ok(IndexDataOfField::BinaryIndex(5))
        );
    }

    #[test]
    fn parse_single_attribute_should_fail_if_queried_attribute_is_not_present() {
        let attr: Attribute = parse_quote! {
            #[calltable(qqq = 5)]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );
    }

    #[test]
    fn parse_single_attribute_should_fail_if_multiple_calltable_attributes() {
        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5, abc = 3)]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );

        let attr: Attribute = parse_quote! {
            #[calltable(skip, skip)]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );

        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5, abc = 5)]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );

        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5, def = 5)]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );

        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5, xxx)]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );

        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5, some_other(a))]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );

        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5, calltable(a, b, x = 1))]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );

        let attr: Attribute = parse_quote! {
            #[calltable(abc = 5, calltable(abc = 10))]
        };
        matches!(
            parse_single_attribute(&attr, "abc", "xyz"),
            Err(ParsingError::BinaryAttributeMissing { .. })
        );
    }
}
