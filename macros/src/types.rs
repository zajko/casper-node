use proc_macro2::Span;
use quote::ToTokens;
use std::{collections::BTreeMap, u16};
use syn::{Attribute, Fields, Ident, Meta, Type};

use crate::{error::ParsingError, BINARY_INDEX_ATTRIBUTE};

pub(crate) enum IndexDataOfField {
    BinaryIndex(u16),
    SkipBinarySerialization,
    NoBinaryData,
}
pub enum FieldDefinitions {
    UnnamedFieldDefinitions(BTreeMap<u16, UnnamedFieldDefinition>),
    NamedFieldDefinitions(BTreeMap<u16, NamedFieldDefinition>),
    Unit,
}

impl FieldDefinitions {
    pub(crate) fn number_of_fields(&self) -> usize {
        match self {
            FieldDefinitions::UnnamedFieldDefinitions(fields) => fields.len(),
            FieldDefinitions::NamedFieldDefinitions(fields) => fields.len(),
            FieldDefinitions::Unit => 0,
        }
    }
    pub(crate) fn indexes(&self) -> Vec<u16> {
        let mut to_ret = match self {
            FieldDefinitions::UnnamedFieldDefinitions(fields) => fields
                .iter()
                .filter(|(_, v)| v.index.is_some())
                .map(|(_, v)| v.index.unwrap())
                .collect(),
            FieldDefinitions::NamedFieldDefinitions(fields) => fields
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
                let maybe_binary_index =
                    get_binary_index(&variant.attrs, variant_name.to_string())?;

                match maybe_binary_index {
                    Some(IndexDataOfField::SkipBinarySerialization) => {
                        return Err(ParsingError::SkipNotAllowedForEnum {
                            variant_name: variant_name.to_string(),
                        })
                    }
                    Some(IndexDataOfField::NoBinaryData) => {
                        return Err(ParsingError::BinaryAttributeMissing {
                            field_name: variant_name.to_string(),
                        })
                    }
                    Some(IndexDataOfField::BinaryIndex(idx)) => {
                        variants.insert(
                            idx as u8,
                            EnumVariant {
                                variant_name,
                                variant_index: idx,
                                field_definitions: build_index_map(&variant.fields)?,
                            },
                        );
                    }
                    None => {
                        return Err(ParsingError::BinaryAttributeMissing {
                            field_name: variant_name.to_string(),
                        })
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
                        .find(|(_, v)| v.index.is_some() && v.index.unwrap() == idx)
                        .is_some()
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
            Ok(FieldDefinitions::NamedFieldDefinitions(binary_indexes))
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
                        .find(|(_, v)| v.index.is_some() && v.index.unwrap() == idx)
                        .is_some()
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
            Ok(FieldDefinitions::UnnamedFieldDefinitions(binary_indexes))
        }
        syn::Fields::Unit => Ok(FieldDefinitions::Unit),
    }
}

fn build_field_definition(
    attrs: &Vec<Attribute>,
    name: Option<&Ident>,
    ty: &Type,
    field_position: u16,
) -> Result<NamedFieldDefinition, ParsingError> {
    let field_ident = field_name_or_field_position(name.clone(), field_position);
    let field_name = field_ident.to_string();
    let binary_index = get_binary_index(attrs, field_name.clone())?;

    match binary_index {
        Some(IndexDataOfField::BinaryIndex(idx)) => Ok(NamedFieldDefinition {
            name: field_ident.clone(),
            index: Some(idx),
            ty: ty.clone(),
        }),
        Some(_) => {
            //We need to keep data about skipped fields, because for unnamed fields we need to keep
            // the order
            Ok(NamedFieldDefinition {
                name: field_ident.clone(),
                index: None,
                ty: ty.clone(),
            })
        }
        None => Err(ParsingError::BinaryAttributeMissing { field_name }),
    }
}

fn get_binary_index(
    attrs: &Vec<Attribute>,
    name: String,
) -> Result<Option<IndexDataOfField>, ParsingError> {
    let mut binary_index: Option<IndexDataOfField> = None;
    for attr in attrs.iter() {
        match parse_binary_index(name.clone(), attr)? {
            IndexDataOfField::BinaryIndex(_) | IndexDataOfField::SkipBinarySerialization
                if binary_index.is_some() =>
            {
                return Err(ParsingError::MultipleBinaryAttributes {
                    field_name: name.to_string(),
                });
            }
            bi @ (IndexDataOfField::BinaryIndex(_) | IndexDataOfField::SkipBinarySerialization) => {
                binary_index = Some(bi)
            }
            IndexDataOfField::NoBinaryData => {}
        }
    }
    Ok(binary_index)
}

fn parse_binary_index(
    field_name: String,
    attr: &syn::Attribute,
) -> Result<IndexDataOfField, ParsingError> {
    if attr.path().is_ident(BINARY_INDEX_ATTRIBUTE) {
        let meta = &attr.meta;
        match meta {
            Meta::NameValue(nv) => {
                match &nv.value {
                    syn::Expr::Lit(lit) => match &lit.lit {
                        syn::Lit::Str(s) if s.value() == "skip" => {
                            return Ok(IndexDataOfField::SkipBinarySerialization)
                        }
                        syn::Lit::Str(s) => {
                            return Err(ParsingError::UnexpectedBinaryIndexDefinition {
                                field_name,
                                got: s.value(),
                            })
                        }
                        syn::Lit::Int(i) => {
                            let v: u16 =
                                i.base10_parse()
                                    .map_err(|e| ParsingError::IndexValueNotU16 {
                                        field_name,
                                        got: i.to_string(),
                                        err: e.to_string(),
                                    })?;
                            return Ok(IndexDataOfField::BinaryIndex(v));
                        }
                        _ => {
                            return Err(ParsingError::UnexpectedBinaryIndexDefinition {
                                field_name,
                                got: lit.into_token_stream().to_string(),
                            })
                        }
                    },
                    _ => {
                        return Err(ParsingError::UnexpectedBinaryIndexDefinition {
                            field_name,
                            got: (&nv.value).into_token_stream().to_string(),
                        })
                    }
                };
            }
            _ => {
                return Err(ParsingError::UnexpectedBinaryIndexDefinition {
                    field_name,
                    got: meta.into_token_stream().to_string(),
                })
            }
        }
    }
    return Ok(IndexDataOfField::NoBinaryData);
}

pub(crate) fn field_name_or_field_position(name: Option<&Ident>, position: u16) -> Ident {
    name.map(|i| i.clone()).unwrap_or(Ident::new_raw(
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
    return Ok(());
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
    return Ok(());
}
