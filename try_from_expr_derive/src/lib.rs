mod generators;
mod helpers;

use crate::{
    generators::{generate_arg_processing, generate_helper_functions},
    helpers::{detect_wrapper_enum, extract_type_name},
};
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Error, Fields, parse_macro_input};

#[proc_macro_derive(TryFromExpr, attributes(try_from_expr))]
pub fn derive_try_from_expr(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let enum_name = &input.ident;

    let data = match &input.data {
        Data::Enum(data) => data,
        _ => {
            return Error::new_spanned(&input, "TryFromExpr can only be applied to enums")
                .to_compile_error()
                .into();
        }
    };

    // Detect if this is a wrapper enum or a leaf enum
    let is_wrapper_enum = detect_wrapper_enum(&data);

    if is_wrapper_enum {
        generate_wrapper_enum_impl(&input, &enum_name, &data)
    } else {
        generate_leaf_enum_impl(&input, &enum_name, &data)
    }
}

// Generate implementation for wrapper enums
fn generate_wrapper_enum_impl(
    _input: &DeriveInput,
    enum_name: &syn::Ident,
    data: &syn::DataEnum,
) -> TokenStream {
    let helpers = generate_helper_functions();
    // Extract child types from wrapper enum variants
    let child_types: Vec<_> = data
        .variants
        .iter()
        .filter_map(|variant| {
            if let Fields::Unnamed(fields) = &variant.fields {
                if let Some(field) = fields.unnamed.first() {
                    return Some((&variant.ident, &field.ty));
                }
            }
            None
        })
        .collect();

    // Generate type determination for fast path
    let type_match_arms: Vec<_> = child_types
        .iter()
        .map(|(variant_name, field_type)| {
            use crate::helpers::types::TypeKind;
            let type_name = extract_type_name(field_type);
            
            // Check if this is a basic type we can parse directly
            let type_kind = TypeKind::parse_syn_ty(field_type);
            let parse_expr = match type_kind {
                // Basic types - use our parsers
                TypeKind::String | TypeKind::Bool | TypeKind::Char |
                TypeKind::F64 | TypeKind::F32 | 
                TypeKind::I64 | TypeKind::I32 | TypeKind::I16 | TypeKind::I8 |
                TypeKind::U64 | TypeKind::U32 | TypeKind::U16 | TypeKind::U8 |
                TypeKind::Usize | TypeKind::Isize | TypeKind::OrderedFloat(_) => {
                    let parser = crate::generators::generate_type_parser(field_type, quote! { expr });
                    quote! { #parser }
                }
                // Custom types - require TryFrom
                _ => {
                    quote! { <#field_type as TryFrom<&::syn::Expr>>::try_from(expr) }
                }
            };
            
            quote! {
                #type_name => #parse_expr
                    .map(#enum_name::#variant_name)
                    .map_err(|e| ::syn::Error::new(expr.span(), format!("Failed to parse {}: {}", #type_name, e))),
            }
        })
        .collect();

    // Generate fallback that tries all children
    let fallback_attempts: Vec<_> = child_types
        .iter()
        .map(|(variant_name, field_type)| {
            use crate::helpers::types::TypeKind;
            
            // Check if this is a basic type we can parse directly
            let type_kind = TypeKind::parse_syn_ty(field_type);
            let parse_expr = match type_kind {
                // Basic types - use our parsers
                TypeKind::String | TypeKind::Bool | TypeKind::Char |
                TypeKind::F64 | TypeKind::F32 | 
                TypeKind::I64 | TypeKind::I32 | TypeKind::I16 | TypeKind::I8 |
                TypeKind::U64 | TypeKind::U32 | TypeKind::U16 | TypeKind::U8 |
                TypeKind::Usize | TypeKind::Isize | TypeKind::OrderedFloat(_) => {
                    let parser = crate::generators::generate_type_parser(field_type, quote! { expr });
                    quote! { #parser }
                }
                // Custom types - require TryFrom
                _ => {
                    quote! { <#field_type as TryFrom<&::syn::Expr>>::try_from(expr) }
                }
            };
            
            quote! {
                if let Ok(val) = #parse_expr {
                    return Ok(#enum_name::#variant_name(val));
                }
            }
        })
        .collect();

    let expanded = quote! {
        impl TryFrom<&::syn::Expr> for #enum_name {
            type Error = ::syn::Error;

            fn try_from(expr: &::syn::Expr) -> Result<Self, Self::Error> {
                use ::syn::spanned::Spanned;

                // Unwrap parentheses and groups
                let expr = Self::unwrap_expr(expr);

                // Try type-driven dispatch first
                if let Ok(enum_type) = Self::determine_enum_type(expr) {
                    let result = match enum_type.as_str() {
                        #(#type_match_arms)*
                        _ => {
                            // Fall through to try all children
                            Err(::syn::Error::new(expr.span(), "No matching type"))
                        }
                    };
                    if result.is_ok() {
                        return result;
                    }
                }

                // Fallback: try each child type
                #(#fallback_attempts)*

                Err(::syn::Error::new(
                    expr.span(),
                    format!("No matching enum variant could parse this expression for {}", stringify!(#enum_name))
                ))
            }
        }

        impl #enum_name {
            #helpers

            /// Try to parse from any child enum without type hints
            pub fn from_any_expr(expr: &::syn::Expr) -> Result<Self, ::syn::Error> {
                <Self as TryFrom<&::syn::Expr>>::try_from(expr)
            }
        }
    };

    TokenStream::from(expanded)
}

// Generate implementation for leaf enums
fn generate_leaf_enum_impl(
    _input: &DeriveInput,
    enum_name: &syn::Ident,
    data: &syn::DataEnum,
) -> TokenStream {
    let helpers = generate_helper_functions();

    // Collect variant information for better error messages
    let unit_variant_names: Vec<String> = data
        .variants
        .iter()
        .filter_map(|v| match &v.fields {
            Fields::Unit => Some(v.ident.to_string()),
            _ => None,
        })
        .collect();
    let unit_variants_str = if unit_variant_names.is_empty() {
        "(no unit variants)".to_string()
    } else {
        unit_variant_names.join(", ")
    };

    // Generate match arms for path expressions (unit variants)
    let unit_variant_arms: Vec<_> = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let variant_str = variant_name.to_string();

            match &variant.fields {
                Fields::Unit => Some(quote! {
                    #variant_str => Ok(#enum_name::#variant_name),
                }),
                _ => None,
            }
        })
        .collect();

    // Collect parametrized variant information
    let param_variant_names: Vec<String> = data
        .variants
        .iter()
        .filter_map(|v| match &v.fields {
            Fields::Unnamed(_) => Some(v.ident.to_string()),
            _ => None,
        })
        .collect();
    let param_variants_str = if param_variant_names.is_empty() {
        "(no parametrized variants)".to_string()
    } else {
        param_variant_names.join(", ")
    };

    // Generate match arms for call expressions (variants with parameters)
    let call_variant_arms: Vec<_> = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let variant_str = variant_name.to_string();

            match &variant.fields {
                Fields::Unnamed(fields) => {
                    let field_count = fields.unnamed.len();
                    let arg_processing = generate_arg_processing(variant_name, &fields.unnamed);

                    Some(quote! {
                        #variant_str => {
                            if call_expr.args.len() != #field_count {
                                return Err(::syn::Error::new(
                                    call_expr.span(),
                                    format!(
                                        "Variant '{}' expects exactly {} argument{}, but {} {} provided",
                                        #variant_str,
                                        #field_count,
                                        if #field_count == 1 { "" } else { "s" },
                                        call_expr.args.len(),
                                        if call_expr.args.len() == 1 { "was" } else { "were" }
                                    )
                                ));
                            }
                            #arg_processing
                        },
                    })
                }
                _ => None,
            }
        })
        .collect();

    let expanded = quote! {
        impl TryFrom<&::syn::Expr> for #enum_name {
            type Error = ::syn::Error;

            fn try_from(expr: &::syn::Expr) -> Result<Self, Self::Error> {
                use ::syn::spanned::Spanned;

                // Unwrap parentheses and groups
                let expr = Self::unwrap_expr(expr);

                match expr {
                    // Handle unit variants like StringEnum::Email
                    ::syn::Expr::Path(path_expr) => {
                        let path = &path_expr.path;

                        if path.segments.len() >= 2 {
                            let enum_name_seg = &path.segments[path.segments.len() - 2].ident;
                            let variant_name = &path.segments[path.segments.len() - 1].ident;

                            // Verify this is the correct enum
                            if enum_name_seg.to_string() == stringify!(#enum_name) {
                                let variant_str = variant_name.to_string();
                                match variant_str.as_str() {
                                    #(#unit_variant_arms)*
                                    _ => Err(::syn::Error::new(
                                        variant_name.span(),
                                        format!(
                                            "Unknown variant '{}' for enum '{}'. Valid unit variants: {}",
                                            variant_str,
                                            stringify!(#enum_name),
                                            #unit_variants_str
                                        )
                                    )),
                                }
                            } else {
                                Err(::syn::Error::new(
                                    enum_name_seg.span(),
                                    format!("Expected {}, got {}", stringify!(#enum_name), enum_name_seg)
                                ))
                            }
                        } else if path.segments.len() == 1 {
                            // Handle shorthand like just "Email"
                            let variant_name = &path.segments[0].ident;
                            let variant_str = variant_name.to_string();
                            match variant_str.as_str() {
                                #(#unit_variant_arms)*
                                _ => Err(::syn::Error::new(
                                    variant_name.span(),
                                    format!(
                                        "Unknown variant '{}' for enum '{}'. Valid unit variants: {}",
                                        variant_str,
                                        stringify!(#enum_name),
                                        #unit_variants_str
                                    )
                                )),
                            }
                        } else {
                            Err(::syn::Error::new(
                                path.span(),
                                format!(
                                    "Invalid path format. Expected either 'VariantName' or '{}::VariantName'",
                                    stringify!(#enum_name)
                                )
                            ))
                        }
                    }

                    // Handle variants with parameters like StringEnum::MinLength(5)
                    ::syn::Expr::Call(call_expr) => {
                        if let ::syn::Expr::Path(path_expr) = &*call_expr.func {
                            let path = &path_expr.path;

                            if path.segments.len() >= 2 {
                                let enum_name_seg = &path.segments[path.segments.len() - 2].ident;
                                let variant_name = &path.segments[path.segments.len() - 1].ident;

                                if enum_name_seg.to_string() == stringify!(#enum_name) {
                                    let variant_str = variant_name.to_string();
                                    match variant_str.as_str() {
                                        #(#call_variant_arms)*
                                        _ => Err(::syn::Error::new(
                                            variant_name.span(),
                                            format!(
                                                "Unknown variant '{}' for enum '{}'. Valid parametrized variants: {}",
                                                variant_str,
                                                stringify!(#enum_name),
                                                #param_variants_str
                                            )
                                        )),
                                    }
                                } else {
                                    Err(::syn::Error::new(
                                        enum_name_seg.span(),
                                        format!("Expected {}, got {}", stringify!(#enum_name), enum_name_seg)
                                    ))
                                }
                            } else {
                                Err(::syn::Error::new(
                                    call_expr.span(),
                                    format!(
                                        "Invalid call expression format. Expected '{}::VariantName(args...)'",
                                        stringify!(#enum_name)
                                    )
                                ))
                            }
                        } else {
                            Err(::syn::Error::new(
                                call_expr.span(),
                                "Invalid syntax: call expression must use a path (e.g., EnumName::Variant(...))"
                            ))
                        }
                    }

                    _ => Err(::syn::Error::new(
                        expr.span(),
                        format!(
                            "Unsupported expression type for enum '{}'. Expected either a path (e.g., {}::Variant) or a call expression (e.g., {}::Variant(value))",
                            stringify!(#enum_name),
                            stringify!(#enum_name),
                            stringify!(#enum_name)
                        )
                    )),
                }
            }
        }

        impl #enum_name {
            #helpers

            pub fn parse_from_expr(expr: &::syn::Expr) -> Result<Self, ::syn::Error> {
                <Self as TryFrom<&::syn::Expr>>::try_from(expr)
            }
        }
    };

    TokenStream::from(expanded)
}
