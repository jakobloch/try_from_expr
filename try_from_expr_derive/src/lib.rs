//! Derive macro implementation for `try_from_expr`.
//!
//! This crate provides the procedural macro implementation for generating
//! `TryFrom<&syn::Expr>` implementations. It should be used through the
//! main `try_from_expr` crate.

mod generators;
mod helpers;

use crate::{
    generators::{
        generate_arg_processing, generate_helper_functions, generate_struct_field_parsing,
    },
    helpers::{detect_wrapper_enum, extract_type_name},
};
use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Error, Fields, parse_macro_input};

/// Derives `TryFrom<&syn::Expr>` for enums.
///
/// This macro generates an implementation that can parse `syn::Expr` values
/// into your enum type. It automatically detects whether the enum is a
/// "wrapper" enum or a "leaf" enum and generates appropriate parsing logic.
///
/// # Attributes
///
/// - `#[try_from_expr(wrapper)]` - Force wrapper enum mode
/// - `#[try_from_expr(leaf)]` - Force leaf enum mode
///
/// # Example
///
/// ```rust,ignore
/// #[derive(TryFromExpr)]
/// enum MyEnum {
///     Unit,
///     Tuple(String),
///     Struct { field: i32 },
/// }
/// ```
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

    // Check for explicit mode attribute
    let mut forced_mode = None;
    for attr in &input.attrs {
        if attr.path().is_ident("try_from_expr") {
            if let Ok(list) = attr.meta.require_list() {
                if let Ok(nested) = list.parse_args::<syn::Ident>() {
                    match nested.to_string().as_str() {
                        "wrapper" => forced_mode = Some(true),
                        "leaf" => forced_mode = Some(false),
                        _ => {}
                    }
                }
            }
        }
    }

    // Detect if this is a wrapper enum or a leaf enum (or use forced mode)
    let is_wrapper_enum = forced_mode.unwrap_or_else(|| detect_wrapper_enum(&data));

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
            let type_name = extract_type_name(field_type);
            let type_name_lit = syn::LitStr::new(&type_name, proc_macro2::Span::call_site());
            let parser = crate::generators::generate_type_parser(field_type, quote! { expr });
            quote! {
                #type_name_lit => {
                    return #parser
                        .map(#enum_name::#variant_name)
                        .map_err(|e| ::syn::Error::new(expr.span(), format!("Failed to parse {}: {}", #type_name_lit, e)))
                },
            }
        })
        .collect();

    // Generate fallback that tries all children
    let fallback_attempts: Vec<_> = child_types
        .iter()
        .map(|(variant_name, field_type)| {
            let parser = crate::generators::generate_type_parser(field_type, quote! { expr });

            quote! {
                {
                    match #parser {
                        Ok(v) => return Ok(#enum_name::#variant_name(v)),
                        Err(e) => {
                            errors.push(format!(
                                "- Variant '{}': Could not parse as type '{}': {}",
                                stringify!(#variant_name),
                                stringify!(#field_type),
                                e
                            ));
                        }
                    }
                }
            }
        })
        .collect();

    // Generate unit variant arms for wrapper enums
    let unit_variant_arms: Vec<_> = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let variant_str = variant_name.to_string();

            match &variant.fields {
                Fields::Unit => {
                    let variant_lit =
                        syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());
                    Some(quote! {
                        #variant_lit => return Ok(#enum_name::#variant_name),
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

                // Check for unit variants first (e.g., Default, None, etc.)
                if let ::syn::Expr::Path(path_expr) = expr {
                    let path = &path_expr.path;
                    if let Some(last) = path.segments.last() {
                        let variant_str = last.ident.to_string();
                        match variant_str.as_str() {
                            #(#unit_variant_arms)*
                            _ => {}
                        }
                    }
                }

                // Try type-driven dispatch first
                if let Ok(mut enum_type) = Self::determine_enum_type(expr) {
                    // Normalize "Self" to the actual enum name
                    if enum_type == "Self" {
                        enum_type = stringify!(#enum_name).to_string();
                    }
                    match enum_type.as_str() {
                        #(#type_match_arms)*
                        _ => {
                            // Fall through to try all children
                        }
                    }
                }

                // Fallback: try each child type and aggregate errors
                let mut errors = Vec::new();
                #(#fallback_attempts)*

                let error_details = errors.join("\n");
                Err(::syn::Error::new(
                    expr.span(),
                    format!("No variant of {} could parse the expression. Failures:\n{}", stringify!(#enum_name), error_details)
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
    let enum_name_str = enum_name.to_string();

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
                Fields::Unit => {
                    let variant_lit =
                        syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());
                    Some(quote! {
                        #variant_lit => Ok(#enum_name::#variant_name),
                    })
                }
                _ => None,
            }
        })
        .collect();

    // Collect parametrized variant information (tuple variants)
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

    // Collect struct variant information
    let struct_variant_names: Vec<String> = data
        .variants
        .iter()
        .filter_map(|v| match &v.fields {
            Fields::Named(_) => Some(v.ident.to_string()),
            _ => None,
        })
        .collect();
    let _struct_variants_str = if struct_variant_names.is_empty() {
        "(no struct variants)".to_string()
    } else {
        struct_variant_names.join(", ")
    };

    // Generate match arms for call expressions (tuple variants with parameters)
    let call_variant_arms: Vec<_> = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let variant_str = variant_name.to_string();

            match &variant.fields {
                Fields::Unnamed(fields) => {
                    let arg_processing = generate_arg_processing(variant_name, &fields.unnamed);
                    let variant_lit =
                        syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());

                    Some(quote! {
                        #variant_lit => {
                            #arg_processing
                        },
                    })
                }
                _ => None,
            }
        })
        .collect();

    // Generate match arms for struct expressions (struct variants)
    let struct_variant_arms: Vec<_> = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let variant_str = variant_name.to_string();

            match &variant.fields {
                Fields::Named(fields) => {
                    let field_parsers = generate_struct_field_parsing(variant_name, fields);
                    let variant_lit =
                        syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());

                    Some(quote! {
                        #variant_lit => {
                            #field_parsers
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
                    // Handle unit variants like StringEnum::Email or just Email
                    ::syn::Expr::Path(path_expr) => {
                        let path = &path_expr.path;
                        if let Some(variant_seg) = path.segments.last() {
                            let variant_name = &variant_seg.ident;
                            if path.segments.len() > 1 {
                                let is_correct_enum = if let Some(enum_seg) = path.segments.iter().rev().nth(1) {
                                    let name = enum_seg.ident.to_string();
                                    name == #enum_name_str || name == "Self"
                                } else {
                                    false
                                };

                                if is_correct_enum {
                                    let variant_str = variant_name.to_string();
                                    match variant_str.as_str() {
                                        #(#unit_variant_arms)*
                                        _ => Err(::syn::Error::new(
                                            variant_name.span(),
                                            format!("Unknown unit variant '{}' for enum '{}'", variant_str, #enum_name_str)
                                        )),
                                    }
                                } else {
                                    Err(::syn::Error::new(
                                        path.span(),
                                        format!("Expected a path to enum '{}'", #enum_name_str)
                                    ))
                                }
                            } else {
                                // Shorthand like "Email"
                                let variant_str = variant_name.to_string();
                                match variant_str.as_str() {
                                    #(#unit_variant_arms)*
                                    _ => Err(::syn::Error::new(
                                        variant_name.span(),
                                        format!("Unknown unit variant '{}'. Valid options: {}", variant_str, #unit_variants_str)
                                    )),
                                }
                            }
                        } else {
                             Err(::syn::Error::new(path.span(), "Path has no segments"))
                        }
                    }

                    // Handle variants with parameters like StringEnum::MinLength(5)
                    ::syn::Expr::Call(call_expr) => {
                        if let ::syn::Expr::Path(path_expr) = &*call_expr.func {
                            let path = &path_expr.path;
                            if let Some(variant_seg) = path.segments.last() {
                                let is_correct_enum = if let Some(enum_seg) = path.segments.iter().rev().nth(1) {
                                    let name = enum_seg.ident.to_string();
                                    name == #enum_name_str || name == "Self"
                                } else {
                                    false
                                };

                                if is_correct_enum {
                                    let variant_str = variant_seg.ident.to_string();
                                    match variant_str.as_str() {
                                        #(#call_variant_arms)*
                                        _ => Err(::syn::Error::new(
                                            variant_seg.span(),
                                            format!("Unknown parametrized variant '{}'. Valid options: {}", variant_str, #param_variants_str)
                                        )),
                                    }
                                } else {
                                    Err(::syn::Error::new(
                                        path.span(),
                                        format!("Expected a call to a variant of enum '{}'", #enum_name_str)
                                    ))
                                }
                            } else {
                                Err(::syn::Error::new(path.span(), "Call path has no segments"))
                            }
                        } else {
                            Err(::syn::Error::new(
                                call_expr.func.span(),
                                "Call expression must be a path to an enum variant"
                            ))
                        }
                    }

                    // Handle struct variants like StringEnum::Config { min: 5, max: 10 }
                    ::syn::Expr::Struct(struct_expr) => {
                        let path = &struct_expr.path;
                        if let Some(variant_seg) = path.segments.last() {
                            let is_correct_enum = if let Some(enum_seg) = path.segments.iter().rev().nth(1) {
                                let name = enum_seg.ident.to_string();
                                name == #enum_name_str || name == "Self"
                            } else {
                                false
                            };

                            if is_correct_enum {
                                let variant_str = variant_seg.ident.to_string();
                                match variant_str.as_str() {
                                    #(#struct_variant_arms)*
                                    _ => Err(::syn::Error::new(
                                        variant_seg.span(),
                                        format!("Unknown struct variant '{}' for enum '{}'", variant_str, #enum_name_str)
                                    )),
                                }
                            } else {
                                Err(::syn::Error::new(
                                    path.span(),
                                    format!("Expected a struct literal for a variant of enum '{}'", #enum_name_str)
                                ))
                            }
                        } else {
                            Err(::syn::Error::new(path.span(), "Struct path has no segments"))
                        }
                    }

                    _ => Err(::syn::Error::new(
                        expr.span(),
                        format!(
                            "Unsupported expression type for enum '{}'. Expected a path, call, or struct expression.",
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
