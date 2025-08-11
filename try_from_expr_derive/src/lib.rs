mod generators;
mod helpers;

use crate::{
    generators::{generate_arg_processing, generate_struct_field_parsing, generate_helper_functions},
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
            use crate::helpers::types::TypeKind;
            let type_name = extract_type_name(field_type);
            let type_name_lit = syn::LitStr::new(&type_name, proc_macro2::Span::call_site());
            
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
                #type_name_lit => {
                    return #parse_expr
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
                {
                    match #parse_expr {
                        Ok(v) => return Ok(#enum_name::#variant_name(v)),
                        Err(e) => {
                            if let Some(mut a) = acc.take() { 
                                a.combine(e); 
                                acc = Some(a); 
                            } else { 
                                acc = Some(e); 
                            }
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
                    let variant_lit = syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());
                    Some(quote! {
                        #variant_lit => return Ok(#enum_name::#variant_name),
                    })
                },
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
                let mut acc: Option<::syn::Error> = None;
                #(#fallback_attempts)*

                Err(acc.unwrap_or_else(|| ::syn::Error::new(
                    expr.span(),
                    format!("No variant of {} could parse this expression", stringify!(#enum_name))
                )))
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
                Fields::Unit => {
                    let variant_lit = syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());
                    Some(quote! {
                        #variant_lit => Ok(#enum_name::#variant_name),
                    })
                },
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
                    let field_count = fields.unnamed.len();
                    let arg_processing = generate_arg_processing(variant_name, &fields.unnamed);
                    let variant_lit = syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());

                    Some(quote! {
                        #variant_lit => {
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
                    let variant_lit = syn::LitStr::new(&variant_str, proc_macro2::Span::call_site());
                    
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
                    // Handle unit variants like StringEnum::Email
                    ::syn::Expr::Path(path_expr) => {
                        let path = &path_expr.path;

                        if path.segments.len() >= 2 {
                            let enum_name_seg = &path.segments[path.segments.len() - 2].ident;
                            let variant_name = &path.segments[path.segments.len() - 1].ident;

                            // Verify this is the correct enum (supports both EnumName and Self)
                            let enum_ident_str = stringify!(#enum_name);
                            let enum_name_str = enum_name_seg.to_string();
                            let matches_enum = enum_name_str == enum_ident_str || enum_name_str == "Self";
                            
                            if matches_enum {
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

                                let enum_ident_str = stringify!(#enum_name);
                                let enum_name_str = enum_name_seg.to_string();
                                let matches_enum = enum_name_str == enum_ident_str || enum_name_str == "Self";
                                
                                if matches_enum {
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

                    // Handle struct variants like StringEnum::Config { min: 5, max: 10 }
                    ::syn::Expr::Struct(struct_expr) => {
                        let path = &struct_expr.path;
                        
                        if path.segments.len() >= 2 {
                            let enum_name_seg = &path.segments[path.segments.len() - 2].ident;
                            let variant_name = &path.segments[path.segments.len() - 1].ident;

                            let enum_ident_str = stringify!(#enum_name);
                            let enum_name_str = enum_name_seg.to_string();
                            let matches_enum = enum_name_str == enum_ident_str || enum_name_str == "Self";
                            
                            if matches_enum {
                                let variant_str = variant_name.to_string();
                                match variant_str.as_str() {
                                    #(#struct_variant_arms)*
                                    _ => Err(::syn::Error::new(
                                        variant_name.span(),
                                        format!(
                                            "Unknown struct variant '{}' for enum '{}'",
                                            variant_str,
                                            stringify!(#enum_name)
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
                                struct_expr.span(),
                                format!(
                                    "Invalid struct expression format. Expected '{}::VariantName {{ fields... }}'",
                                    stringify!(#enum_name)
                                )
                            ))
                        }
                    }

                    _ => Err(::syn::Error::new(
                        expr.span(),
                        format!(
                            "Unsupported expression type for enum '{}'. Expected a path (e.g., {}::Variant), call expression (e.g., {}::Variant(value)), or struct expression (e.g., {}::Variant {{ field: value }})",
                            stringify!(#enum_name),
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
