use crate::helpers::{is_option_type, types::TypeKind};
use quote::quote;
use syn::Type;

// Helper to extract inner type from Vec<T>
fn extract_vec_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if let Some(seg) = type_path.path.segments.last() {
            if seg.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                        return Some(inner);
                    }
                }
            }
        }
    }
    None
}

// Helper to extract key and value types from HashMap<K, V> or BTreeMap<K, V>
fn extract_map_types(ty: &Type) -> Option<(&Type, &Type)> {
    if let Type::Path(type_path) = ty {
        if let Some(seg) = type_path.path.segments.last() {
            let ident_str = seg.ident.to_string();
            if ident_str == "HashMap" || ident_str == "BTreeMap" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    let types: Vec<&Type> = args
                        .args
                        .iter()
                        .filter_map(|arg| {
                            if let syn::GenericArgument::Type(ty) = arg {
                                Some(ty)
                            } else {
                                None
                            }
                        })
                        .collect();

                    if types.len() == 2 {
                        return Some((types[0], types[1]));
                    }
                }
            }
        }
    }
    None
}

// Helper to generate the appropriate parser for a given type
pub fn generate_type_parser(
    field_type: &Type,
    arg_name: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    // Use TypeKind to determine the appropriate parser
    let type_kind = TypeKind::parse_syn_ty(field_type);

    match type_kind {
        TypeKind::String => quote! { Self::parse_string_literal(#arg_name) },
        TypeKind::Bool => quote! { Self::parse_bool_literal(#arg_name) },
        TypeKind::Char => quote! { Self::parse_char_literal(#arg_name) },
        TypeKind::F64 => quote! { Self::parse_f64_literal(#arg_name) },
        TypeKind::F32 => quote! { Self::parse_f32_literal(#arg_name) },
        TypeKind::I64 => quote! { Self::parse_i64_literal(#arg_name) },
        TypeKind::I32 => quote! { Self::parse_i32_literal(#arg_name) },
        TypeKind::I16 => quote! { Self::parse_i16_literal(#arg_name) },
        TypeKind::I8 => quote! { Self::parse_i8_literal(#arg_name) },
        TypeKind::U64 => quote! { Self::parse_u64_literal(#arg_name) },
        TypeKind::U32 => quote! { Self::parse_u32_literal(#arg_name) },
        TypeKind::U16 => quote! { Self::parse_u16_literal(#arg_name) },
        TypeKind::U8 => quote! { Self::parse_u8_literal(#arg_name) },
        TypeKind::Usize => quote! { Self::parse_usize_literal(#arg_name) },
        TypeKind::Isize => quote! { Self::parse_isize_literal(#arg_name) },
        TypeKind::OrderedFloat(inner) => match inner.as_ref() {
            TypeKind::F32 => quote! {
                Self::parse_f32_literal(#arg_name).map(::ordered_float::OrderedFloat::<f32>)
            },
            TypeKind::F64 | _ => quote! {
                Self::parse_f64_literal(#arg_name).map(::ordered_float::OrderedFloat::<f64>)
            },
        },
        TypeKind::Vec(_inner) => {
            // Extract the actual inner syn::Type for recursive parsing
            if let Some(inner_ty) = extract_vec_inner_type(field_type) {
                let inner_parser = generate_type_parser(inner_ty, quote! { elem });
                quote! {
                    {
                        use ::syn::spanned::Spanned;
                        let expr = Self::unwrap_expr(#arg_name);

                        // Handle vec![...] macro
                        if let ::syn::Expr::Macro(mac) = expr {
                            if mac.mac.path.is_ident("vec") {
                                // Parse the vec! macro content as array elements
                                let tokens = mac.mac.tokens.clone();

                                // Check for vec![x; n] repetition syntax
                                let tokens_str = tokens.to_string();
                                if tokens_str.contains(";") {
                                    return Err(::syn::Error::new(
                                        mac.span(),
                                        "vec![value; count] repetition syntax is not supported. Please use vec![value1, value2, ...] instead"
                                    ));
                                }

                                if let Ok(array_expr) = ::syn::parse2::<::syn::ExprArray>(tokens) {
                                    let mut result = Vec::new();
                                    for elem in array_expr.elems.iter() {
                                        match #inner_parser {
                                            Ok(val) => result.push(val),
                                            Err(e) => return Err(e),
                                        }
                                    }
                                    Ok(result)
                                } else {
                                    Err(::syn::Error::new(mac.span(), "Invalid vec! macro syntax"))
                                }
                            } else {
                                Err(::syn::Error::new(mac.span(), "Expected vec! macro or array literal"))
                            }
                        }
                        // Handle [...] array literals
                        else if let ::syn::Expr::Array(array) = expr {
                            let mut result = Vec::new();
                            for elem in array.elems.iter() {
                                match #inner_parser {
                                    Ok(val) => result.push(val),
                                    Err(e) => return Err(e),
                                }
                            }
                            Ok(result)
                        } else {
                            Err(::syn::Error::new(expr.span(), "Expected vec! macro or array literal [...]"))
                        }
                    }
                }
            } else {
                // Fallback if we can't extract inner type
                quote! { <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name) }
            }
        }
        TypeKind::Option(_inner) => {
            // This case is for Option types that are NOT inside a tuple variant,
            // e.g., in a struct.
            if let Type::Path(type_path) = field_type {
                if let Some(inner_ty) = crate::helpers::extract_option_inner_type(type_path) {
                    let parse_inner = generate_type_parser(inner_ty, quote! { inner_arg });
                    return quote! {
                        {
                            use ::syn::spanned::Spanned;
                            match #arg_name {
                                ::syn::Expr::Path(p) if p.path.is_ident("None") => Ok(None),
                                ::syn::Expr::Call(call) if call.func.to_token_stream().to_string() == "Some" && call.args.len() == 1 => {
                                    let inner_arg = &call.args[0];
                                    #parse_inner.map(Some)
                                }
                                other => {
                                    // Implicit Some
                                    let inner_arg = other;
                                    #parse_inner.map(Some)
                                }
                            }
                        }
                    };
                }
            }
            // Fallback
            quote! { <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name) }
        }
        TypeKind::HashMap(_, _) | TypeKind::BTreeMap(_, _) => {
            // Extract the actual key and value types for recursive parsing
            if let Some((key_ty, val_ty)) = extract_map_types(field_type) {
                let key_parser = generate_type_parser(key_ty, quote! { key_expr });
                let val_parser = generate_type_parser(val_ty, quote! { val_expr });

                // Determine if it's HashMap or BTreeMap
                let is_hashmap = matches!(type_kind, TypeKind::HashMap(_, _));
                let map_constructor = if is_hashmap {
                    quote! { ::std::collections::HashMap::new() }
                } else {
                    quote! { ::std::collections::BTreeMap::new() }
                };

                quote! {
                    {
                        use ::syn::spanned::Spanned;
                        let expr = Self::unwrap_expr(#arg_name);

                        if let ::syn::Expr::Array(array) = expr {
                            let mut map = #map_constructor;

                            for elem in array.elems.iter() {
                                if let ::syn::Expr::Tuple(tuple) = elem {
                                    if tuple.elems.len() == 2 {
                                        let key_expr = &tuple.elems[0];
                                        let val_expr = &tuple.elems[1];

                                        match (#key_parser, #val_parser) {
                                            (Ok(k), Ok(v)) => { map.insert(k, v); },
                                            (Err(e), _) => return Err(e),
                                            (_, Err(e)) => return Err(e),
                                        }
                                    } else {
                                        return Err(::syn::Error::new(tuple.span(), "Map entry must be a tuple of (key, value)"));
                                    }
                                } else {
                                    return Err(::syn::Error::new(elem.span(), "Map entry must be a tuple of (key, value)"));
                                }
                            }
                            Ok(map)
                        } else {
                            Err(::syn::Error::new(expr.span(), "Expected an array of tuples [(k, v), ...] for map"))
                        }
                    }
                }
            } else {
                // Fallback if we can't extract types
                quote! { <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name) }
            }
        }
        _ => {
            // For unknown types, assume they implement TryFrom<&syn::Expr>
            quote! {
                <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name)
            }
        }
    }
}

pub fn generate_struct_field_parsing(
    variant_name: &syn::Ident,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_parsers: Vec<_> = fields.named.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let name_str = name.to_string();
        let ty = &f.ty;

        let is_option = if let Type::Path(tp) = ty {
            is_option_type(tp)
        } else {
            false
        };

        if is_option {
            let inner_parser = generate_type_parser(ty, quote! { field_expr });
            quote! {
                let #name = if let Some(field_expr) = field_map.get(#name_str) {
                    match #inner_parser {
                        Ok(val) => val,
                        Err(e) => return Err(::syn::Error::new(
                            field_expr.span(),
                            format!("Failed to parse optional field '{}': {}", #name_str, e)
                        )),
                    }
                } else {
                    None
                };
            }
        } else {
            let parser = generate_type_parser(ty, quote! { field_expr });
            quote! {
                let #name = {
                    let field_expr = field_map.get(#name_str)
                        .ok_or_else(|| ::syn::Error::new(
                            struct_expr.span(),
                            format!("Missing required field '{}' for variant '{}'", #name_str, stringify!(#variant_name))
                        ))?;
                    match #parser {
                        Ok(val) => val,
                        Err(e) => return Err(::syn::Error::new(
                            field_expr.span(),
                            format!("Failed to parse field '{}': {}", #name_str, e)
                        )),
                    }
                };
            }
        }
    }).collect();

    let field_names: Vec<_> = fields.named.iter().map(|f| &f.ident).collect();

    quote! {
        {
            use ::syn::spanned::Spanned;

            let mut field_map = ::std::collections::HashMap::new();
            for field in struct_expr.fields.iter() {
                if let ::syn::Member::Named(name) = &field.member {
                    field_map.insert(name.to_string(), &field.expr);
                }
            }

            #(#field_parsers)*

            Ok(Self::#variant_name {
                #(#field_names,)*
            })
        }
    }
}

pub fn generate_arg_processing(
    variant_name: &syn::Ident,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
) -> proc_macro2::TokenStream {
    let field_count = fields.len();

    // Special case for single-argument Option<T> to allow implicit Some(v)
    if field_count == 1 {
        let field = fields.first().unwrap();
        if let Type::Path(type_path) = &field.ty {
            if is_option_type(type_path) {
                if let Some(inner_ty) = crate::helpers::extract_option_inner_type(type_path) {
                    let parse_inner = generate_type_parser(inner_ty, quote! { inner_arg });
                    return quote! {
                        {
                            use ::syn::spanned::Spanned;
                            if call_expr.args.len() != 1 {
                                return Err(::syn::Error::new(call_expr.span(), "Variant expects one argument for Option<T>"));
                            }
                            let arg = &call_expr.args[0];
                            match arg {
                                // None
                                ::syn::Expr::Path(p) if p.path.is_ident("None") => Ok(Self::#variant_name(None)),
                                // Some(inner)
                                ::syn::Expr::Call(call) if call.func.to_token_stream().to_string() == "Some" && call.args.len() == 1 => {
                                    let inner_arg = &call.args[0];
                                    #parse_inner.map(|v| Self::#variant_name(Some(v)))
                                }
                                // Implicit Some: treat a bare value as Some(...)
                                other => {
                                    let inner_arg = other;
                                    #parse_inner.map(|v| Self::#variant_name(Some(v)))
                                }
                            }
                        }
                    };
                }
            }
        }
    }

    // General case for any number of arguments
    let parsers: Vec<_> = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let field_type = &field.ty;
            let arg_name = quote! { &call_expr.args[#i] };
            generate_type_parser(field_type, arg_name)
        })
        .collect();

    let assignments: Vec<_> = (0..field_count)
        .map(|i| {
            let var_name =
                proc_macro2::Ident::new(&format!("val{}", i), proc_macro2::Span::call_site());
            let parser = &parsers[i];
            quote! {
                let #var_name = match #parser {
                    Ok(val) => val,
                    Err(e) => return Err(::syn::Error::new(
                        call_expr.args[#i].span(),
                        format!("Failed to parse argument {}: {}", #i + 1, e)
                    )),
                };
            }
        })
        .collect();

    let constructor_args = (0..field_count)
        .map(|i| proc_macro2::Ident::new(&format!("val{}", i), proc_macro2::Span::call_site()));

    quote! {
        {
            use ::syn::spanned::Spanned;
            if call_expr.args.len() != #field_count {
                return Err(::syn::Error::new(
                    call_expr.span(),
                    format!("Variant '{}' expects {} argument(s), but {} were provided",
                        stringify!(#variant_name), #field_count, call_expr.args.len())
                ));
            }

            #(#assignments)*

            Ok(Self::#variant_name(#(#constructor_args),*))
        }
    }
}

fn generate_numeric_parsers() -> proc_macro2::TokenStream {
    let mut parsers = proc_macro2::TokenStream::new();

    let int_types_signed = [
        ("parse_i8_literal", "i8"),
        ("parse_i16_literal", "i16"),
        ("parse_i32_literal", "i32"),
        ("parse_i64_literal", "i64"),
        ("parse_isize_literal", "isize"),
    ];

    for (fn_name, type_name) in int_types_signed.iter() {
        let fn_ident = syn::Ident::new(fn_name, proc_macro2::Span::call_site());
        let type_ident = syn::Ident::new(type_name, proc_macro2::Span::call_site());
        parsers.extend(quote! {
            pub fn #fn_ident(expr: &::syn::Expr) -> Result<#type_ident, ::syn::Error> {
                use ::syn::spanned::Spanned;
                let expr = Self::unwrap_expr(expr);
                match expr {
                    ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                        lit_int.base10_parse::<#type_ident>().map_err(|e| {
                            ::syn::Error::new(lit_int.span(), format!("Invalid {}: {}", #type_name, e))
                        })
                    }
                    ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr: inner_expr, .. }) => {
                        if let ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) = &**inner_expr {
                            lit_int.base10_parse::<#type_ident>().map(|n| -n).map_err(|e| {
                                ::syn::Error::new(lit_int.span(), format!("Invalid {}: {}", #type_name, e))
                            })
                        } else {
                            Err(::syn::Error::new(inner_expr.span(), "Expected an integer literal after '-'"))
                        }
                    }
                    _ => Err(::syn::Error::new(expr.span(), "Expected an integer literal")),
                }
            }
        });
    }

    let int_types_unsigned = [
        ("parse_u8_literal", "u8"),
        ("parse_u16_literal", "u16"),
        ("parse_u32_literal", "u32"),
        ("parse_u64_literal", "u64"),
        ("parse_usize_literal", "usize"),
    ];

    for (fn_name, type_name) in int_types_unsigned.iter() {
        let fn_ident = syn::Ident::new(fn_name, proc_macro2::Span::call_site());
        let type_ident = syn::Ident::new(type_name, proc_macro2::Span::call_site());
        parsers.extend(quote! {
            pub fn #fn_ident(expr: &::syn::Expr) -> Result<#type_ident, ::syn::Error> {
                use ::syn::spanned::Spanned;
                let expr = Self::unwrap_expr(expr);
                match expr {
                    ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                        lit_int.base10_parse::<#type_ident>().map_err(|e| {
                            ::syn::Error::new(lit_int.span(), format!("Invalid {}: {}", #type_name, e))
                        })
                    }
                    _ => Err(::syn::Error::new(expr.span(), "Expected an integer literal")),
                }
            }
        });
    }

    let float_types = [("parse_f32_literal", "f32"), ("parse_f64_literal", "f64")];

    for (fn_name, type_name) in float_types.iter() {
        let fn_ident = syn::Ident::new(fn_name, proc_macro2::Span::call_site());
        let type_ident = syn::Ident::new(type_name, proc_macro2::Span::call_site());
        parsers.extend(quote! {
            pub fn #fn_ident(expr: &::syn::Expr) -> Result<#type_ident, ::syn::Error> {
                use ::syn::spanned::Spanned;
                let expr = Self::unwrap_expr(expr);
                match expr {
                    ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                        lit_float.base10_parse::<#type_ident>().map_err(|e| {
                            ::syn::Error::new(lit_float.span(), format!("Invalid {}: {}", #type_name, e))
                        })
                    }
                    ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                        lit_int.base10_parse::<#type_ident>().map_err(|e| {
                            ::syn::Error::new(lit_int.span(), format!("Invalid {}: {}", #type_name, e))
                        })
                    }
                    ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr: inner_expr, .. }) => {
                         match &**inner_expr {
                            ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                                lit_float.base10_parse::<#type_ident>().map(|n| -n).map_err(|e| {
                                    ::syn::Error::new(lit_float.span(), format!("Invalid {}: {}", #type_name, e))
                                })
                            }
                            ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                                lit_int.base10_parse::<#type_ident>().map(|n| -n).map_err(|e| {
                                    ::syn::Error::new(lit_int.span(), format!("Invalid {}: {}", #type_name, e))
                                })
                            }
                            _ => Err(::syn::Error::new(inner_expr.span(), "Expected numeric literal after '-'")),
                        }
                    }
                    _ => Err(::syn::Error::new(expr.span(), "Expected a numeric literal")),
                }
            }
        });
    }

    parsers
}

// Generate helper functions for parsing
pub fn generate_helper_functions() -> proc_macro2::TokenStream {
    let numeric_parsers = generate_numeric_parsers();

    quote! {
        // Helper to unwrap Expr::Paren and Expr::Group
        pub fn unwrap_expr(expr: &::syn::Expr) -> &::syn::Expr {
            match expr {
                ::syn::Expr::Paren(paren) => Self::unwrap_expr(&paren.expr),
                ::syn::Expr::Group(group) => Self::unwrap_expr(&group.expr),
                _ => expr,
            }
        }

        pub fn determine_enum_type(expr: &::syn::Expr) -> Result<String, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Path(path_expr) => {
                    let path = &path_expr.path;
                    if path.segments.len() >= 2 {
                        let enum_name = &path.segments[path.segments.len() - 2].ident;
                        Ok(enum_name.to_string())
                    } else if path.segments.len() == 1 {
                        let variant_name = &path.segments[0].ident;
                        Ok(variant_name.to_string()) // Infer from variant
                    } else {
                        Err(::syn::Error::new(path.span(), "Invalid path format"))
                    }
                }
                ::syn::Expr::Call(call_expr) => {
                    if let ::syn::Expr::Path(path_expr) = &*call_expr.func {
                        let path = &path_expr.path;
                        if path.segments.len() >= 2 {
                            let enum_name = &path.segments[path.segments.len() - 2].ident;
                            Ok(enum_name.to_string())
                        } else {
                            Err(::syn::Error::new(path.span(), "Call must be qualified: Enum::Variant()"))
                        }
                    } else {
                        Err(::syn::Error::new(call_expr.span(), "Call expression must use a path"))
                    }
                }
                ::syn::Expr::Lit(_) => Err(::syn::Error::new(expr.span(), "Cannot determine enum type from a literal")),
                _ => Err(::syn::Error::new(expr.span(), "Unsupported expression for type determination")),
            }
        }


        pub fn parse_string_literal(expr: &::syn::Expr) -> Result<String, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Str(lit_str), .. }) => {
                    Ok(lit_str.value())
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected a string literal (e.g., \"text\")"
                )),
            }
        }

        pub fn parse_bool_literal(expr: &::syn::Expr) -> Result<bool, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Bool(lit_bool), .. }) => {
                    Ok(lit_bool.value)
                }
                ::syn::Expr::Path(path) if path.path.is_ident("true") => Ok(true),
                ::syn::Expr::Path(path) if path.path.is_ident("false") => Ok(false),
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected a boolean literal (true or false)"
                )),
            }
        }

        pub fn parse_char_literal(expr: &::syn::Expr) -> Result<char, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Char(lit_char), .. }) => {
                    Ok(lit_char.value())
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected a character literal (e.g., 'a')"
                )),
            }
        }

        #numeric_parsers
    }
}
