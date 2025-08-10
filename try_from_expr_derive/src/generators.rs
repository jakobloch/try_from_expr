use crate::helpers::{extract_option_inner_type, is_option_type, types::TypeKind};
use quote::quote;
use syn::Type;

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
        TypeKind::OrderedFloat(_) => quote! { Self::parse_ordered_float_literal(#arg_name) },
        TypeKind::Vec(_inner) => {
            // For Vec types, we need to parse array literals
            quote! { Self::parse_vec_literal::<#field_type>(#arg_name) }
        }
        TypeKind::Option(_inner) => {
            // Options are already handled separately in generate_arg_processing
            quote! { Self::parse_option_literal::<#field_type>(#arg_name) }
        }
        TypeKind::HashMap(_, _) | TypeKind::BTreeMap(_, _) => {
            // Maps need special parsing
            quote! { Self::parse_map_literal::<#field_type>(#arg_name) }
        }
        _ => {
            // For unknown types, assume they implement TryFrom<&syn::Expr> if they derive TryFromExpr
            // This avoids requiring FromStr for custom types
            quote! { 
                <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name)
            }
        }
    }
}

pub fn generate_arg_processing(
    variant_name: &syn::Ident,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
) -> proc_macro2::TokenStream {
    let field_types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    if field_types.len() == 1 {
        let field_type = &field_types[0];

        // Special handling for Option types
        if let Type::Path(type_path) = field_type {
            if is_option_type(type_path) {
                // Extract inner type from Option<T>
                let inner_type = extract_option_inner_type(type_path);
                return quote! {
                    {
                        let arg = &call_expr.args[0];
                        match arg {
                            ::syn::Expr::Path(path) if path.path.is_ident("None") => {
                                Ok(Self::#variant_name(None))
                            }
                            _ => match Self::parse_option_inner(arg, &(#inner_type)) {
                                Ok(val) => Ok(Self::#variant_name(Some(val))),
                                Err(e) => Err(e.into())
                            }
                        }
                    }
                };
            }
        }

        // Use the unified type parser for all other types
        let parse_expr = generate_type_parser(field_type, quote! { arg });
        quote! {
            {
                use ::syn::spanned::Spanned;
                let arg = &call_expr.args[0];
                match #parse_expr {
                    Ok(val) => Ok(Self::#variant_name(val)),
                    Err(e) => Err(e.into())
                }
            }
        }
    } else if field_types.len() == 2 {
        let field_type1 = &field_types[0];
        let field_type2 = &field_types[1];

        // Generate appropriate parser for each argument type
        let parse_arg1 = generate_type_parser(field_type1, quote! { arg1 });
        let parse_arg2 = generate_type_parser(field_type2, quote! { arg2 });

        // Handle two-argument variants with specific type parsing
        quote! {
            {
                use ::syn::spanned::Spanned;
                let arg1 = &call_expr.args[0];
                let arg2 = &call_expr.args[1];
                match (#parse_arg1, #parse_arg2) {
                    (Ok(val1), Ok(val2)) => Ok(Self::#variant_name(val1, val2)),
                    (Err(e), _) => Err(::syn::Error::new(
                        arg1.span(),
                        format!("Failed to parse first argument for variant '{}': {}",
                            stringify!(#variant_name), e)
                    )),
                    (_, Err(e)) => Err(::syn::Error::new(
                        arg2.span(),
                        format!("Failed to parse second argument for variant '{}': {}",
                            stringify!(#variant_name), e)
                    ))
                }
            }
        }
    } else {
        let num_fields = field_types.len();
        quote! {
            Err(::syn::Error::new(
                call_expr.span(),
                format!("Variant '{}' has {} fields, which is not supported. Only 1 or 2 argument variants are supported.",
                    stringify!(#variant_name), #num_fields)
            ))
        }
    }
}

// Generate helper functions for parsing
pub fn generate_helper_functions() -> proc_macro2::TokenStream {
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
            match expr {
                ::syn::Expr::Path(path_expr) => {
                    let path = &path_expr.path;
                    if path.segments.len() >= 2 {
                        let enum_name = &path.segments[path.segments.len() - 2].ident;
                        Ok(enum_name.to_string())
                    } else if path.segments.len() == 1 {
                        // Try to infer from variant name for shorthand
                        let variant_name = &path.segments[0].ident;
                        Ok(variant_name.to_string())
                    } else {
                        Err(::syn::Error::new(
                            path.span(),
                            "Invalid path format"
                        ))
                    }
                }
                ::syn::Expr::Call(call_expr) => {
                    if let ::syn::Expr::Path(path_expr) = &*call_expr.func {
                        let path = &path_expr.path;
                        if path.segments.len() >= 2 {
                            let enum_name = &path.segments[path.segments.len() - 2].ident;
                            Ok(enum_name.to_string())
                        } else {
                            Err(::syn::Error::new(
                                path.span(),
                                "Invalid call expression format. Expected 'EnumName::VariantName(...)'"
                            ))
                        }
                    } else {
                        Err(::syn::Error::new(
                            call_expr.span(),
                            "Invalid syntax: call expression must use a path"
                        ))
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Unsupported expression type"
                )),
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
                    "Expected a string literal. Use double quotes for strings (e.g., \"text\")"
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

        pub fn parse_usize_literal(expr: &::syn::Expr) -> Result<usize, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse().map_err(|e| ::syn::Error::new(
                        lit_int.span(),
                        format!("Invalid usize: {}", e)
                    ))
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an integer literal (e.g., 42)"
                )),
            }
        }

        pub fn parse_ordered_float_literal(expr: &::syn::Expr) -> Result<::ordered_float::OrderedFloat<f64>, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                    lit_float.base10_parse::<f64>()
                        .map(::ordered_float::OrderedFloat)
                        .map_err(|e| ::syn::Error::new(
                            lit_float.span(),
                            format!("Invalid float: {}", e)
                        ))
                }
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<f64>()
                        .map(::ordered_float::OrderedFloat)
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid number: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    // Handle negative floats
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                            lit_float.base10_parse::<f64>()
                                .map(|n| ::ordered_float::OrderedFloat(-n))
                                .map_err(|e| ::syn::Error::new(
                                    lit_float.span(),
                                    format!("Invalid float: {}", e)
                                ))
                        }
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<f64>()
                                .map(|n| ::ordered_float::OrderedFloat(-n))
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid number: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected a numeric literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected a numeric literal (integer or float)"
                )),
            }
        }

        pub fn parse_f64_literal(expr: &::syn::Expr) -> Result<f64, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                    lit_float.base10_parse::<f64>()
                        .map_err(|e| ::syn::Error::new(
                            lit_float.span(),
                            format!("Invalid float: {}", e)
                        ))
                }
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<f64>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid number: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    // Handle negative floats
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                            lit_float.base10_parse::<f64>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_float.span(),
                                    format!("Invalid float: {}", e)
                                ))
                        }
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<f64>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid number: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected a numeric literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected a numeric literal (integer or float)"
                )),
            }
        }

        pub fn parse_i64_literal(expr: &::syn::Expr) -> Result<i64, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<i64>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid i64: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    // Handle negative numbers
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<i64>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid i64: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected an integer literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an integer literal"
                )),
            }
        }

        pub fn parse_i32_literal(expr: &::syn::Expr) -> Result<i32, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<i32>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid i32: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<i32>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid i32: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected an integer literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an integer literal"
                )),
            }
        }

        pub fn parse_i16_literal(expr: &::syn::Expr) -> Result<i16, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<i16>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid i16: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<i16>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid i16: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected an integer literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an integer literal"
                )),
            }
        }

        pub fn parse_i8_literal(expr: &::syn::Expr) -> Result<i8, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<i8>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid i8: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<i8>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid i8: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected an integer literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an integer literal"
                )),
            }
        }

        pub fn parse_isize_literal(expr: &::syn::Expr) -> Result<isize, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<isize>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid isize: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<isize>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid isize: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected an integer literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an integer literal"
                )),
            }
        }

        pub fn parse_u64_literal(expr: &::syn::Expr) -> Result<u64, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<u64>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid u64: {}", e)
                        ))
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an unsigned integer literal"
                )),
            }
        }

        pub fn parse_u32_literal(expr: &::syn::Expr) -> Result<u32, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<u32>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid u32: {}", e)
                        ))
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an unsigned integer literal"
                )),
            }
        }

        pub fn parse_u16_literal(expr: &::syn::Expr) -> Result<u16, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<u16>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid u16: {}", e)
                        ))
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an unsigned integer literal"
                )),
            }
        }

        pub fn parse_u8_literal(expr: &::syn::Expr) -> Result<u8, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<u8>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid u8: {}", e)
                        ))
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected an unsigned integer literal"
                )),
            }
        }

        pub fn parse_f32_literal(expr: &::syn::Expr) -> Result<f32, ::syn::Error> {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                    lit_float.base10_parse::<f32>()
                        .map_err(|e| ::syn::Error::new(
                            lit_float.span(),
                            format!("Invalid f32: {}", e)
                        ))
                }
                ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                    lit_int.base10_parse::<f32>()
                        .map_err(|e| ::syn::Error::new(
                            lit_int.span(),
                            format!("Invalid number: {}", e)
                        ))
                }
                ::syn::Expr::Unary(::syn::ExprUnary { op: ::syn::UnOp::Neg(_), expr, .. }) => {
                    match &**expr {
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Float(lit_float), .. }) => {
                            lit_float.base10_parse::<f32>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_float.span(),
                                    format!("Invalid f32: {}", e)
                                ))
                        }
                        ::syn::Expr::Lit(::syn::ExprLit { lit: ::syn::Lit::Int(lit_int), .. }) => {
                            lit_int.base10_parse::<f32>()
                                .map(|n| -n)
                                .map_err(|e| ::syn::Error::new(
                                    lit_int.span(),
                                    format!("Invalid number: {}", e)
                                ))
                        }
                        _ => Err(::syn::Error::new(
                            expr.span(),
                            "Expected a numeric literal after negative sign"
                        )),
                    }
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected a numeric literal (integer or float)"
                )),
            }
        }

        pub fn parse_vec_literal<T>(expr: &::syn::Expr) -> Result<T, ::syn::Error>
        where
            T: Default,
        {
            use ::syn::spanned::Spanned;
            // For now, return error - Vec parsing would require more complex logic
            Err(::syn::Error::new(
                expr.span(),
                "Vec parsing is not yet supported. Consider implementing TryFrom<&syn::Expr> for your Vec type"
            ))
        }

        pub fn parse_option_literal<T>(expr: &::syn::Expr) -> Result<T, ::syn::Error>
        where
            T: Default,
        {
            use ::syn::spanned::Spanned;
            // For now, return error - would need to handle Some(...) and None
            Err(::syn::Error::new(
                expr.span(),
                "Option parsing through this method is not yet supported"
            ))
        }

        pub fn parse_map_literal<T>(expr: &::syn::Expr) -> Result<T, ::syn::Error>
        where
            T: Default,
        {
            use ::syn::spanned::Spanned;
            // For now, return error - map parsing would be very complex
            Err(::syn::Error::new(
                expr.span(),
                "HashMap/BTreeMap parsing is not yet supported"
            ))
        }

        pub fn parse_option_inner(expr: &::syn::Expr, inner_type: &str) -> Result<String, ::syn::Error> {
            use ::syn::spanned::Spanned;
            // For now, handle String inner type
            match inner_type {
                "String" => Self::parse_string_literal(expr),
                _ => Err(::syn::Error::new(
                    expr.span(),
                    format!("Option<{}> parsing not yet supported", inner_type)
                )),
            }
        }

        pub fn parse_typed_argument<T>(expr: &::syn::Expr) -> Result<T, ::syn::Error>
        where
            T: std::str::FromStr,
            <T as std::str::FromStr>::Err: std::fmt::Display,
        {
            use ::syn::spanned::Spanned;
            let expr = Self::unwrap_expr(expr);

            // Try to parse using FromStr
            match expr {
                ::syn::Expr::Lit(::syn::ExprLit { lit, .. }) => {
                    let string_val = match lit {
                        ::syn::Lit::Str(s) => s.value(),
                        ::syn::Lit::Int(i) => i.to_string(),
                        ::syn::Lit::Float(f) => f.to_string(),
                        ::syn::Lit::Bool(b) => b.value.to_string(),
                        ::syn::Lit::Char(c) => c.value().to_string(),
                        _ => return Err(::syn::Error::new(
                            lit.span(),
                            "Unsupported literal type"
                        )),
                    };
                    string_val.parse().map_err(|e| ::syn::Error::new(
                        lit.span(),
                        format!("Parse error: {}", e)
                    ))
                }
                _ => Err(::syn::Error::new(
                    expr.span(),
                    "Expected a literal value"
                )),
            }
        }
    }
}
