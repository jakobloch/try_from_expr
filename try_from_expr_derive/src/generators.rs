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
                    let types: Vec<&Type> = args.args.iter().filter_map(|arg| {
                        if let syn::GenericArgument::Type(ty) = arg {
                            Some(ty)
                        } else {
                            None
                        }
                    }).collect();
                    
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
            // Options are handled specially in generate_arg_processing
            // This shouldn't be reached, but provide a fallback
            quote! {
                Err(::syn::Error::new(
                    #arg_name.span(),
                    "Option types should be handled by generate_arg_processing"
                ))
            }
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
                        
                        // Handle HashMap::from([...]) or BTreeMap::from([...])
                        if let ::syn::Expr::Call(call) = expr {
                            // Check if it's a from() call
                            if let ::syn::Expr::Path(path) = &*call.func {
                                let segments: Vec<_> = path.path.segments.iter().map(|s| s.ident.to_string()).collect();
                                
                                // Check for HashMap::from or BTreeMap::from pattern
                                if segments.len() >= 2 && segments[segments.len() - 1] == "from" {
                                    if call.args.len() == 1 {
                                        // Parse the array of tuples
                                        if let ::syn::Expr::Array(array) = &call.args[0] {
                                            let mut map = #map_constructor;
                                            
                                            for elem in array.elems.iter() {
                                                // Each element should be a tuple (key, value)
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
                                                        return Err(::syn::Error::new(
                                                            tuple.span(),
                                                            "Map entry must be a tuple of (key, value)"
                                                        ));
                                                    }
                                                } else {
                                                    return Err(::syn::Error::new(
                                                        elem.span(),
                                                        "Map entry must be a tuple of (key, value)"
                                                    ));
                                                }
                                            }
                                            Ok(map)
                                        } else {
                                            Err(::syn::Error::new(
                                                call.args[0].span(),
                                                "Expected array literal in from() call"
                                            ))
                                        }
                                    } else {
                                        Err(::syn::Error::new(call.span(), "from() expects exactly one argument"))
                                    }
                                } else {
                                    // Fallback to TryFrom
                                    <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name)
                                }
                            } else {
                                // Fallback to TryFrom
                                <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name)
                            }
                        }
                        // Handle direct array of tuples: [(key, value), ...]
                        else if let ::syn::Expr::Array(array) = expr {
                            let mut map = #map_constructor;
                            
                            for elem in array.elems.iter() {
                                // Each element should be a tuple (key, value)
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
                                        return Err(::syn::Error::new(
                                            tuple.span(),
                                            "Map entry must be a tuple of (key, value)"
                                        ));
                                    }
                                } else {
                                    return Err(::syn::Error::new(
                                        elem.span(),
                                        "Map entry must be a tuple of (key, value)"
                                    ));
                                }
                            }
                            Ok(map)
                        } else {
                            Err(::syn::Error::new(
                                expr.span(),
                                "Expected HashMap::from([...]), BTreeMap::from([...]), or array of tuples [(k, v), ...]"
                            ))
                        }
                    }
                }
            } else {
                // Fallback if we can't extract types
                quote! { <#field_type as TryFrom<&::syn::Expr>>::try_from(#arg_name) }
            }
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

pub fn generate_struct_field_parsing(
    variant_name: &syn::Ident,
    fields: &syn::FieldsNamed,
) -> proc_macro2::TokenStream {
    let field_names: Vec<_> = fields.named.iter().map(|f| &f.ident).collect();
    let field_types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();
    
    let field_parsers: Vec<_> = field_names.iter().zip(field_types.iter()).map(|(name, ty)| {
        let name_str = name.as_ref().unwrap().to_string();
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
    }).collect();
    
    quote! {
        {
            use ::syn::spanned::Spanned;
            
            // Build a map of field names to expressions
            let mut field_map = ::std::collections::HashMap::new();
            for field in struct_expr.fields.iter() {
                if let ::syn::Member::Named(name) = &field.member {
                    field_map.insert(name.to_string(), &field.expr);
                }
            }
            
            // Parse each field
            #(#field_parsers)*
            
            // Construct the variant
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
    let field_types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    if field_types.len() == 1 {
        let field_type = &field_types[0];

        // Special handling for Option types
        if let Type::Path(type_path) = field_type {
            if is_option_type(type_path) {
                // Extract the actual inner Type from Option<T>
                let inner_ty = type_path.path.segments.last().and_then(|seg| {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        args.args.first().and_then(|arg| {
                            if let syn::GenericArgument::Type(ty) = arg {
                                Some(ty)
                            } else {
                                None
                            }
                        })
                    } else {
                        None
                    }
                });

                if let Some(inner_ty) = inner_ty {
                    let parse_inner = generate_type_parser(inner_ty, quote! { inner_arg });

                    return quote! {
                        {
                            use ::syn::spanned::Spanned;
                            let arg = &call_expr.args[0];
                            match arg {
                                // None
                                ::syn::Expr::Path(p) if p.path.is_ident("None") => Ok(Self::#variant_name(None)),

                                // Some(inner)
                                ::syn::Expr::Call(call) => {
                                    if let ::syn::Expr::Path(p) = &*call.func {
                                        if p.path.is_ident("Some") && call.args.len() == 1 {
                                            let inner_arg = &call.args[0];
                                            match #parse_inner {
                                                Ok(v) => Ok(Self::#variant_name(Some(v))),
                                                Err(e) => Err(e),
                                            }
                                        } else {
                                            Err(::syn::Error::new(call.span(), "Expected Some(value) or None"))
                                        }
                                    } else {
                                        Err(::syn::Error::new(call.span(), "Expected Some(value) or None"))
                                    }
                                }

                                // Implicit Some: treat a bare value as Some(...)
                                other => {
                                    let inner_arg = other;
                                    match #parse_inner {
                                        Ok(v) => Ok(Self::#variant_name(Some(v))),
                                        Err(_) => Err(::syn::Error::new(other.span(), "Expected Some(value) or None"))
                                    }
                                }
                            }
                        }
                    };
                }
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



    }
}
