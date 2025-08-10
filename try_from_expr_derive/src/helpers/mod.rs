pub mod types;

use syn::{Fields, Type};

pub fn is_option_type(type_path: &syn::TypePath) -> bool {
    type_path
        .path
        .segments
        .last()
        .map(|seg| seg.ident == "Option")
        .unwrap_or(false)
}

pub fn extract_option_inner_type(type_path: &syn::TypePath) -> String {
    type_path
        .path
        .segments
        .last()
        .and_then(|seg| {
            if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                args.args.first().and_then(|arg| {
                    if let syn::GenericArgument::Type(Type::Path(inner)) = arg {
                        inner.path.segments.last().map(|s| s.ident.to_string())
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        })
        .unwrap_or_else(|| "unknown".to_string())
}

// Extract the type name from a Type for matching
pub fn extract_type_name(ty: &Type) -> String {
    match ty {
        Type::Path(type_path) => type_path
            .path
            .segments
            .last()
            .map(|seg| seg.ident.to_string())
            .unwrap_or_else(|| "unknown".to_string()),
        _ => "unknown".to_string(),
    }
}

// Detect if this is a wrapper enum (all variants are single-field tuples)
pub fn detect_wrapper_enum(data: &syn::DataEnum) -> bool {
    if data.variants.is_empty() {
        return false;
    }

    data.variants.iter().all(
        |variant| matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1),
    )
}
