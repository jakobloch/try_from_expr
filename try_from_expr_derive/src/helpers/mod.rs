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

    // A wrapper enum should only have single-field tuple variants
    // If there are any unit variants or struct variants, it's not a wrapper enum
    data.variants.iter().all(
        |variant| matches!(&variant.fields, Fields::Unnamed(fields) if fields.unnamed.len() == 1),
    )
}
