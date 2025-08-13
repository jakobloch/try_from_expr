use syn::Type as SynType;

pub enum TypeKind {
    String,
    Char,
    Bool,
    Unit,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    DateTime,
    Duration,
    Timezone,
    Decimal,
    OrderedFloat(Box<TypeKind>), // Wraps F32 or F64
    Tuple(Vec<TypeKind>),
    Struct(Vec<(String, TypeKind)>),
    Option(Box<TypeKind>),
    Vec(Box<TypeKind>),
    HashMap(Box<TypeKind>, Box<TypeKind>),
    BTreeMap(Box<TypeKind>, Box<TypeKind>),
    Other(String),
}

impl TypeKind {
    pub fn parse_syn_ty(ty: &SynType) -> TypeKind {
        use quote::ToTokens;

        match ty {
            // Path types (most common - String, Vec<T>, custom types, etc.)
            SynType::Path(tp) => Self::handle_type_path(tp),

            // Tuple types - (T1, T2, ...) or unit ()
            SynType::Tuple(t) => Self::handle_tuple(t),

            // [T] -> treat as Vec<T>
            SynType::Slice(s) => TypeKind::Vec(Box::new(Self::parse_syn_ty(&s.elem))),

            // [T; N] -> treat as Vec<T> (ignore size)
            SynType::Array(arr) => TypeKind::Vec(Box::new(Self::parse_syn_ty(&arr.elem))),

            // &T or &mut T -> parse inner type
            SynType::Reference(r) => Self::parse_syn_ty(&r.elem),

            // *const T or *mut T -> parse inner type
            SynType::Ptr(p) => Self::parse_syn_ty(&p.elem),

            // (T) -> unwrap parentheses
            SynType::Paren(p) => Self::parse_syn_ty(&p.elem),

            // Group tokens -> unwrap
            SynType::Group(g) => Self::parse_syn_ty(&g.elem),

            // impl Trait -> fallback to string
            SynType::ImplTrait(it) => TypeKind::Other(it.to_token_stream().to_string()),

            // dyn Trait -> fallback to string
            SynType::TraitObject(to) => TypeKind::Other(to.to_token_stream().to_string()),

            // fn(...) -> ... -> fallback to string
            SynType::BareFn(f) => TypeKind::Other(f.to_token_stream().to_string()),

            // _ (infer) -> fallback to string
            SynType::Infer(i) => TypeKind::Other(i.to_token_stream().to_string()),

            // ! (never) -> fallback to string
            SynType::Never(n) => TypeKind::Other(n.to_token_stream().to_string()),

            // type_macro!(...) -> fallback to string
            SynType::Macro(m) => TypeKind::Other(m.to_token_stream().to_string()),

            // Verbatim tokens from macro expansion -> fallback to string
            SynType::Verbatim(ts) => TypeKind::Other(ts.to_string()),

            // Future-proofing for new syn variants
            _ => TypeKind::Other(ty.to_token_stream().to_string()),
        }
    }

    fn handle_tuple(t: &syn::TypeTuple) -> TypeKind {
        if t.elems.is_empty() {
            // Unit type ()
            TypeKind::Unit
        } else {
            let elems = t.elems.iter().map(Self::parse_syn_ty).collect();
            TypeKind::Tuple(elems)
        }
    }

    fn handle_type_path(tp: &syn::TypePath) -> TypeKind {
        use quote::ToTokens;

        // If qualified self type (e.g., <T as Trait>::Assoc), fallback
        if tp.qself.is_some() {
            return TypeKind::Other(tp.to_token_stream().to_string());
        }

        // Get the last segment (works for both `String` and `std::string::String`)
        let last = match tp.path.segments.last() {
            Some(s) => s,
            None => return TypeKind::Other(tp.to_token_stream().to_string()),
        };

        let ident = last.ident.to_string();

        // Handle generic types with angle brackets
        if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
            // Extract only type arguments (ignore lifetimes and const generics)
            let type_args: Vec<_> = args
                .args
                .iter()
                .filter_map(|ga| match ga {
                    syn::GenericArgument::Type(t) => Some(t),
                    _ => None,
                })
                .collect();

            match ident.as_str() {
                "Option" if type_args.len() == 1 => {
                    return TypeKind::Option(Box::new(Self::parse_syn_ty(type_args[0])));
                }
                "Vec" if type_args.len() == 1 => {
                    return TypeKind::Vec(Box::new(Self::parse_syn_ty(type_args[0])));
                }
                "HashMap" if type_args.len() == 2 => {
                    return TypeKind::HashMap(
                        Box::new(Self::parse_syn_ty(type_args[0])),
                        Box::new(Self::parse_syn_ty(type_args[1])),
                    );
                }
                "BTreeMap" if type_args.len() == 2 => {
                    return TypeKind::BTreeMap(
                        Box::new(Self::parse_syn_ty(type_args[0])),
                        Box::new(Self::parse_syn_ty(type_args[1])),
                    );
                }
                "OrderedFloat" if type_args.len() == 1 => {
                    return TypeKind::OrderedFloat(Box::new(Self::parse_syn_ty(type_args[0])));
                }
                "DateTime" => {
                    // DateTime<Utc>, DateTime<Local>, etc. all become DateTime
                    return TypeKind::DateTime;
                }
                _ => {
                    // Unknown generic type, fall through to check if it's a known non-generic
                }
            }
        }

        // Match known types without generics
        match ident.as_str() {
            "String" | "str" => TypeKind::String,
            "char" => TypeKind::Char,
            "bool" => TypeKind::Bool,
            "f32" => TypeKind::F32,
            "f64" => TypeKind::F64,
            "i8" => TypeKind::I8,
            "i16" => TypeKind::I16,
            "i32" => TypeKind::I32,
            "i64" => TypeKind::I64,
            "i128" => TypeKind::I128,
            "isize" => TypeKind::Isize,
            "u8" => TypeKind::U8,
            "u16" => TypeKind::U16,
            "u32" => TypeKind::U32,
            "u64" => TypeKind::U64,
            "u128" => TypeKind::U128,
            "usize" => TypeKind::Usize,
            "DateTime" => TypeKind::DateTime,
            "Duration" => TypeKind::Duration,
            "Tz" | "Timezone" => TypeKind::Timezone,
            "Decimal" => TypeKind::Decimal,
            _ => {
                // Unknown type - store as Other
                let type_str = tp.to_token_stream().to_string();
                TypeKind::Other(type_str)
            }
        }
    }
}
