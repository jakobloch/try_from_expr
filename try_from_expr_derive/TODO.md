[] Handle `Self::Variant` and UFCS paths

Leaf parsing fails if users write `Self::Email` inside an `impl`. Patch the leaf
path branch:

```rust
let enum_ident_str = stringify!(#enum_name);
let enum_name_str = enum_name_seg.to_string();
let matches_enum = enum_name_str == enum_ident_str || enum_name_str == "Self";

if matches_enum { /* existing match on variants */ } else {
    // Also allow UFCS like <StringRule as Trait>::Variant if you care:
    // walk the path to find a segment == enum_ident_str.
}
```

[] Make the wrapper “fast path” less brittle

`determine_enum_type` + `extract_type_name` both use only the last ident. If two
child types share a name in different modules, dispatch gets ambiguous. You can
try both a **full path key** and a **last-segment key**:

-   For the expr: capture `"crate::rules::text::StringRule"` _and_
    `"StringRule"`.
-   For the field type: capture the same pair.
-   Match on full path first; fall back to last segment; else drop to your
    fallback loop.

If you don’t want to stringify full paths, your current “fast path → fallback”
is fine; just be aware.

[] Aggregate fallback errors (great UX)

Right now the fallback returns a generic “no match” error. Collect and combine
child parse errors:

```rust
let mut acc: Option<::syn::Error> = None;
#(#{
    let attempt = { #parse_expr };
    match attempt {
        Ok(val) => return Ok(#enum_name::#variant_name(val)),
        Err(e) => {
            if let Some(mut a) = acc.take() { a.combine(e); acc = Some(a); }
            else { acc = Some(e); }
        }
    }
})*
return Err(acc.unwrap_or_else(|| ::syn::Error::new(expr.span(),
    format!("No variant of {} could parse this expression", stringify!(#enum_name)))));
```

[] Negative floats in leaves

You handle negative `i64` but not `f64`. Mirror the unary `-` handling for
floats in your helpers:

```rust
fn parse_f64_like(expr: &Expr) -> Result<f64, Error> {
    match expr {
        Expr::Lit(ExprLit { lit: Lit::Float(f), .. }) => f.base10_parse::<f64>().map_err(|e| err(expr, e)),
        Expr::Lit(ExprLit { lit: Lit::Int(i), .. }) => i.base10_parse::<f64>().map_err(|e| err(expr, e)),
        Expr::Unary(ExprUnary { op: UnOp::Neg(_), expr, .. }) => {
            match &**expr {
                Expr::Lit(ExprLit { lit: Lit::Float(f), .. }) =>
                    f.base10_parse::<f64>().map(|v| -v).map_err(|e| err(expr, e)),
                Expr::Lit(ExprLit { lit: Lit::Int(i), .. }) =>
                    i.base10_parse::<f64>().map(|v| -v).map_err(|e| err(expr, e)),
                _ => Err(err(expr, "Expected numeric literal")),
            }
        }
        _ => Err(err(expr, "Expected numeric literal")),
    }
}
```

Then make `OrderedFloat<f64>` from it when needed.

[] Wrapper detection: keep the escape hatch

Your `detect_wrapper_enum` heuristic is fine, but keep a forcing attr like
`#[try_from_expr(wrapper)]` and `#[try_from_expr(leaf)]` for edge cases. (You
already parse attributes on the derive, so this is easy to wire in.)

[] Support “simple” wrapper fields cleanly

You went the extra mile to allow wrapper variants whose field type is a
primitive (parse directly) _or_ an enum (use `TryFrom`). That’s great. Just
ensure `generate_type_parser` always returns `Result<#ty, _>` so the two code
paths are syntactically identical in both fast path and fallback (looks like you
already did that).

[] Named/struct variants: friendlier error

Right now they’re ignored. Emit a compile error at derive time if a wrapper
variant isn’t unit or single-tuple:

```rust
if let Fields::Named(_) = &variant.fields {
    return Error::new_spanned(&variant.ident, "Struct variants are not supported").to_compile_error().into();
}
```

[] Tiny polish

-   In the wrapper fast path you build a `result` then
    `if result.is_ok() { return result; }`. Consider
    `if let Ok(v) = result { return Ok(v); }` so you don’t keep an
    `Err("No matching type")` around.
-   Consider implementing `impl TryFrom<::syn::Expr> for #enum_name` too
    (by-reference impl is enough, but the owned one is a nice convenience).

Nice—this is getting really close. A few correctness fixes and small refactors
will make it solid:

[] Fast-path match uses an **identifier**, not a **string literal**

In `generate_wrapper_enum_impl`, the arm:

```rust
quote! {
    #type_name => #parse_expr
        .map(#enum_name::#variant_name)
        .map_err(|e| ::syn::Error::new(expr.span(), format!("Failed to parse {}: {}", #type_name, e))),
}
```

emits `StringRule => { ... }` (an identifier), but you’re matching on
`enum_type.as_str()`. You must match a **string literal**:

```rust
let type_name_lit = syn::LitStr::new(&type_name, proc_macro2::Span::call_site());
quote! {
    #type_name_lit => #parse_expr
        .map(#enum_name::#variant_name)
        .map_err(|e| ::syn::Error::new(expr.span(), format!("Failed to parse {}: {}", #type_name_lit, e))),
}
```

(Do the same anywhere you compare against strings.)

[] `Option<T>` parsing should be **AST-driven**, not string-driven

This bit:

```rust
let inner_type = extract_option_inner_type(type_path); // -> String name
...
match Self::parse_option_inner(arg, &(#inner_type)) { ... }
```

is brittle (paths, generics, type aliases). Generate code that matches `None` /
`Some(expr)` and reuses the **real inner type**:

```rust
// inside the Option<T> branch of generate_arg_processing
let inner_ty = /* extract the actual syn::Type of T */;
let parse_inner = generate_type_parser(&inner_ty, quote! { inner_arg });

quote! {
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

            // (optional) implicit Some: treat a bare literal as Some(...)
            other => {
                let inner_arg = other;
                match #parse_inner {
                    Ok(v) => Ok(Self::#variant_name(Some(v))),
                    Err(e) => Err(e),
                }
            }
        }
    }
}
```

Then delete `parse_option_inner` (it’s no longer needed) and the current
`parse_option_literal` placeholder.

[] `OrderedFloat<T>` should respect **inner T**

`TypeKind::OrderedFloat(_)` always calls `parse_ordered_float_literal` which
returns `OrderedFloat<f64>`. If the inner is `f32`, that’s wrong. Make
`generate_type_parser` branch on the inner:

```rust
TypeKind::OrderedFloat(inner) => match &*inner {
    TypeKind::F32 => quote! {
        Self::parse_f32_literal(#arg_name).map(::ordered_float::OrderedFloat::<f32>)
    },
    _ => quote! {
        Self::parse_f64_literal(#arg_name).map(::ordered_float::OrderedFloat::<f64>)
    },
},
```

(You can keep your specialized negative-float handling in `parse_f32_literal` /
`parse_f64_literal`.)

[] Wrapper detection: keep an **escape hatch**

`detect_wrapper_enum` = “all variants are single-field tuples” is fine as a
default. But keep attributes to force mode:

-   `#[try_from_expr(wrapper)]` → wrapper
-   `#[try_from_expr(leaf)]` → leaf

Heuristics will misclassify mixed enums; the attrs prevent surprises.

[] Improve wrapper fallback UX (combine errors)

Right now you return a generic “No matching enum variant…”. Aggregate child
errors:

```rust
let mut acc: Option<::syn::Error> = None;
#(
{
    match { #parse_expr } {
        Ok(v) => return Ok(#enum_name::#variant_name(v)),
        Err(e) => {
            if let Some(mut a) = acc.take() { a.combine(e); acc = Some(a); }
            else { acc = Some(e); }
        }
    }
}
)*
return Err(acc.unwrap_or_else(|| ::syn::Error::new(
    expr.span(),
    format!("No variant of {} could parse this expression", stringify!(#enum_name))
)));
```

[] Support `Self::Variant` in leaf parsing

In `determine_enum_type` and the leaf `Expr::Path` branch, treat `Self` as the
current enum:

```rust
let enum_ident_str = stringify!(#enum_name);
let enum_name_str = enum_name_seg.to_string();
let matches_enum = enum_name_str == enum_ident_str || enum_name_str == "Self";
```

[] Consider re-exporting `syn`

Generated code uses `::syn::...`. That forces downstream crates to depend on
`syn`. If you want to avoid that, re-export and emit
`::your_macro_crate::__syn::...` (or resolve via `proc_macro_crate`).

[] Placeholders that currently just error

-   `parse_vec_literal`, `parse_map_literal`, `parse_option_literal` return
    errors. That’s fine if intentional; otherwise, remove branches that generate
    calls to them until you implement them, or the wrapper will silently fail
    for those types.

[] Minor polish

-   `parse_typed_argument<T>` is unused here—either use it (e.g., in 2-arg
    fallbacks) or drop it.
-   For struct variants in leaves/wrappers, emit a compile error at derive-time
    (clear message).
-   `extract_type_name` only returns the last segment; for the fast path, it’s
    okay, but you might want a `full_type_key(ty) -> String` to prefer full
    paths when the expr gives one.
