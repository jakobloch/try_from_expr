# try_from_expr_derive

The procedural macro implementation for `try_from_expr`. This crate provides the
`TryFromExpr` derive macro.

## Usage

This crate is typically not used directly. Instead, use the main `try_from_expr`
crate which re-exports this macro:

```toml
[dependencies]
try_from_expr = "0.1.0"
```

## Implementation Details

This crate implements the `TryFromExpr` derive macro that generates
`TryFrom<&syn::Expr>` implementations for enums. It handles:

-   **Variant Detection**: Analyzes enum variants (unit, tuple, struct)
-   **Mode Detection**: Automatically determines if an enum is a "wrapper" or
    "leaf" enum
-   **Parser Generation**: Creates type-specific parsers for each field
-   **Error Handling**: Generates helpful error messages for parsing failures

### Wrapper vs Leaf Enums

The macro distinguishes between two types of enums:

**Leaf Enums**: Enums with concrete values

-   Parse exact variant names
-   Validate enum membership
-   Extract and parse parameters

**Wrapper Enums**: Enums that contain other enum types

-   Try type-directed dispatch first
-   Fall back to trying each child type
-   Aggregate error messages

### Generated Code

For each enum, the macro generates:

1. `TryFrom<&syn::Expr>` implementation
2. Helper methods for expression unwrapping
3. Type-specific parsing functions
4. Error aggregation logic

## License

MIT
