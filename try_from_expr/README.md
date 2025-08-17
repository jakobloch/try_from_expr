# try_from_expr

A Rust procedural macro for generating `TryFrom<&syn::Expr>` implementations for
enums. This allows you to parse Rust syntax expressions into strongly-typed enum
values, making it easy to work with configuration DSLs, macro arguments, and
other syntax-based APIs.

## Features

-   🚀 **Automatic Parser Generation** - Derives `TryFrom<&syn::Expr>` for your
    enums
-   📦 **Multiple Variant Types** - Supports unit, tuple, and struct variants
-   🎯 **Smart Type Detection** - Automatically detects wrapper vs leaf enums
-   🔧 **Flexible Parsing** - Works with primitives, collections, and custom
    types
-   💡 **Helpful Error Messages** - Provides clear error messages for parse
    failures
-   ⚡ **Zero Runtime Overhead** - All parsing logic is generated at compile
    time

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
try_from_expr = "0.1.0"
```

## Quick Start

```rust
use try_from_expr::TryFromExpr;
use syn::Expr;

#[derive(TryFromExpr)]
enum ConfigValue {
    Enabled,
    Threshold(u32),
    Range { min: i32, max: i32 },
}

// Parse from a syn::Expr
let expr: Expr = syn::parse_str("ConfigValue::Threshold(42)").unwrap();
let config = ConfigValue::try_from(&expr).unwrap();
```

## Usage Examples

### Basic Enum with Different Variant Types

```rust
#[derive(TryFromExpr)]
enum Setting {
    // Unit variant
    Default,

    // Tuple variant with single value
    Timeout(u64),

    // Tuple variant with multiple values
    Coordinate(f64, f64),

    // Struct variant
    Config {
        name: String,
        value: i32,
        enabled: bool,
    },
}

// Parse unit variant
let expr = syn::parse_str("Setting::Default").unwrap();
let setting = Setting::try_from(&expr).unwrap();

// Parse tuple variant
let expr = syn::parse_str("Setting::Timeout(5000)").unwrap();
let setting = Setting::try_from(&expr).unwrap();

// Parse struct variant
let expr = syn::parse_str(r#"Setting::Config {
    name: "test",
    value: 42,
    enabled: true
}"#).unwrap();
let setting = Setting::try_from(&expr).unwrap();
```

### Wrapper Enums (Composite Enums)

The macro automatically detects "wrapper" enums that contain other enum types
and generates optimized parsing:

```rust
#[derive(TryFromExpr)]
enum StringConstraint {
    MinLength(usize),
    MaxLength(usize),
    Pattern(String),
}

#[derive(TryFromExpr)]
enum NumberConstraint {
    Min(i64),
    Max(i64),
    Range(i64, i64),
}

#[derive(TryFromExpr)]
enum Constraint {
    String(StringConstraint),
    Number(NumberConstraint),
    Required,
}

// The macro detects this is a wrapper and allows parsing nested enums
let expr = syn::parse_str("Constraint::String(StringConstraint::MinLength(10))").unwrap();
let constraint = Constraint::try_from(&expr).unwrap();
```

### Working with Collections

```rust
#[derive(TryFromExpr)]
enum DataType {
    Single(String),
    Multiple(Vec<String>),
    Mapping(HashMap<String, i32>),
}

// Parse Vec
let expr = syn::parse_str(r#"DataType::Multiple(vec!["a", "b", "c"])"#).unwrap();
let data = DataType::try_from(&expr).unwrap();

// Parse HashMap
let expr = syn::parse_str(r#"DataType::Mapping([("key", 42)])"#).unwrap();
let data = DataType::try_from(&expr).unwrap();
```

### Optional Values

```rust
#[derive(TryFromExpr)]
enum OptionalConfig {
    Value(Option<String>),
}

// Explicit Some
let expr = syn::parse_str(r#"OptionalConfig::Value(Some("text"))"#).unwrap();

// Explicit None
let expr = syn::parse_str("OptionalConfig::Value(None)").unwrap();

// Implicit Some (bare value treated as Some)
let expr = syn::parse_str(r#"OptionalConfig::Value("text")"#).unwrap();
```

## Force Mode Selection

By default, the macro automatically detects whether your enum is a wrapper or
leaf enum. You can override this:

```rust
#[derive(TryFromExpr)]
#[try_from_expr(wrapper)]  // Force wrapper mode
enum ForceWrapper {
    Variant1(CustomType),
    Variant2(AnotherType),
}

#[derive(TryFromExpr)]
#[try_from_expr(leaf)]  // Force leaf mode
enum ForceLeaf {
    Simple,
    Complex(String),
}
```

## How It Works

The macro analyzes your enum at compile time and generates a
`TryFrom<&syn::Expr>` implementation that:

1. **Unwraps** any parentheses or group expressions
2. **Matches** the expression type (path, call, struct, literal)
3. **Parses** the variant name and validates it belongs to your enum
4. **Extracts** and parses any parameters or fields
5. **Constructs** the appropriate enum variant
6. **Returns** helpful error messages for any parsing failures

## Supported Types

The macro has built-in support for:

-   **Primitives**: `bool`, `char`, `String`, all integer types, `f32`, `f64`
-   **Collections**: `Vec<T>`, `HashMap<K, V>`, `BTreeMap<K, V>`, `Option<T>`
-   **Special**: `OrderedFloat<T>` from the `ordered-float` crate
-   **Custom Types**: Any type that implements `TryFrom<&syn::Expr>`

## Error Handling

The macro provides detailed error messages:

```rust
// Unknown variant
"Unknown variant 'Invalid' for enum 'Setting'. Valid unit variants: Default"

// Wrong number of arguments
"Variant 'Coordinate' expects exactly 2 arguments, but 3 were provided"

// Type parsing failure
"Failed to parse argument 1: expected u32, got string literal"

// Missing required field
"Missing required field 'name' for variant 'Config'"
```

## Project Structure

This workspace contains two crates:

-   `try_from_expr` - The main library crate that users import
-   `try_from_expr_derive` - The procedural macro implementation

## License

This project is licensed under the MIT license.

## Contributing

Please feel free to submit a Pull Request.
