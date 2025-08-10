// Re-export the derive macro
pub use try_from_expr_derive::TryFromExpr;

// Re-export dependencies that the macro needs
pub use syn;
pub use ordered_float;

use ordered_float::OrderedFloat;
use syn::spanned::Spanned;
use std::ops::Deref;

// Newtype wrapper for OrderedFloat that implements TryFrom<&syn::Expr>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParseableFloat(pub OrderedFloat<f64>);

impl Deref for ParseableFloat {
    type Target = OrderedFloat<f64>;
    
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<ParseableFloat> for OrderedFloat<f64> {
    fn from(p: ParseableFloat) -> Self {
        p.0
    }
}

impl From<OrderedFloat<f64>> for ParseableFloat {
    fn from(o: OrderedFloat<f64>) -> Self {
        ParseableFloat(o)
    }
}

impl TryFrom<&syn::Expr> for ParseableFloat {
    type Error = syn::Error;

    fn try_from(expr: &syn::Expr) -> Result<Self, Self::Error> {
        // Unwrap parentheses and groups
        let expr = unwrap_expr(expr);
        
        match expr {
            syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Float(lit_float), .. }) => {
                lit_float.base10_parse::<f64>()
                    .map(|f| ParseableFloat(OrderedFloat(f)))
                    .map_err(|e| syn::Error::new(lit_float.span(), format!("Invalid float: {}", e)))
            }
            syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Int(lit_int), .. }) => {
                lit_int.base10_parse::<f64>()
                    .map(|f| ParseableFloat(OrderedFloat(f)))
                    .map_err(|e| syn::Error::new(lit_int.span(), format!("Invalid number: {}", e)))
            }
            syn::Expr::Unary(syn::ExprUnary { op: syn::UnOp::Neg(_), expr, .. }) => {
                // Handle negative numbers
                match &**expr {
                    syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Float(lit_float), .. }) => {
                        lit_float.base10_parse::<f64>()
                            .map(|n| ParseableFloat(OrderedFloat(-n)))
                            .map_err(|e| syn::Error::new(lit_float.span(), format!("Invalid float: {}", e)))
                    }
                    syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Int(lit_int), .. }) => {
                        lit_int.base10_parse::<f64>()
                            .map(|n| ParseableFloat(OrderedFloat(-n)))
                            .map_err(|e| syn::Error::new(lit_int.span(), format!("Invalid number: {}", e)))
                    }
                    _ => Err(syn::Error::new(expr.span(), "Expected a numeric literal after negative sign")),
                }
            }
            _ => Err(syn::Error::new(expr.span(), "Expected a numeric literal (integer or float)")),
        }
    }
}

// Helper function to unwrap Expr::Paren and Expr::Group
fn unwrap_expr(expr: &syn::Expr) -> &syn::Expr {
    match expr {
        syn::Expr::Paren(paren) => unwrap_expr(&paren.expr),
        syn::Expr::Group(group) => unwrap_expr(&group.expr),
        _ => expr,
    }
}
