use std::{borrow::Borrow, collections::HashMap};

use nom_locate::LocatedSpan;
use nom_recursive::RecursiveInfo;

use crate::{
    ast::{Definition, EnumVariant, Expr, Ident, RExpr},
    builtin_functions::scope_with_builtin_functions,
    eval::ScopeMap,
};

pub fn map_from_defs(defs: Vec<Definition>) -> HashMap<Ident, RExpr> {
    // TODO: unfortunately this allows for shadowing ;//
    // need to figure out a different way
    defs.into_iter().map(|d| (d.name, d.body)).collect()
}

pub fn ident(s: &str) -> Ident {
    Ident(s.to_owned())
}

pub fn default_global_scope() -> ScopeMap {
    let mut scope = ScopeMap::new();
    scope.extend(scope_with_builtin_functions());
    scope
}

pub fn get_function_value(expr: Expr) -> Result<String, ()> {
    if let Expr::Function(fun) = expr {
        match fun.body.borrow() {
            Expr::Int(v) => Ok(v.to_string()),
            Expr::Float(v) => Ok(v.to_string()),
            Expr::String(v) => Ok(v.to_owned()),
            Expr::Record(v) if fun.body.is_realized() => Ok(format!("{:?}", v)),
            Expr::List(v) if fun.body.is_realized() => Ok(format!("{:?}", v)),
            Expr::EnumVariant(v) if fun.body.is_realized() => Ok(format!("{v:?}")),
            _ => {
                dbg!(fun.body);
                Err(())
            }
        }
    } else {
        Err(())
    }
}

pub fn bool_to_enum(v: bool) -> RExpr {
    let variant_str = match v {
        true => "True",
        false => "False",
    }
    .to_string();
    enum_variant("Bool", &variant_str, None)
}

pub fn enum_to_bool(ev: &EnumVariant) -> bool {
    if ev.enu.0 == "Bool" {
        match ev.variant.0.as_str() {
            "True" => true,
            "False" => false,
            _ => panic!("Unexpected Bool variant: {ev:?}"),
        }
    } else {
        panic!("Expected `Bool` enum, got {ev:?}")
    }
}

pub fn enum_variant(enu: &str, variant: &str, body: Option<RExpr>) -> RExpr {
    Expr::EnumVariant(EnumVariant {
        enu: ident(enu),
        variant: ident(variant),
        body,
    })
    .into()
}

pub fn expr_option_to_enum(body: Option<RExpr>) -> RExpr {
    let variant_str = if body.is_some() { "Some" } else { "None" };
    enum_variant("Option", variant_str, body)
}

pub fn wrap_in_span(input: &str) -> LocatedSpan<&str, RecursiveInfo> {
    LocatedSpan::new_extra(input, RecursiveInfo::new())
}

pub trait ErrWithContext: Sized {
    fn with_context(self, ctx: String) -> Self;
}

impl<T> ErrWithContext for Result<T, String> {
    fn with_context(self, ctx: String) -> Self {
        match self {
            Ok(_) => self,
            Err(s) => Err(ctx + &s),
        }
    }
}
