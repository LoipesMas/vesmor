use std::collections::HashMap;

use crate::{
    ast::{Definition, EnumVariant, Expr, Ident},
    builtin_functions::scope_with_builtin_functions,
    eval::ScopeMap,
};

pub fn map_from_defs(defs: Vec<Definition>) -> HashMap<Ident, Expr> {
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
        match *fun.body {
            Expr::Int(v) => Ok(v.to_string()),
            Expr::Float(v) => Ok(v.to_string()),
            Expr::String(v) => Ok(v),
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

pub fn bool_to_enum(v: bool) -> Expr {
    let variant_str = match v {
        true => "True",
        false => "False",
    }
    .to_string();
    Expr::EnumVariant(EnumVariant {
        enu: ident("Bool"),
        variant: Ident(variant_str),
        body: None,
    })
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
