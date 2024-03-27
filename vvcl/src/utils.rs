use std::collections::HashMap;

use crate::{
    ast::{Definition, Expr, Ident},
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
    scope.insert(ident("true"), Expr::Bool(true));
    scope.insert(ident("false"), Expr::Bool(false));
    scope
}
