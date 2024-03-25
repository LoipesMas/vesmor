use std::collections::HashMap;

use crate::ast::{Definition, Expr, Ident};

pub fn map_from_defs(defs: Vec<Definition>) -> HashMap<Ident, Expr> {
    // TODO: unfortunately this allows for shadowing ;//
    // need to figure out a different way
    defs.into_iter().map(|d| (d.name, d.body)).collect()
}
