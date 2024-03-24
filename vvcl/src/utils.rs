use std::collections::HashMap;

use crate::ast::{Definition, Expr, Ident};

pub fn map_from_defs(defs: Vec<Definition>) -> HashMap<Ident, Expr> {
    defs.into_iter().map(|d| (d.name, d.body)).collect()
}
