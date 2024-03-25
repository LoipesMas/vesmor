#![allow(dead_code)]

use crate::{
    ast::{ArgDef, BuiltInFunction, Expr, Function, Ident},
    eval::ScopeMap,
};

pub fn double() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let a = local_scope
            .get(&Ident("a".to_owned()))
            .expect("Expected to be called with argument `a`");
        if let Expr::Int(a) = a {
            Expr::Int(a * 2)
        } else {
            panic!("argument a is not `Expr::Int`!")
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        name: Ident("double".to_owned()),
        arguments: vec![ArgDef {
            name: Ident("a".to_owned()),
            typ: "Int".to_owned(),
        }],
        return_type: "Int".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn int_to_str() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let a = local_scope
            .get(&Ident("a".to_owned()))
            .expect("Expected to be called with argument `a`");
        if let Expr::Int(a) = a {
            Expr::String(a.to_string())
        } else {
            panic!("argument a is not `Expr::Int`!")
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        name: Ident("double".to_owned()),
        arguments: vec![ArgDef {
            name: Ident("a".to_owned()),
            typ: "Int".to_owned(),
        }],
        return_type: "String".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}
