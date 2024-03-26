#![allow(dead_code)]

use crate::{
    ast::{ArgDef, BuiltInFunction, Expr, Function, FunctionCall, Ident},
    eval::ScopeMap,
};

pub fn double() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        if let Some(a) = local_scope.get(&Ident("a".to_owned())) {
            if let Expr::Int(a) = a {
                Expr::Int(a * 2)
            } else {
                panic!("argument a is not `Expr::Int`!")
            }
        } else {
            double()
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
        if let Some(a) = local_scope.get(&Ident("a".to_owned())) {
            if let Expr::Int(a) = a {
                Expr::String(a.to_string())
            } else {
                panic!("Expected `Int`, got {:?}", a)
            }
        } else {
            int_to_str()
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        name: Ident("int_to_str".to_owned()),
        arguments: vec![ArgDef {
            name: Ident("a".to_owned()),
            typ: "Int".to_owned(),
        }],
        return_type: "String".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn list_map() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let list = local_scope
            .get(&Ident("list".to_owned()))
            .expect("Expected to be called with argument `list`");
        let function = local_scope
            .get(&Ident("function".to_owned()))
            .expect("Expected to be called with argument `function`");
        if let (Expr::List(list), Expr::Function(fun)) = (list, function) {
            Expr::List(
                list.iter()
                    .map(|v| FunctionCall {
                        name: fun.name.clone(),
                        arguments: vec![v.clone()],
                    })
                    .map(Expr::FunctionCall)
                    .collect(),
            )
        } else {
            panic!(
                "Expected List and Function, got {:?} and {:?}",
                list, function
            )
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        name: Ident("list_map".to_owned()),
        arguments: vec![
            ArgDef {
                name: Ident("list".to_owned()),
                typ: "List".to_owned(),
            },
            ArgDef {
                name: Ident("function".to_owned()),
                typ: "Function".to_owned(),
            },
        ],
        return_type: "String".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}
