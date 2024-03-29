#![allow(dead_code)]

use crate::{
    ast::{ArgDef, BuiltInFunction, Expr, Function, FunctionCall},
    eval::ScopeMap,
    utils::{enum_variant, expr_option_to_enum, ident},
};

pub fn double() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        if let Some(a) = local_scope.get(&ident("a")) {
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
        name: ident("double"),
        arguments: vec![ArgDef {
            name: ident("a"),
            typ: "Int".to_owned(),
        }],
        return_type: "Int".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn int_to_str() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        if let Some(a) = local_scope.get(&ident("a")) {
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
        name: ident("int_to_str"),
        arguments: vec![ArgDef {
            name: ident("a"),
            typ: "Int".to_owned(),
        }],
        return_type: "String".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn list_map() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let list = local_scope
            .get(&ident("list"))
            .expect("Expected to be called with argument `list`");
        let function = local_scope
            .get(&ident("function"))
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
        name: ident("list_map"),
        arguments: vec![
            ArgDef {
                name: ident("list"),
                typ: "List".to_owned(),
            },
            ArgDef {
                name: ident("function"),
                typ: "Function".to_owned(),
            },
        ],
        return_type: "String".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn list_get() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let list = local_scope
            .get(&ident("list"))
            .expect("Expected to be called with argument `list`");
        let idx = local_scope
            .get(&ident("idx"))
            .expect("Expected to be called with argument `idx`");
        if let (Expr::List(list), Expr::Int(idx)) = (list, idx) {
            expr_option_to_enum(list.get(*idx as usize).cloned())
        } else {
            panic!("Expected List and Int, got {:?} and {:?}", list, idx)
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        name: ident("list_get"),
        arguments: vec![
            ArgDef {
                name: ident("list"),
                typ: "List".to_owned(),
            },
            ArgDef {
                name: ident("idx"),
                typ: "Int".to_owned(),
            },
        ],
        return_type: "Option<Int>".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn scope_with_builtin_functions() -> ScopeMap {
    let mut scope = ScopeMap::new();
    scope.insert(ident("double"), double());
    scope.insert(ident("int_to_str"), int_to_str());
    scope.insert(ident("list_map"), list_map());
    scope.insert(ident("list_get"), list_get());
    scope
}
