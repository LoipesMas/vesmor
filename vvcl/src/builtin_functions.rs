#![allow(dead_code)]

use crate::{
    ast::{ArgDef, BuiltInFunction, Expr, Function, FunctionCall},
    eval::ScopeMap,
    utils::{expr_option_to_enum, ident},
};

fn double() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        if let Some(a) = local_scope.get(&ident("a")) {
            if let Expr::Int(a) = a {
                Expr::Int(a * 2)
            } else {
                panic!("argument a is not `Expr::Int`!")
            }
        } else {
            Expr::BuiltInFunction(BuiltInFunction { body: &body })
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("a"),
            typ: "Int".to_owned(),
        }],
        return_type: "Int".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

fn int_to_str() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        if let Some(a) = local_scope.get(&ident("a")) {
            if let Expr::Int(a) = a {
                Expr::String(a.to_string())
            } else {
                panic!("Expected `Int`, got {:?}", a)
            }
        } else {
            Expr::BuiltInFunction(BuiltInFunction { body: &body })
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("a"),
            typ: "Int".to_owned(),
        }],
        return_type: "String".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

fn list_map() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let list = local_scope.get(&ident("list"));
        let function = local_scope.get(&ident("function"));
        match (list, function) {
            (Some(Expr::List(list)), Some(function)) => Expr::List(
                list.iter()
                    .map(|v| FunctionCall {
                        function: Box::new(function.clone()),
                        arguments: vec![v.clone()],
                    })
                    .map(Expr::FunctionCall)
                    .collect(),
            ),
            (Some(a), Some(b)) if a.is_realized() && b.is_realized() => {
                panic!("Expected List and Function, got {:?} and {:?}", a, b)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
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

fn list_get() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let list = local_scope.get(&ident("list"));
        let idx = local_scope.get(&ident("idx"));
        match (list, idx) {
            (Some(Expr::List(list)), Some(Expr::Int(idx))) => {
                expr_option_to_enum(list.get(*idx as usize).cloned())
            }
            (Some(a), Some(b)) if a.is_realized() && b.is_realized() => {
                panic!("Expected List and Int, got {:?} and {:?}", a, b)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![
            ArgDef {
                name: ident("list"),
                typ: "List<T>".to_owned(),
            },
            ArgDef {
                name: ident("idx"),
                typ: "Int".to_owned(),
            },
        ],
        return_type: "Option<T>".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

fn list_size() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let list = local_scope.get(&ident("list"));
        match list {
            Some(Expr::List(list)) => Expr::Int(list.len() as i64),
            Some(a) if a.is_realized() => {
                panic!("Expected List, got {:?}", a)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("list"),
            typ: "List".to_owned(),
        }],
        return_type: "Int".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

fn sin() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let x = local_scope.get(&ident("x"));
        match x {
            Some(Expr::Float(x)) => Expr::Float(x.sin()),
            Some(a) if a.is_realized() => {
                panic!("Expected List, got {:?}", a)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("x"),
            typ: "Float".to_owned(),
        }],
        return_type: "Float".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

fn cos() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let x = local_scope.get(&ident("x"));
        match x {
            Some(Expr::Float(x)) => Expr::Float(x.cos()),
            Some(a) if a.is_realized() => {
                panic!("Expected List, got {:?}", a)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("x"),
            typ: "Float".to_owned(),
        }],
        return_type: "Float".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn scope_with_builtin_functions() -> ScopeMap {
    let mut scope = ScopeMap::new();
    scope.insert(ident("double"), double());
    scope.insert(ident("int_to_str"), int_to_str());
    scope.insert(ident("list_map"), list_map());
    scope.insert(ident("list_get"), list_get());
    scope.insert(ident("list_size"), list_size());
    scope.insert(ident("sin"), sin());
    scope.insert(ident("cos"), cos());
    scope
}
