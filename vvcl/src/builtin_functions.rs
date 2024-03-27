#![allow(dead_code)]

use crate::{
    ast::{ArgDef, BuiltInFunction, Expr, Function, FunctionCall},
    eval::ScopeMap,
    ident,
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

// TODO: this is a HACK
// we need a if-statement (or match-statement) to do this properly
// because right now both branches are being evaluated,
// which causes recursion to not return, ever.
// a language-level statement could be special-cased in `eval::beta_reduction`
pub fn if_() -> Expr {
    fn body(local_scope: &ScopeMap) -> Expr {
        let cond = local_scope.get(&ident("cond"));
        let then = local_scope.get(&ident("then"));
        let else_ = local_scope.get(&ident("else"));
        if let (Some(cond), Some(then), Some(else_)) = (cond, then, else_) {
            if let Expr::Bool(cond) = cond {
                if *cond {
                    then.clone()
                } else {
                    else_.clone()
                }
            } else {
                panic!("Expected Bool, got {:?}", cond)
            }
        } else {
            if_()
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        name: ident("if"),
        arguments: vec![
            ArgDef {
                name: ident("cond"),
                typ: "Bool".to_owned(),
            },
            ArgDef {
                name: ident("then"),
                typ: "T".to_owned(),
            },
            ArgDef {
                name: ident("else"),
                typ: "T".to_owned(),
            },
        ],
        return_type: "T".to_owned(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn scope_with_builtin_functions() -> ScopeMap {
    let mut scope = ScopeMap::new();
    scope.insert(ident("double"), double());
    scope.insert(ident("int_to_str"), int_to_str());
    scope.insert(ident("list_map"), list_map());
    scope.insert(ident("if"), if_());
    scope
}
