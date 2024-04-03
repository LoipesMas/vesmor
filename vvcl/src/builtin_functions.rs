#![allow(dead_code)]

use std::collections::HashMap;

use crate::{
    ast::{ArgDef, BuiltInFunction, Expr, Function, FunctionCall, Ident},
    eval::ScopeMap,
    typ_check::{
        generic_list_type_name, generic_option_type_name, type_hole_name, FunctionTypeName,
        NormalTypeName, Type, TypeName,
    },
    utils::{expr_option_to_enum, ident},
};

fn int_type_name() -> TypeName {
    TypeName::Normal(NormalTypeName {
        name: ident("Int"),
        subtype: None,
    })
}

fn float_type_name() -> TypeName {
    TypeName::Normal(NormalTypeName {
        name: ident("Float"),
        subtype: None,
    })
}

fn string_type_name() -> TypeName {
    TypeName::Normal(NormalTypeName {
        name: ident("String"),
        subtype: None,
    })
}

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
            typ: int_type_name(),
        }],
        return_type: int_type_name(),
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
            typ: int_type_name(),
        }],
        return_type: string_type_name(),
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
                typ: generic_list_type_name(),
            },
            ArgDef {
                name: ident("function"),
                typ: TypeName::Function(FunctionTypeName {
                    args: vec![NormalTypeName {
                        name: ident("_"),
                        subtype: None,
                    }],
                    return_type: NormalTypeName {
                        name: ident("_"),
                        subtype: None,
                    },
                }),
            },
        ],
        return_type: TypeName::Normal(type_hole_name()),
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
                typ: generic_list_type_name(),
            },
            ArgDef {
                name: ident("idx"),
                typ: int_type_name(),
            },
        ],
        return_type: generic_option_type_name(),
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
            typ: generic_list_type_name(),
        }],
        return_type: int_type_name(),
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
            typ: float_type_name(),
        }],
        return_type: float_type_name(),
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
            typ: float_type_name(),
        }],
        return_type: float_type_name(),
        body: Box::new(Expr::BuiltInFunction(bif)),
    })
}

pub fn scope_with_builtin_functions() -> ScopeMap {
    [
        (ident("double"), double()),
        (ident("int_to_str"), int_to_str()),
        (ident("list_map"), list_map()),
        (ident("list_get"), list_get()),
        (ident("list_size"), list_size()),
        (ident("sin"), sin()),
        (ident("cos"), cos()),
    ]
    .into()
}

pub fn builtin_function_type_definitions(
    type_definitions: &HashMap<Ident, Type>,
) -> HashMap<Ident, Type> {
    [
        (
            ident("double"),
            Type::from_function_def_unchecked(&double(), type_definitions),
        ),
        (
            ident("int_to_str"),
            Type::from_function_def_unchecked(&int_to_str(), type_definitions),
        ),
        (
            ident("list_map"),
            Type::from_function_def_unchecked(&list_map(), type_definitions),
        ),
        (
            ident("list_get"),
            Type::from_function_def_unchecked(&list_get(), type_definitions),
        ),
        (
            ident("list_size"),
            Type::from_function_def_unchecked(&list_size(), type_definitions),
        ),
        (
            ident("sin"),
            Type::from_function_def_unchecked(&sin(), type_definitions),
        ),
        (
            ident("cos"),
            Type::from_function_def_unchecked(&cos(), type_definitions),
        ),
    ]
    .into()
}
