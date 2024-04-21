#![allow(dead_code)]

use std::{borrow::Borrow, collections::HashMap, ops::Deref, rc::Rc};

use crate::{
    ast::{ArgDef, BuiltInFunction, Expr, Function, FunctionCall, Ident, RExpr},
    eval::ScopeMap,
    typ_check::{
        generic_list_type_name, generic_option_type_name, FunctionTypeName, NormalTypeName, Type,
        TypeName,
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

fn double() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        if let Some(a) = local_scope.get(&ident("a")) {
            if let Expr::Int(a) = **a {
                Expr::Int(a * 2).into()
            } else {
                panic!("argument a is not `Expr::Int`!")
            }
        } else {
            Expr::BuiltInFunction(BuiltInFunction { body: &body }).into()
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("a"),
            typ: int_type_name(),
        }],
        return_type: int_type_name(),
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

fn int_to_str() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        if let Some(a) = local_scope.get(&ident("a")) {
            if let Expr::Int(a) = **a {
                Expr::String(a.to_string()).into()
            } else {
                panic!("Expected `Int`, got {:?}", a)
            }
        } else {
            Expr::BuiltInFunction(BuiltInFunction { body: &body }).into()
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("a"),
            typ: int_type_name(),
        }],
        return_type: string_type_name(),
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

fn list_map() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        let list = local_scope.get(&ident("list"));
        let function = local_scope.get(&ident("function"));
        match (list.map(|v| v.borrow()), function.borrow()) {
            (Some(Expr::List(list)), Some(function)) => Expr::List(
                list.iter()
                    .map(|v| FunctionCall {
                        function: (*function).clone(),
                        arguments: vec![v.clone()],
                    })
                    .map(Expr::FunctionCall)
                    .map(Rc::new)
                    .collect(),
            ),
            (Some(a), Some(b)) if a.is_realized() && b.is_realized() => {
                panic!("Expected List and Function, got {:?} and {:?}", a, b)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }),
        }
        .into()
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![
            ArgDef {
                name: ident("list"),
                typ: TypeName::Normal(NormalTypeName {
                    name: ident("List"),
                    subtype: Some(Box::new(NormalTypeName {
                        name: ident("*I"),
                        subtype: None,
                    })),
                }),
            },
            ArgDef {
                name: ident("function"),
                typ: TypeName::Function(FunctionTypeName {
                    args: vec![NormalTypeName {
                        name: ident("*I"),
                        subtype: None,
                    }],
                    return_type: NormalTypeName {
                        name: ident("*I"),
                        subtype: None,
                    },
                }),
            },
        ],
        return_type: TypeName::Normal(NormalTypeName {
            name: ident("List"),
            subtype: Some(Box::new(NormalTypeName {
                name: ident("*O"),
                subtype: None,
            })),
        }),
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

fn list_fold() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        let list = local_scope.get(&ident("list"));
        let function = local_scope.get(&ident("function"));
        let init = local_scope.get(&ident("init"));
        match (list.map(|v| v.borrow()), function.borrow(), init.borrow()) {
            (Some(Expr::List(list)), Some(function), Some(init)) => {
                let mut value = (*init).clone();
                for elem in list {
                    value = Expr::FunctionCall(FunctionCall {
                        function: (*function).clone(),
                        arguments: vec![value.clone(), elem.clone()],
                    })
                    .into();
                }
                value
            }
            (Some(a), Some(b), Some(c))
                if a.is_realized() && b.is_realized() && c.is_realized() =>
            {
                panic!(
                    "Expected List, Function and *O, got {:?}, {:?} and {:?}",
                    a, b, c
                )
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }).into(),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![
            ArgDef {
                name: ident("list"),
                typ: TypeName::Normal(NormalTypeName {
                    name: ident("List"),
                    subtype: Some(Box::new(NormalTypeName {
                        name: ident("*I"),
                        subtype: None,
                    })),
                }),
            },
            ArgDef {
                name: ident("function"),
                typ: TypeName::Function(FunctionTypeName {
                    args: vec![
                        NormalTypeName {
                            name: ident("*O"),
                            subtype: None,
                        },
                        NormalTypeName {
                            name: ident("*I"),
                            subtype: None,
                        },
                    ],
                    return_type: NormalTypeName {
                        name: ident("*O"),
                        subtype: None,
                    },
                }),
            },
            ArgDef {
                name: ident("init"),
                typ: TypeName::Normal(NormalTypeName {
                    name: ident("*O"),
                    subtype: None,
                }),
            },
        ],
        return_type: TypeName::Normal(NormalTypeName {
            name: ident("*O"),
            subtype: None,
        }),
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

fn list_get() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        let list = local_scope.get(&ident("list"));
        let idx = local_scope.get(&ident("idx"));
        match (list.map(|v| v.borrow()), idx.map(|v| v.borrow())) {
            (Some(Expr::List(list)), Some(Expr::Int(idx))) => {
                expr_option_to_enum(list.get(*idx as usize).cloned())
            }
            (Some(a), Some(b)) if a.is_realized() && b.is_realized() => {
                panic!("Expected List and Int, got {:?} and {:?}", a, b)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }).into(),
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
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

fn list_size() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        let list = local_scope.get(&ident("list"));
        match list.map(|v| v.borrow()) {
            Some(Expr::List(list)) => Expr::Int(list.len() as i64).into(),
            Some(a) if a.is_realized() => {
                panic!("Expected List, got {:?}", a)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }).into(),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("list"),
            typ: generic_list_type_name(),
        }],
        return_type: int_type_name(),
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

fn sin() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        let x = local_scope.get(&ident("x"));
        match x.map(|v| v.borrow()) {
            Some(Expr::Float(x)) => Expr::Float(x.sin()).into(),
            Some(a) if a.is_realized() => {
                panic!("Expected List, got {:?}", a)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }).into(),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("x"),
            typ: float_type_name(),
        }],
        return_type: float_type_name(),
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

fn cos() -> RExpr {
    fn body(local_scope: &ScopeMap) -> RExpr {
        let x = local_scope.get(&ident("x"));
        match x.map(|v| v.borrow()) {
            Some(Expr::Float(x)) => Expr::Float(x.cos()).into(),
            Some(a) if a.is_realized() => {
                panic!("Expected List, got {:?}", a)
            }
            _ => Expr::BuiltInFunction(BuiltInFunction { body: &body }).into(),
        }
    }
    let bif = BuiltInFunction { body: &body };
    Expr::Function(Function {
        arguments: vec![ArgDef {
            name: ident("x"),
            typ: float_type_name(),
        }],
        return_type: float_type_name(),
        body: Rc::new(Expr::BuiltInFunction(bif)),
    })
    .into()
}

pub fn scope_with_builtin_functions() -> ScopeMap {
    [
        (ident("double"), double()),
        (ident("int_to_str"), int_to_str()),
        (ident("list_map"), list_map()),
        (ident("list_fold"), list_fold()),
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
            Type::from_function_def_unchecked(&double(), type_definitions).unwrap(),
        ),
        (
            ident("int_to_str"),
            Type::from_function_def_unchecked(&int_to_str(), type_definitions).unwrap(),
        ),
        (
            ident("list_map"),
            Type::from_function_def_unchecked(&list_map(), type_definitions).unwrap(),
        ),
        (
            ident("list_fold"),
            Type::from_function_def_unchecked(&list_fold(), type_definitions).unwrap(),
        ),
        (
            ident("list_get"),
            Type::from_function_def_unchecked(&list_get(), type_definitions).unwrap(),
        ),
        (
            ident("list_size"),
            Type::from_function_def_unchecked(&list_size(), type_definitions).unwrap(),
        ),
        (
            ident("sin"),
            Type::from_function_def_unchecked(&sin(), type_definitions).unwrap(),
        ),
        (
            ident("cos"),
            Type::from_function_def_unchecked(&cos(), type_definitions).unwrap(),
        ),
    ]
    .into()
}
