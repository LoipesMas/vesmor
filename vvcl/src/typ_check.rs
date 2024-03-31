use std::collections::HashMap;

use crate::{
    ast::{BinaryOperation, BinaryOperator, Expr, Ident},
    utils::ident,
};

pub fn generic_option_type() -> TypeDef {
    TypeDef::Enum {
        enu: ident("Option"),
        variants: [
            (ident("None"), None),
            (ident("Some"), Some(TypeDef::simple("T"))),
        ]
        .into(),
    }
}

pub fn concrete_option_type(typ: TypeDef) -> TypeDef {
    TypeDef::Enum {
        enu: ident("Option"),
        variants: [(ident("None"), None), (ident("Some"), Some(typ))].into(),
    }
}

pub fn generic_list_type() -> TypeDef {
    TypeDef::Simple {
        name: ident("List"),
        subtype: Some(Box::new(TypeDef::simple("T"))),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeDef {
    Simple {
        name: Ident,
        subtype: Option<Box<TypeDef>>,
    },
    Record(TypeMap),
    Enum {
        enu: Ident,
        variants: HashMap<Ident, Option<TypeDef>>,
    },
    Function {
        args: Vec<TypeDef>,
        return_type: Box<TypeDef>,
    },
}

impl std::fmt::Display for TypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple { name, subtype } => {
                if let Some(ref subtype) = subtype {
                    write!(f, "{}<{}>", name.0, subtype)
                } else {
                    write!(f, "{}", name.0)
                }
            }
            Self::Record(map) => {
                write!(f, "{{")?;
                for (k, v) in map.iter() {
                    write!(f, "{}: {v},", k.0)?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            _ => todo!("{:?}", self),
        }
    }
}

impl TypeDef {
    pub fn simple(name: &str) -> Self {
        Self::Simple {
            name: ident(name),
            subtype: None,
        }
    }
}

type TypeMap = HashMap<Ident, TypeDef>;

trait Scope {
    fn get(&self, ident: &Ident) -> Option<&TypeDef>;
}

type TwoScopes<'a> = (&'a TypeMap, &'a TypeMap);
impl Scope for TwoScopes<'_> {
    fn get(&self, ident: &Ident) -> Option<&TypeDef> {
        self.1.get(ident).or_else(|| self.0.get(ident))
    }
}

pub fn check(
    global_scope: &TypeMap,
    local_scope: &TypeMap,
    expr: &Expr,
) -> Result<TypeDef, String> {
    let combined_scope = (global_scope, local_scope);
    let c = |e| check(global_scope, local_scope, e);
    match expr {
        Expr::Int(_) => Ok(TypeDef::simple("Int")),
        Expr::Float(_) => Ok(TypeDef::simple("Float")),
        Expr::String(_) => Ok(TypeDef::simple("String")),
        Expr::List(list) => {
            let types = list.iter().map(c).collect::<Result<Vec<_>, _>>()?;
            match &types[..] {
                [] => Ok(TypeDef::Simple {
                    name: ident("List"),
                    subtype: Some(Box::new(TypeDef::simple("T"))),
                }),
                [subtype] => Ok(TypeDef::Simple {
                    name: ident("List"),
                    subtype: Some(Box::new(subtype.clone())),
                }),
                [first, ..] => {
                    let all_same = types.windows(2).all(|s| s[0] == s[1]);
                    if all_same {
                        Ok(TypeDef::Simple {
                            name: ident("List"),
                            subtype: Some(Box::new(first.clone())),
                        })
                    } else {
                        Err(format!(
                            "List isn't homogeneous, found types: {:?}",
                            types.iter().map(|t| t.to_string()).collect::<Vec<_>>()
                        ))
                    }
                }
            }
        }
        Expr::Block(b) => {
            let mut new_scope = local_scope.clone();
            for def in &b.definitions {
                new_scope.insert(def.name.clone(), c(&def.body)?);
            }
            check(global_scope, &new_scope, &b.expr)
        }
        Expr::Value(v) => combined_scope
            .get(v)
            .cloned()
            .ok_or_else(|| format!("Type for value '{}' not found!", v.0)),
        Expr::BinaryOperation(bin_opt) => check_bin_opt(global_scope, local_scope, bin_opt),
        Expr::Function(f) => {
            let mut new_scope = TypeMap::new();
            for arg in f.arguments.clone() {
                new_scope.insert(arg.name, arg.typ);
            }
            let evaled_type = check(global_scope, &new_scope, &f.body)?;
            if evaled_type == f.return_type {
                Ok(f.return_type.clone())
            } else {
                Err(format!(
                    "Function has incorrect return type, expected {}, got {}",
                    f.return_type, evaled_type
                ))
            }
        }
        Expr::BuiltInFunction(_) => unreachable!("This shouldn't have been checked"),
        Expr::FunctionCall(fc) => {
            let func_type = c(&fc.function)?;
            if let TypeDef::Function { args, return_type } = func_type {
                let call_arg_types = fc.arguments.iter().map(c).collect::<Result<Vec<_>, _>>()?;
                match call_arg_types.len().cmp(&args.len()) {
                    std::cmp::Ordering::Greater => {
                        Err("Called function with too many arguments".to_string())
                    }
                    std::cmp::Ordering::Less => {
                        Err("Partial application not implemented yet!".to_string())
                    }
                    std::cmp::Ordering::Equal => {
                        if call_arg_types.iter().zip(args.iter()).all(|(a, b)| a == b) {
                            Ok(*return_type)
                        } else {
                            Err(format!(
                                "Invalid call args, expected {:?}, got {:?}",
                                args.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
                                call_arg_types
                                    .iter()
                                    .map(|t| t.to_string())
                                    .collect::<Vec<_>>()
                            ))
                        }
                    }
                }
            } else {
                Err(format!("Tried to call non-function: {}", func_type))
            }
        }
        Expr::Record(r) => {
            let mut new_types = HashMap::new();
            for (k, v) in r.update.iter() {
                new_types.insert(k.clone(), c(v)?);
            }
            if let Some(base) = &r.base {
                let base_type = c(base)?;
                if let TypeDef::Record(base_r) = base_type {
                    for (k, v) in base_r.iter() {
                        if let Some(new_type) = new_types.insert(k.clone(), v.clone()) {
                            if &new_type != v {
                                return Err(format!(
                                    "Incorrect new type for {k:?}, expected {v}, got {new_type}"
                                ));
                            }
                        }
                    }
                    Ok(TypeDef::Record(new_types))
                } else {
                    Err(format!(
                        "Base argument should be a record, got {base_type} instead"
                    ))
                }
            } else {
                Ok(TypeDef::Record(new_types))
            }
        }
        Expr::RecordAccess(ra) => {
            let rec_type = c(&ra.record)?;
            if let TypeDef::Record(ref map) = rec_type {
                if let Some(t) = map.get(&ra.member) {
                    Ok(t.clone())
                } else {
                    Err(format!(
                        "Type {rec_type} doesn't have member '{}'",
                        &ra.member.0
                    ))
                }
            } else {
                Err(format!(
                    "Tried to access member {} of non-record type {rec_type}",
                    &ra.member.0
                ))
            }
        }
        Expr::EnumVariant(ev) => {
            if ev.enu.0 == "Option" {
                match ev.variant.0.as_str() {
                    "None" => {
                        if ev.body.is_some() {
                            Err("Option::None can't have a body!".to_owned())
                        } else {
                            Ok(generic_option_type())
                        }
                    }
                    "Some" => {
                        if let Some(body) = &ev.body {
                            let body_type = c(body)?;
                            Ok(concrete_option_type(body_type))
                        } else {
                            Err("Option::Some requires a body!".to_owned())
                        }
                    }
                    v => Err(format!("Invalid variant '{v}' of enum 'Option'")),
                }
            } else {
                // would have to get enum def from some storage..
                Err("Enum other than Option not implemented yet.".to_owned())
            }
        }
        Expr::EnumMatching(_) => todo!(),
    }
}

fn check_bin_opt(
    global_scope: &TypeMap,
    local_scope: &TypeMap,
    bin_opt: &BinaryOperation,
) -> Result<TypeDef, String> {
    let left = check(global_scope, local_scope, &bin_opt.left)?;
    let right = check(global_scope, local_scope, &bin_opt.right)?;
    use BinaryOperator::*;
    match bin_opt.operator {
        IntAdd | IntSub | IntMul | IntDiv | IntMod => {
            if left == right && left == TypeDef::simple("Int") {
                Ok(TypeDef::simple("Int"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        FloatAdd | FloatSub | FloatMul | FloatDiv | FloatLT | FloatGT => {
            if left == right && left == TypeDef::simple("Float") {
                Ok(TypeDef::simple("Float"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        BoolOr | BoolAnd => {
            if left == right && left == TypeDef::simple("Bool") {
                Ok(TypeDef::simple("Bool"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        StringConcat => {
            if left == right && left == TypeDef::simple("String") {
                Ok(TypeDef::simple("String"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        ListConcat => {
            if let TypeDef::Simple {
                ref name,
                subtype: _,
            } = left
            {
                if left == right && name == &ident("List") {
                    Ok(left)
                } else {
                    Err(format!(
                        "Invalid use of operator: {} {:?} {}",
                        left, bin_opt.operator, right
                    ))
                }
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
    }
}
