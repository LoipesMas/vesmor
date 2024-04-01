use std::collections::HashMap;

use crate::{
    ast::{BinaryOperation, BinaryOperator, EnumPattern, Expr, Ident},
    utils::ident,
};

pub fn generic_option_type() -> Type {
    Type::Enum {
        enu: ident("Option"),
        variants: [(ident("None"), None), (ident("Some"), Some(Type::Hole))].into(),
    }
}

pub fn generic_option_type_name() -> TypeName {
    TypeName::Normal(NormalTypeName {
        name: ident("Option"),
        subtype: Some(Box::new(type_hole_name())),
    })
}

pub fn concrete_option_type(typ: Type) -> Type {
    Type::Enum {
        enu: ident("Option"),
        variants: [(ident("None"), None), (ident("Some"), Some(typ))].into(),
    }
}

pub fn generic_list_type() -> Type {
    Type::Simple {
        name: ident("List"),
        subtype: Some(Box::new(Type::Hole)),
    }
}

pub fn type_hole_name() -> NormalTypeName {
    NormalTypeName {
        name: ident("_"),
        subtype: None,
    }
}

pub fn generic_list_type_name() -> TypeName {
    TypeName::Normal(NormalTypeName {
        name: ident("List"),
        subtype: Some(Box::new(type_hole_name())),
    })
}

pub fn bool_enum() -> Type {
    Type::Enum {
        enu: ident("Bool"),
        variants: [(ident("True"), None), (ident("False"), None)].into(),
    }
}

pub fn int_type() -> Type {
    Type::Simple {
        name: ident("Int"),
        subtype: None,
    }
}

pub fn float_type() -> Type {
    Type::Simple {
        name: ident("Float"),
        subtype: None,
    }
}

pub fn string_type() -> Type {
    Type::Simple {
        name: ident("String"),
        subtype: None,
    }
}

pub fn default_type_definitions() -> TypeMap {
    [
        (ident("Option"), generic_option_type()),
        (ident("List"), generic_list_type()),
        (ident("Bool"), bool_enum()),
        (ident("Int"), int_type()),
        (ident("Float"), float_type()),
        (ident("String"), string_type()),
    ]
    .into()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Hole,
    Simple {
        name: Ident,
        subtype: Option<Box<Type>>,
    },
    Record(TypeMap),
    Enum {
        enu: Ident,
        variants: HashMap<Ident, Option<Type>>,
    },
    Function {
        args: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl Type {
    pub fn simple(name: &str) -> Self {
        Self::Simple {
            name: ident(name),
            subtype: None,
        }
    }

    pub fn matches(&self, other: &Self) -> Result<Self, String> {
        if self == other {
            return Ok(self.clone());
        }
        if matches!(self, Type::Hole) {
            return Ok(other.clone());
        };
        if matches!(other, Type::Hole) {
            return Ok(self.clone());
        };
        match (self, other) {
            (
                Type::Simple {
                    name: name_a,
                    subtype: subtype_a,
                },
                Type::Simple {
                    name: name_b,
                    subtype: subtype_b,
                },
            ) => {
                if name_a != name_b {
                    Err(format!("Types don't match: {self} and {other}"))
                } else {
                    match (subtype_a, subtype_b) {
                        (None, None) => Ok(self.clone()),
                        (Some(sa), Some(sb)) => Ok(Type::Simple {
                            name: name_a.clone(),
                            subtype: Some(Box::new(sa.matches(sb)?)),
                        }),
                        _ => Err(format!("Types don't match: {self} and {other}")),
                    }
                }
            }
            (
                Type::Enum {
                    enu: enu_a,
                    variants: variants_a,
                },
                Type::Enum {
                    enu: enu_b,
                    variants: variants_b,
                },
            ) => {
                if enu_a != enu_b {
                    Err(format!("Types don't match: {self} and {other}"))
                } else {
                    let mut variants_a_k = variants_a.keys().collect::<Vec<_>>();
                    variants_a_k.sort_by_key(|i| &i.0);
                    let mut variants_b_k = variants_b.keys().collect::<Vec<_>>();
                    variants_b_k.sort_by_key(|i| &i.0);
                    if variants_a_k != variants_b_k {
                        Err(format!("Types don't match: {self} and {other}"))
                    } else {
                        let mut new_variants = variants_a.clone();
                        for k in variants_a_k {
                            let typ_a = variants_a.get(k).expect("Using known keys");
                            let typ_b = variants_b.get(k).expect("Using known keys");
                            match (typ_a, typ_b) {
                                (None, None) => {}
                                (Some(a), Some(b)) => {
                                    new_variants.insert(k.clone(), Some(a.matches(b)?));
                                }
                                _ => {
                                    return Err(format!("Types don't match: {self} and {other}"));
                                }
                            }
                        }
                        Ok(Type::Enum {
                            enu: enu_a.clone(),
                            variants: new_variants,
                        })
                    }
                }
            }
            _ => Err(format!("Types don't match: {self} and {other}")),
        }
    }

    pub fn fill_type_hole(self, typ: Type) -> Self {
        match self {
            Type::Hole => typ,
            Type::Simple {
                ref name,
                ref subtype,
            } => match subtype.as_deref() {
                Some(Type::Hole) => Type::Simple {
                    name: name.clone(),
                    subtype: Some(Box::new(typ)),
                },
                Some(_) => todo!(),
                None => self,
            },
            Type::Record(_) => todo!(),
            Type::Enum { enu, variants } => {
                let mut new_variants = variants.clone();
                for (k, variant) in variants {
                    if let Some(v) = variant {
                        new_variants.insert(k, Some(v.fill_type_hole(typ.clone())));
                    }
                }
                Type::Enum {
                    enu,
                    variants: new_variants,
                }
            }
            Type::Function { args, return_type } => todo!(),
        }
    }
}

impl std::fmt::Display for Type {
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
            Self::Hole => {
                write!(f, "_")
            }
            _ => write!(f, "{self:?}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct NormalTypeName {
    pub name: Ident,
    pub subtype: Option<Box<NormalTypeName>>,
}

#[derive(Clone, Debug)]
pub struct FunctionTypeName {
    pub args: Vec<NormalTypeName>,
    pub return_type: NormalTypeName,
}

#[derive(Clone, Debug)]
pub enum TypeName {
    Normal(NormalTypeName),
    Function(FunctionTypeName),
}

impl NormalTypeName {
    pub fn to_type(&self, type_definitions: &TypeMap) -> Result<Type, String> {
        if let Some(typ) = type_definitions.get(&self.name) {
            if let Some(subtype) = &self.subtype {
                if subtype.name.0 == "_" {
                    Ok(typ.clone())
                } else {
                    let subtype = subtype.to_type(type_definitions)?;
                    let new_typ = typ.clone().fill_type_hole(subtype);
                    Ok(new_typ)
                }
            } else {
                Ok(typ.clone())
            }
        } else {
            Err(format!("Unknown type: {}", self.name.0))
        }
    }
}

impl FunctionTypeName {
    pub fn to_type(&self, type_definitions: &TypeMap) -> Result<Type, String> {
        let arg_types = self
            .args
            .iter()
            .map(|t| t.to_type(type_definitions))
            .collect::<Result<Vec<_>, _>>()?;
        let return_type_type = self.return_type.to_type(type_definitions)?;
        Ok(Type::Function {
            args: arg_types,
            return_type: Box::new(return_type_type),
        })
    }
}

impl TypeName {
    pub fn to_type(&self, type_definitions: &TypeMap) -> Result<Type, String> {
        match self {
            TypeName::Normal(n) => n.to_type(type_definitions),
            TypeName::Function(f) => f.to_type(type_definitions),
        }
    }
}

type TypeMap = HashMap<Ident, Type>;

trait Scope {
    fn get(&self, ident: &Ident) -> Option<&Type>;
}

type TwoScopes<'a> = (&'a TypeMap, &'a TypeMap);
impl Scope for TwoScopes<'_> {
    fn get(&self, ident: &Ident) -> Option<&Type> {
        self.1.get(ident).or_else(|| self.0.get(ident))
    }
}

// Scopes are for `variable -> type` (e.g., "x" -> Int)
// type_definitions is for `type_name -> type` (e.g. "Option" -> Enum { name: Option, variants:...})
pub fn check(
    global_scope: &TypeMap,
    local_scope: &TypeMap,
    type_definitions: &TypeMap,
    expr: &Expr,
) -> Result<Type, String> {
    let combined_scope = (global_scope, local_scope);
    let c = |e| check(global_scope, local_scope, type_definitions, e);
    match expr {
        Expr::Int(_) => Ok(Type::simple("Int")),
        Expr::Float(_) => Ok(Type::simple("Float")),
        Expr::String(_) => Ok(Type::simple("String")),
        Expr::List(list) => {
            let types = list.iter().map(c).collect::<Result<Vec<_>, _>>()?;
            match &types[..] {
                [] => Ok(Type::Simple {
                    name: ident("List"),
                    subtype: Some(Box::new(Type::Hole)),
                }),
                [subtype] => Ok(Type::Simple {
                    name: ident("List"),
                    subtype: Some(Box::new(subtype.clone())),
                }),
                [first, ..] => {
                    let all_same = types.windows(2).all(|s| s[0] == s[1]);
                    if all_same {
                        Ok(Type::Simple {
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
            check(global_scope, &new_scope, type_definitions, &b.expr)
        }
        Expr::Value(v) => combined_scope
            .get(v)
            .cloned()
            .ok_or_else(|| format!("Type for value '{}' not found!", v.0)),
        Expr::BinaryOperation(bin_opt) => {
            check_bin_opt(global_scope, local_scope, type_definitions, bin_opt)
        }
        Expr::Function(f) => {
            let mut new_scope = TypeMap::new();
            for arg in &f.arguments {
                let arg_type = arg.typ.to_type(type_definitions)?;
                new_scope.insert(arg.name.clone(), arg_type);
            }
            let evaled_type = check(global_scope, &new_scope, type_definitions, &f.body)?;
            let return_type_type = f.return_type.to_type(type_definitions)?;
            let matched = return_type_type.matches(&evaled_type)?;
            Ok(matched)
        }
        Expr::BuiltInFunction(_) => unreachable!("This shouldn't have been checked"),
        Expr::FunctionCall(fc) => {
            let func_type = c(&fc.function)?;
            if let Type::Function { args, return_type } = func_type {
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
                if let Type::Record(base_r) = base_type {
                    for (k, v) in base_r.iter() {
                        if let Some(new_type) = new_types.insert(k.clone(), v.clone()) {
                            if &new_type != v {
                                return Err(format!(
                                    "Incorrect new type for {k:?}, expected {v}, got {new_type}"
                                ));
                            }
                        }
                    }
                    Ok(Type::Record(new_types))
                } else {
                    Err(format!(
                        "Base argument should be a record, got {base_type} instead"
                    ))
                }
            } else {
                Ok(Type::Record(new_types))
            }
        }
        Expr::RecordAccess(ra) => {
            let rec_type = c(&ra.record)?;
            if let Type::Record(ref map) = rec_type {
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
            if let Some(enum_def) = type_definitions.get(&ev.enu) {
                if let Type::Enum { enu, variants } = enum_def {
                    if let Some(variant) = variants.get(&ev.variant) {
                        match (&ev.body, variant) {
                            (None, None) => Ok(enum_def.clone()),
                            (None, Some(t)) => Err(format!(
                                "Enum variant {}::{} requires a body of type {}",
                                &ev.enu.0, &ev.variant.0, t
                            )),
                            (Some(a), None) => Err(format!(
                                "Enum variant {}::{} can't have a body, but got type {}",
                                &ev.enu.0,
                                &ev.variant.0,
                                c(a)?
                            )),
                            (Some(a), Some(b)) => {
                                let a_typ = c(a)?;
                                let matched = a_typ.matches(b)?;
                                let mut new_variants = variants.clone();
                                new_variants.insert(ev.variant.clone(), Some(matched));
                                Ok(Type::Enum {
                                    enu: ev.enu.clone(),
                                    variants: new_variants,
                                })
                            }
                        }
                    } else {
                        Err(format!(
                            "Enum {} doesn't have variant {}",
                            &ev.enu.0, &ev.variant.0
                        ))
                    }
                } else {
                    Err(format!("Expected enum type, got {:?}", &ev.enu.0))
                }
            } else {
                Err(format!("Unknown type: {:?}", &ev.enu.0))
            }
        }
        Expr::EnumMatching(em) => {
            let value_type = c(&em.value)?;
            if let Type::Enum {
                ref enu,
                ref variants,
            } = value_type
            {
                let mut branch_types = vec![];
                for branch in &em.branches {
                    // TODO: this is ugly, refactor plz
                    let branch_type = match &branch.pattern {
                        EnumPattern::Variant { variant, bind } => {
                            if let Some(variant_body) = variants.get(variant) {
                                match (bind, variant_body) {
                                    (None, None) => c(&branch.expr),
                                    (None, Some(t)) => Err(format!(
                                        "'{}::{}`' has a body of type {}, which wasn't bound.",
                                        enu.0, variant.0, t
                                    )),
                                    (Some(_), None) => Err(format!(
                                        "'{}::{}`' doesn't have a body, but binding was attempted.",
                                        enu.0, variant.0
                                    )),
                                    (Some(bind), Some(variant_body_typ)) => {
                                        let mut new_scope = local_scope.clone();
                                        new_scope.insert(bind.clone(), variant_body_typ.clone());
                                        check(
                                            global_scope,
                                            &new_scope,
                                            type_definitions,
                                            &branch.expr,
                                        )
                                    }
                                }
                            } else {
                                Err(format!(
                                    "Enum '{}' doesn't have variant {}",
                                    enu.0, variant.0
                                ))
                            }
                        }
                        EnumPattern::Any { bind } => {
                            let mut new_scope = local_scope.clone();
                            new_scope.insert(bind.clone(), value_type.clone());
                            check(global_scope, &new_scope, type_definitions, &branch.expr)
                        }
                    }?;
                    branch_types.push(branch_type);
                }
                let result_type = branch_types
                    .iter()
                    .try_fold(branch_types[0].clone(), |a, x| a.matches(x));
                result_type.map_err(|e| format!("Branch type mismatch: {e}"))
            } else {
                Err(format!("Enum matching requires an enum, got {value_type}"))
            }
        }
    }
}

fn check_bin_opt(
    global_scope: &TypeMap,
    local_scope: &TypeMap,
    type_definitions: &TypeMap,
    bin_opt: &BinaryOperation,
) -> Result<Type, String> {
    let left = check(global_scope, local_scope, type_definitions, &bin_opt.left)?;
    let right = check(global_scope, local_scope, type_definitions, &bin_opt.right)?;
    use BinaryOperator::*;
    match bin_opt.operator {
        IntAdd | IntSub | IntMul | IntDiv | IntMod => {
            if left == right && left == Type::simple("Int") {
                Ok(Type::simple("Int"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        FloatAdd | FloatSub | FloatMul | FloatDiv | FloatLT | FloatGT => {
            if left == right && left == Type::simple("Float") {
                Ok(Type::simple("Float"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        BoolOr | BoolAnd => {
            if left == right && left == Type::simple("Bool") {
                Ok(bool_enum())
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        StringConcat => {
            if left == right && left == Type::simple("String") {
                Ok(Type::simple("String"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        ListConcat => {
            if let Type::Simple {
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
