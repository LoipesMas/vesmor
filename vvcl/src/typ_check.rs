use std::collections::{HashMap, HashSet};

use crate::{
    ast::{BinaryOperation, BinaryOperator, EnumPattern, Expr, Function, Ident, Record},
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
        (ident("_"), Type::Hole),
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

    pub(crate) fn from_function_def_unchecked(fun: &Expr, type_definitions: &TypeMap) -> Self {
        if let Expr::Function(fun) = fun {
            Type::Function {
                args: fun
                    .arguments
                    .iter()
                    .map(|d| d.typ.to_type(type_definitions).unwrap())
                    .collect(),
                return_type: Box::new(fun.return_type.to_type(type_definitions).unwrap()),
            }
        } else {
            panic!("gimme function")
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
            (Type::Record(a), Type::Record(b)) => {
                let a_keys = a.keys().collect::<HashSet<_>>();
                let b_keys = b.keys().collect::<HashSet<_>>();
                if a_keys != b_keys {
                    Err(format!("Types don't match: {self} and {other}"))
                } else {
                    let mut new_members = HashMap::new();
                    for key in a_keys {
                        let member_a = a.get(key).expect("Iterating using known keys.");
                        let member_b = b.get(key).expect("Iterating using known keys.");
                        new_members.insert(key.clone(), member_a.matches(member_b)?);
                    }
                    Ok(Type::Record(new_members))
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

    pub fn has_holes(&self) -> bool {
        match self {
            Type::Hole => true,
            Type::Simple { name: _, subtype } => {
                subtype.as_deref().map(Type::has_holes).unwrap_or(false)
            }
            Type::Record(r) => r.values().any(Type::has_holes),
            Type::Enum { enu: _, variants } => variants
                .values()
                .any(|v| v.as_ref().map(Type::has_holes).unwrap_or(false)),
            Type::Function { args, return_type } => {
                args.iter().any(Type::has_holes) || return_type.has_holes()
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple { name, subtype } => {
                if let Some(ref subtype) = subtype {
                    write!(f, "{name}<{subtype}>")
                } else {
                    write!(f, "{name}")
                }
            }
            Self::Record(map) => {
                write!(f, "{{")?;
                for (k, v) in map.iter() {
                    write!(f, "{k}: {v}, ")?;
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
pub struct RecordTypeName {
    pub members: HashMap<Ident, TypeName>,
}

#[derive(Clone, Debug)]
pub struct EnumTypeName {
    pub enu: Ident,
    pub variants: HashMap<Ident, Option<TypeName>>,
}

#[derive(Clone, Debug)]
pub enum TypeName {
    Normal(NormalTypeName),
    Function(FunctionTypeName),
    Record(RecordTypeName),
    Enum(EnumTypeName),
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
            Err(format!("Unknown type: {}", self.name))
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

impl RecordTypeName {
    pub fn to_type(&self, type_definitions: &TypeMap) -> Result<Type, String> {
        let member_types = self
            .members
            .iter()
            .map(|(k, t)| t.to_type(type_definitions).map(|t| (k.clone(), t)))
            .collect::<Result<HashMap<_, _>, _>>()?;
        Ok(Type::Record(member_types))
    }
}

impl EnumTypeName {
    pub fn to_type(&self, type_definitions: &TypeMap) -> Result<Type, String> {
        // bruh what is this
        let variants = self
            .variants
            .iter()
            .map(|(k, v)| {
                v.as_ref().map_or_else(
                    || Ok((k.clone(), None)),
                    |v| v.to_type(type_definitions).map(|x| (k.clone(), Some(x))),
                )
            })
            .collect::<Result<_, _>>()?;
        Ok(Type::Enum {
            enu: self.enu.clone(),
            variants,
        })
    }
}

impl TypeName {
    pub fn to_type(&self, type_definitions: &TypeMap) -> Result<Type, String> {
        match self {
            TypeName::Normal(n) => n.to_type(type_definitions),
            TypeName::Function(f) => f.to_type(type_definitions),
            TypeName::Record(r) => r.to_type(type_definitions),
            TypeName::Enum(e) => e.to_type(type_definitions),
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
                new_scope.insert(
                    def.name.clone(),
                    check(global_scope, &new_scope, type_definitions, &def.body)?,
                );
            }
            check(global_scope, &new_scope, type_definitions, &b.expr)
        }
        Expr::Value(v) => combined_scope
            .get(v)
            .cloned()
            .ok_or_else(|| format!("Type for value '{v}' not found!")),
        Expr::BinaryOperation(bin_opt) => {
            check_bin_opt(global_scope, local_scope, type_definitions, bin_opt)
        }
        Expr::Function(f) => {
            let mut new_scope = TypeMap::new();
            let mut args = Vec::with_capacity(f.arguments.len());
            for arg in &f.arguments {
                let arg_type = arg.typ.to_type(type_definitions)?;
                if arg_type.has_holes() {
                    return Err(format!(
                        "Argument types with holes not supported (yet). ({arg_type})"
                    ));
                }
                args.push(arg_type.clone());
                new_scope.insert(arg.name.clone(), arg_type);
            }
            let evaled_type = check(global_scope, &new_scope, type_definitions, &f.body)?;
            let return_type_type = f.return_type.to_type(type_definitions)?;
            if return_type_type.has_holes() {
                Err(format!(
                    "Returning types with holes not supported (yet). ({return_type_type})"
                ))
            } else {
                let matched = return_type_type.matches(&evaled_type)?;
                Ok(Type::Function {
                    args,
                    return_type: Box::new(matched),
                })
            }
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
                        // TODO: I think we should create new temporary function `Expr` that takes
                        // remaining arguments and in the body calls the target function
                        Err("Partial application not implemented yet!".to_string())
                    }
                    std::cmp::Ordering::Equal => {
                        if args.iter().any(|a| a.has_holes()) {
                            // TODO: this is a HACK for builtin functions
                            Ok(*return_type)
                        } else if call_arg_types.iter().zip(args.iter()).all(|(a, b)| a == b) {
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
                                    "Incorrect new type for member '{k}', expected {v}, got {new_type}"
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
                        &ra.member
                    ))
                }
            } else {
                Err(format!(
                    "Tried to access member {} of non-record type {rec_type}",
                    &ra.member
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
                                &ev.enu, &ev.variant, t
                            )),
                            (Some(a), None) => Err(format!(
                                "Enum variant {}::{} can't have a body, but got type {}",
                                &ev.enu,
                                &ev.variant,
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
                            &ev.enu, &ev.variant
                        ))
                    }
                } else {
                    Err(format!("Expected enum type, got {:?}", &ev.enu))
                }
            } else {
                Err(format!("Unknown type: {:?}", &ev.enu))
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
                let mut not_covered_variants: HashSet<String> =
                    variants.keys().map(|v| v.0.to_owned()).collect();
                for branch in &em.branches {
                    // TODO: this is ugly, refactor plz
                    let branch_type = match &branch.pattern {
                        EnumPattern::Variant { variant, bind } => {
                            if let Some(variant_body) = variants.get(variant) {
                                if !not_covered_variants.remove(&variant.0) {
                                    return Err(format!("'{enu}::{variant}' was already covered!"));
                                }
                                match (bind, variant_body) {
                                    (None, None) => c(&branch.expr),
                                    (None, Some(t)) => Err(format!(
                                        "'{enu}::{variant}`' has a body of type {t}, which wasn't bound.",
                                    )),
                                    (Some(_), None) => Err(format!(
                                        "'{enu}::{variant}`' doesn't have a body, but binding was attempted.",
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
                                Err(format!("Enum '{enu}' doesn't have variant {variant}",))
                            }
                        }
                        EnumPattern::Any { bind } => {
                            let mut new_scope = local_scope.clone();
                            new_scope.insert(bind.clone(), value_type.clone());
                            not_covered_variants = HashSet::new();
                            check(global_scope, &new_scope, type_definitions, &branch.expr)
                        }
                    }?;
                    branch_types.push(branch_type);
                }
                if !not_covered_variants.is_empty() {
                    Err(format!(
                        "Following variants of enum '{enu}' were not covered: {}",
                        not_covered_variants
                            .iter()
                            .fold(String::new(), |a, x| a + x + ", ")
                    ))
                } else {
                    let result_type = branch_types
                        .iter()
                        .try_fold(branch_types[0].clone(), |a, x| a.matches(x));
                    result_type.map_err(|e| format!("Branch type mismatch: {e}"))
                }
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
        FloatAdd | FloatSub | FloatMul | FloatDiv => {
            if left == right && left == Type::simple("Float") {
                Ok(Type::simple("Float"))
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        FloatLT | FloatGT => {
            if left == right && left == Type::simple("Float") {
                Ok(bool_enum())
            } else {
                Err(format!(
                    "Invalid use of operator: {} {:?} {}",
                    left, bin_opt.operator, right
                ))
            }
        }
        BoolOr | BoolAnd => {
            if left == right && left == bool_enum() {
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
