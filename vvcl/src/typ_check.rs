use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
};

use crate::{
    ast::{BinaryOperation, BinaryOperator, EnumPattern, Expr, Ident, RExpr},
    utils::{ident, ErrWithContext},
};

pub fn generic_option_type() -> Type {
    Type::Enum {
        enu: ident("Option"),
        variants: [
            (ident("None"), None),
            (ident("Some"), Some(Type::Hole(ident("T")))),
        ]
        .into(),
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
        subtype: Some(Box::new(Type::Hole(ident("T")))),
    }
}

pub fn concrete_list_type(typ: Type) -> Type {
    Type::Simple {
        name: ident("List"),
        subtype: Some(Box::new(typ)),
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
        (ident("_"), Type::Hole(ident("T"))),
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
    Hole(Ident),
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

    pub fn from_function_def_unchecked(
        fun: &Expr,
        type_definitions: &TypeMap,
    ) -> Result<Self, String> {
        if let Expr::Function(fun) = fun {
            Ok(Type::Function {
                args: fun
                    .arguments
                    .iter()
                    .map(|d| d.typ.to_type(type_definitions))
                    .collect::<Result<_, _>>()?,
                return_type: Box::new(fun.return_type.to_type(type_definitions)?),
            })
        } else {
            Err("Expected a function to `from_function_def_unchecked`".to_owned())
        }
    }

    pub fn matches(&self, other: &Self) -> Result<Self, String> {
        if self == other {
            return Ok(self.clone());
        }
        if matches!(self, Type::Hole(_)) {
            return Ok(other.clone());
        };
        if matches!(other, Type::Hole(_)) {
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
                            subtype: Some(Box::new(
                                sa.matches(sb)
                                    .with_context(format!("In the subtype of {name_a}: "))?,
                            )),
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
                        Err(format!("Enums have different variants: {self} and {other}"))
                    } else {
                        let mut new_variants = variants_a.clone();
                        for k in variants_a_k {
                            let typ_a = variants_a.get(k).expect("Using known keys");
                            let typ_b = variants_b.get(k).expect("Using known keys");
                            match (typ_a, typ_b) {
                                (None, None) => {}
                                (Some(a), Some(b)) => {
                                    new_variants.insert(
                                        k.clone(),
                                        Some(a.matches(b).with_context(format!(
                                            "In enum variant {enu_a}::{k}: "
                                        ))?),
                                    );
                                }
                                _ => {
                                    return Err(
                                        format!("In enum variant {enu_a}::{k}: Types don't match: {self} and {other}")
                                    );
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
                        new_members.insert(
                            key.clone(),
                            member_a
                                .matches(member_b)
                                .with_context(format!("In record member {key}: "))?,
                        );
                    }
                    Ok(Type::Record(new_members))
                }
            }
            (
                Type::Function {
                    args: args_a,
                    return_type: return_type_a,
                },
                Type::Function {
                    args: args_b,
                    return_type: return_type_b,
                },
            ) => {
                if args_a.len() != args_b.len() {
                    return Err(format!("Types don't match: {self} and {other}"));
                }
                let args = args_a
                    .iter()
                    .zip(args_b.iter())
                    .map(|(a, b)| a.matches(b))
                    .collect::<Result<_, _>>()
                    .with_context("In arguments of functions: ".to_owned())?;
                let return_type = return_type_a
                    .matches(return_type_b)
                    .with_context("In return type of functions: ".to_owned())?;
                Ok(Type::Function {
                    args,
                    return_type: Box::new(return_type),
                })
            }
            _ => Err(format!("Types don't match: {self} and {other}")),
        }
    }

    // TODO: use type hole ident?
    pub fn fill_type_hole(self, typ: Type) -> Self {
        match self {
            Type::Hole(_) => typ,
            Type::Simple {
                ref name,
                ref subtype,
            } => match subtype.as_deref() {
                Some(Type::Hole(_)) => Type::Simple {
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

    pub fn fill_type_holes_from_map(self, map: &HashMap<Ident, Type>) -> Self {
        match self {
            Type::Hole(i) => {
                if let Some(t) = map.get(&i) {
                    t.clone()
                } else {
                    Type::Hole(i)
                }
            }
            Type::Simple {
                ref name,
                ref subtype,
            } => match subtype.clone() {
                Some(subtype) => Type::Simple {
                    name: name.clone(),
                    subtype: Some(Box::new(subtype.fill_type_holes_from_map(map))),
                },
                None => self,
            },
            Type::Record(_) => todo!(),
            Type::Enum { enu, variants } => {
                todo!()
            }
            Type::Function { args, return_type } => {
                let args = args
                    .into_iter()
                    .map(|a| a.fill_type_holes_from_map(map))
                    .collect();
                let return_type = Box::new(return_type.fill_type_holes_from_map(map));
                Type::Function { args, return_type }
            }
        }
    }

    pub fn has_holes(&self) -> bool {
        match self {
            Type::Hole(_) => true,
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
            Self::Hole(i) => {
                write!(f, "{i}")
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
        if let Some(r) = self.name.0.strip_prefix('*') {
            return Ok(Type::Hole(ident(r)));
        };
        if let Some(typ) = type_definitions.get(&self.name) {
            if let Some(subtype) = &self.subtype {
                let subtype = subtype.to_type(type_definitions)?;
                let new_typ = typ.clone().fill_type_hole(subtype);
                Ok(new_typ)
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
    expr: RExpr,
) -> Result<Type, String> {
    let combined_scope = (global_scope, local_scope);
    let c = |e| check(global_scope, local_scope, type_definitions, e);
    match expr.borrow() {
        Expr::Int(_) => Ok(Type::simple("Int")),
        Expr::Float(_) => Ok(Type::simple("Float")),
        Expr::String(_) => Ok(Type::simple("String")),
        Expr::List(list) => {
            let types = list
                .iter()
                .map(|v| c(v.clone()))
                .collect::<Result<Vec<_>, _>>()
                .with_context("In list values: ".to_owned())?;
            match &types[..] {
                [] => Ok(Type::Simple {
                    name: ident("List"),
                    subtype: Some(Box::new(Type::Hole(ident("T")))),
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
            // FIXME: just having two passes doesn't solve it...
            // first pass
            for def in &b.definitions {
                if combined_scope.get(&def.name).is_some() || new_scope.contains_key(&def.name) {
                    return Err(format!("'{}' already defined.", def.name))
                        .with_context("In block body: ".to_owned());
                }
                let typ = check(global_scope, &new_scope, type_definitions, def.body.clone())
                    .with_context(format!("In definition of '{}': ", def.name));
                // can fail, because they can rely on other definitions
                if let Ok(typ) = typ {
                    new_scope.insert(def.name.clone(), typ);
                }
            }
            // second pass
            for def in &b.definitions {
                let typ = check(global_scope, &new_scope, type_definitions, def.body.clone())
                    .with_context(format!("In definition of '{}': ", def.name))?;
                new_scope.insert(def.name.clone(), typ);
            }
            check(global_scope, &new_scope, type_definitions, b.expr.clone())
                .with_context("In block body: ".to_owned())
        }
        Expr::Value(v) => combined_scope.get(&v).cloned().ok_or_else(|| {
            format!(
                "Type for value '{v}' not found! (Probably undefined, possibly type checker error)"
            )
        }),
        Expr::BinaryOperation(bin_opt) => {
            check_bin_opt(global_scope, local_scope, type_definitions, &bin_opt)
                .with_context(format!("In binary operation '{:?}': ", bin_opt.operator))
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
            let evaled_type = check(global_scope, &new_scope, type_definitions, f.body.clone())
                .with_context("In function body: ".to_owned())?;
            let return_type_type = f.return_type.to_type(type_definitions)?;
            if return_type_type.has_holes() {
                Err(format!(
                    "Returning types with holes not supported (yet). ({return_type_type})"
                ))
            } else {
                let matched = return_type_type
                    .matches(&evaled_type)
                    .with_context("In function return type: ".to_owned())?;
                Ok(Type::Function {
                    args,
                    return_type: Box::new(matched),
                })
            }
        }
        Expr::BuiltInFunction(_) => unreachable!("This shouldn't have been checked"),
        Expr::FunctionCall(fc) => {
            let func_type = c(fc.function.clone())?;
            if let Type::Function { args, return_type } = func_type {
                let call_arg_types = fc
                    .arguments
                    .iter()
                    .map(|v| c(v.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                match call_arg_types.len().cmp(&args.len()) {
                    std::cmp::Ordering::Greater => {
                        Err("Called function with too many arguments".to_string())
                    }
                    std::cmp::Ordering::Less => Ok(Type::Function {
                        args: args.into_iter().skip(call_arg_types.len()).collect(),
                        return_type,
                    }),
                    std::cmp::Ordering::Equal => {
                        if args.iter().any(|a| a.has_holes()) {
                            //TODO: check rough matching

                            // TODO: add context
                            typ_check_generic_function(args, *return_type, call_arg_types)
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
                new_types.insert(
                    k.clone(),
                    c(v.clone()).with_context(format!("In record member '{k}': "))?,
                );
            }
            if let Some(base) = &r.base {
                let base_type = c(base.clone())?;
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
                        "Base argument for update should be a record, got {base_type} instead"
                    ))
                }
            } else {
                Ok(Type::Record(new_types))
            }
        }
        Expr::RecordAccess(ra) => {
            let rec_type = c(ra.record.clone()).with_context("In record access: ".to_owned())?;
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
                                c(a.clone())?
                            )),
                            (Some(a), Some(b)) => {
                                let a_typ = c(a.clone())?;
                                let matched = a_typ.matches(b).with_context(format!(
                                    "In body of enum variant {}::{}: ",
                                    ev.enu, ev.variant
                                ))?;
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
            let value_type =
                c(em.value.clone()).with_context("In enum matching value: ".to_owned())?;
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
                                    (None, None) => c(branch.expr.clone()),
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
                                            branch.expr.clone(),
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
                            check(
                                global_scope,
                                &new_scope,
                                type_definitions,
                                branch.expr.clone(),
                            )
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
                    result_type.with_context("Enum matching branch type mismatch: ".to_string())
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
    let left = check(
        global_scope,
        local_scope,
        type_definitions,
        bin_opt.left.clone(),
    )?;
    let right = check(
        global_scope,
        local_scope,
        type_definitions,
        bin_opt.right.clone(),
    )?;
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
        IntLT | IntGT | IntEQ => {
            if left == right && left == Type::simple("Int") {
                Ok(bool_enum())
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
        FloatLT | FloatGT | FloatEQ => {
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

fn checked_extend(
    mut a: HashMap<Ident, Type>,
    b: HashMap<Ident, Type>,
) -> Result<HashMap<Ident, Type>, String> {
    for (k, v) in b.into_iter() {
        let prev = a.insert(k.clone(), v.clone());
        if let Some(prev) = prev {
            if prev != v {
                return Err(format!("type mismatch for type '{k}': '{prev}' and '{v}'"));
            }
        }
    }
    Ok(a)
}

fn lock_subtypes(generic_type: Type, concrete_type: Type) -> Result<HashMap<Ident, Type>, String> {
    if !generic_type.has_holes() {
        return Ok([].into());
    }
    use Type::*;
    match (generic_type, concrete_type.clone()) {
        (Hole(i), _) => Ok([(i, concrete_type)].into()),
        (
            Simple {
                name: _,
                subtype: subtype_g,
            },
            Simple {
                name: _,
                subtype: subtype_c,
            },
        ) => {
            let subtype_g = subtype_g.expect("where hole, huh?");
            let subtype_c = subtype_c.expect("should match smh");
            // TODO: add context
            lock_subtypes((*subtype_g).to_owned(), (*subtype_c).to_owned())
        }
        (
            Function {
                args: args_g,
                return_type: ret_type_g,
            },
            Function {
                args: args_c,
                return_type: ret_type_c,
            },
        ) => {
            if args_g.len() != args_c.len() {
                return Err("not matching".to_owned());
            }
            let mut accum: HashMap<Ident, Type> = HashMap::new();
            for (arg_g, arg_c) in args_g.iter().zip(args_c.iter()) {
                // TODO: add context
                let res = lock_subtypes(arg_g.clone(), arg_c.clone())?;
                // TODO: add context
                accum = checked_extend(accum, res)?;
            }
            // TODO: add context
            let res = lock_subtypes((*ret_type_g).clone(), (*ret_type_c).clone())?;
            // TODO: add context
            accum = checked_extend(accum, res)?;
            Ok(accum)
        }
        (_, _) => {
            todo!()
        }
    }
}

fn typ_check_generic_function(
    args_g: Vec<Type>,
    ret_type_g: Type,
    args_c: Vec<Type>,
) -> Result<Type, String> {
    let mut hole_mapping: HashMap<Ident, Type> = HashMap::new();

    for (arg_g, arg_c) in args_g.iter().zip(args_c.iter()) {
        if !arg_g.has_holes() {
            // nothing to do
            continue;
        }
        if arg_c.has_holes() {
            todo!("arguments with type holes to generic function")
        }

        // TODO: add context
        let arg_c = arg_g.matches(arg_c)?;
        // TODO: add context
        let res = lock_subtypes(arg_g.clone(), arg_c)?;
        hole_mapping = checked_extend(hole_mapping, res)?
    }
    let return_type = ret_type_g.fill_type_holes_from_map(&hole_mapping);

    if return_type.has_holes() {
        todo!(
            "{}, {:#?}",
            return_type,
            hole_mapping.into_iter().collect::<Vec<_>>()
        )
    }

    Ok(return_type)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_checked_extend_ok() {
        let a = [(ident("*A"), int_type())].into();
        let b = [(ident("*B"), float_type())].into();
        let res = checked_extend(a, b).unwrap();
        let exp = [(ident("*A"), int_type()), (ident("*B"), float_type())].into();
        assert_eq!(res, exp);
    }

    #[test]
    fn test_checked_extend_ok_2() {
        let a = [(ident("*A"), int_type())].into();
        let b = [(ident("*A"), int_type()), (ident("*B"), float_type())].into();
        let res = checked_extend(a, b).unwrap();
        let exp = [(ident("*A"), int_type()), (ident("*B"), float_type())].into();
        assert_eq!(res, exp);
    }

    #[test]
    fn test_checked_extend_err() {
        let a = [(ident("*A"), int_type())].into();
        let b = [(ident("*A"), float_type()), (ident("*B"), float_type())].into();
        let res = checked_extend(a, b).unwrap_err();
        let exp_msg = "type mismatch for type '*A': 'Int' and 'Float'";
        assert_eq!(&res, exp_msg);
    }

    #[test]
    fn test_lock_subtypes_hole() {
        let generic_type = Type::Hole(ident("*A"));
        let concrete_type = int_type();
        let result = lock_subtypes(generic_type, concrete_type.clone()).unwrap();
        let res_type = result.get(&ident("*A")).unwrap();
        assert_eq!(res_type, &concrete_type);
    }

    #[test]
    fn test_lock_subtypes_subtype() {
        let generic_type = Type::Simple {
            name: ident("List"),
            subtype: Some(Box::new(Type::Hole(ident("*A")))),
        };
        let concrete_type = concrete_list_type(float_type());
        let result = lock_subtypes(generic_type, concrete_type).unwrap();
        let res_type = result.get(&ident("*A")).unwrap();
        assert_eq!(res_type, &float_type());
    }

    #[test]
    fn test_lock_subtypes_noop() {
        let generic_type = float_type();
        let concrete_type = float_type();
        let result = lock_subtypes(generic_type, concrete_type).unwrap();
        assert!(result.is_empty())
    }

    #[test]
    fn test_lock_subtypes_func_arg() {
        let generic_type = Type::Function {
            args: vec![Type::Hole(ident("*A"))],
            return_type: Box::new(float_type()),
        };
        let concrete_type = Type::Function {
            args: vec![int_type()],
            return_type: Box::new(float_type()),
        };
        let result = lock_subtypes(generic_type, concrete_type).unwrap();
        let res_type = result.get(&ident("*A")).unwrap();
        assert_eq!(res_type, &int_type());
    }
    #[test]
    fn test_lock_subtypes_func_ret() {
        let generic_type = Type::Function {
            args: vec![],
            return_type: Box::new(Type::Hole(ident("*A"))),
        };
        let concrete_type = Type::Function {
            args: vec![],
            return_type: Box::new(float_type()),
        };
        let result = lock_subtypes(generic_type, concrete_type).unwrap();
        let res_type = result.get(&ident("*A")).unwrap();
        assert_eq!(res_type, &float_type());
    }
    #[test]
    fn test_lock_subtypes_func_args() {
        let generic_type = Type::Function {
            args: vec![Type::Hole(ident("*A")), Type::Hole(ident("*B"))],
            return_type: Box::new(float_type()),
        };
        let concrete_type = Type::Function {
            args: vec![int_type(), string_type()],
            return_type: Box::new(float_type()),
        };
        let result = lock_subtypes(generic_type, concrete_type).unwrap();
        let res_type_a = result.get(&ident("*A")).unwrap();
        assert_eq!(res_type_a, &int_type());
        let res_type_b = result.get(&ident("*B")).unwrap();
        assert_eq!(res_type_b, &string_type());
    }
    #[test]
    fn test_lock_subtypes_func_args_and_ret() {
        let generic_type = Type::Function {
            args: vec![Type::Hole(ident("*A")), Type::Hole(ident("*B"))],
            return_type: Box::new(Type::Hole(ident("*C"))),
        };
        let concrete_type = Type::Function {
            args: vec![int_type(), string_type()],
            return_type: Box::new(float_type()),
        };
        let result = lock_subtypes(generic_type, concrete_type).unwrap();
        let res_type_a = result.get(&ident("*A")).unwrap();
        assert_eq!(res_type_a, &int_type());
        let res_type_b = result.get(&ident("*B")).unwrap();
        assert_eq!(res_type_b, &string_type());
        let res_type_c = result.get(&ident("*C")).unwrap();
        assert_eq!(res_type_c, &float_type());
    }
    #[test]
    fn test_lock_subtypes_func_args_same_ok() {
        let generic_type = Type::Function {
            args: vec![Type::Hole(ident("*A")), Type::Hole(ident("*A"))],
            return_type: Box::new(float_type()),
        };
        let concrete_type = Type::Function {
            args: vec![int_type(), int_type()],
            return_type: Box::new(float_type()),
        };
        let result = lock_subtypes(generic_type, concrete_type).unwrap();
        let res_type_a = result.get(&ident("*A")).unwrap();
        assert_eq!(res_type_a, &int_type());
        assert!(result.get(&ident("*B")).is_none())
    }
    #[test]
    fn test_lock_subtypes_func_args_same_err() {
        let generic_type = Type::Function {
            args: vec![Type::Hole(ident("*B")), Type::Hole(ident("*B"))],
            return_type: Box::new(float_type()),
        };
        let concrete_type = Type::Function {
            args: vec![int_type(), string_type()],
            return_type: Box::new(float_type()),
        };
        let result = lock_subtypes(generic_type, concrete_type).unwrap_err();
        let exp_msg = "type mismatch for type '*B': 'Int' and 'String'";
        assert_eq!(&result, exp_msg);
    }

    #[test]
    fn test_typ_check_generic_function_a() {
        let def_args = vec![Type::Hole(ident("*A"))];
        let def_ret = Type::Hole(ident("*A"));
        let use_args = vec![int_type()];
        let res = typ_check_generic_function(def_args, def_ret, use_args).unwrap();
        assert_eq!(res, int_type());
    }

    #[test]
    fn test_typ_check_generic_function_b() {
        let def_args = vec![concrete_list_type(Type::Hole(ident("*B")))];
        let def_ret = Type::Hole(ident("*B"));
        let use_args = vec![concrete_list_type(float_type())];
        let res = typ_check_generic_function(def_args, def_ret, use_args).unwrap();
        assert_eq!(res, float_type());
    }

    #[test]
    fn test_typ_check_generic_function_c() {
        let map_f_def = Type::Function {
            args: vec![Type::Hole(ident("*A"))],
            return_type: Box::new(Type::Hole(ident("*B"))),
        };
        let def_args = vec![concrete_list_type(Type::Hole(ident("*A"))), map_f_def];
        let def_ret = concrete_list_type(Type::Hole(ident("*B")));
        let map_f_use = Type::Function {
            args: vec![int_type()],
            return_type: Box::new(float_type()),
        };
        let use_args = vec![concrete_list_type(int_type()), map_f_use];
        let res = typ_check_generic_function(def_args, def_ret, use_args).unwrap();
        assert_eq!(res, concrete_list_type(float_type()));
    }

    #[test]
    fn test_typ_check_generic_function_d() {
        let map_f_def = Type::Function {
            args: vec![Type::Hole(ident("*A"))],
            return_type: Box::new(Type::Hole(ident("*B"))),
        };
        let def_args = vec![concrete_list_type(Type::Hole(ident("*A"))), map_f_def];
        let def_ret = concrete_list_type(Type::Hole(ident("*B")));
        let map_f_use = Type::Function {
            args: vec![float_type()],
            return_type: Box::new(float_type()),
        };
        let use_args = vec![concrete_list_type(int_type()), map_f_use];
        let res = typ_check_generic_function(def_args, def_ret, use_args).unwrap_err();
        assert_eq!(&res, "type mismatch for type '*A': 'Int' and 'Float'");
    }
}
