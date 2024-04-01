#![allow(dead_code)]

use std::collections::HashMap;

use crate::eval::ScopeMap;
use crate::typ_check::Type;

// TODO: try changing Box for Rc and test performance

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct Ident(pub String);

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: Ident,
    pub body: Expr,
}
#[derive(Debug, Clone)]
pub struct Block {
    pub definitions: Vec<Definition>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatLT,
    FloatGT,
    BoolOr,
    BoolAnd,
    StringConcat,
    ListConcat,
}

impl BinaryOperator {
    pub fn from_str(s: &str) -> Result<Self, ()> {
        use BinaryOperator::*;
        match s {
            "+" => Ok(IntAdd),
            "-" => Ok(IntSub),
            "*" => Ok(IntMul),
            "/" => Ok(IntDiv),
            "%" => Ok(IntMod),
            "+." => Ok(FloatAdd),
            "-." => Ok(FloatSub),
            "*." => Ok(FloatMul),
            "/." => Ok(FloatDiv),
            "<." => Ok(FloatLT),
            ">." => Ok(FloatGT),
            "~" => Ok(StringConcat),
            "~~" => Ok(ListConcat),
            "||" => Ok(BoolOr),
            "&&" => Ok(BoolAnd),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ArgDef {
    pub name: Ident,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arguments: Vec<ArgDef>,
    pub return_type: Type,
    pub body: Box<Expr>,
}

// `BuiltInFunction`s only get access to local_scope
type BuiltInFunctionClosure = dyn Fn(&ScopeMap) -> Expr;

#[derive(Clone)]
pub struct BuiltInFunction {
    pub body: &'static BuiltInFunctionClosure,
}

impl std::fmt::Debug for BuiltInFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltInFunction").finish()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub base: Option<Box<Expr>>,
    pub update: HashMap<Ident, Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum RecordAccessError {
    /// value is there, but not realized
    Unrealized,
    /// value is definitely not there
    Invalid,
    /// might or might not be there, can't know
    Unknown,
}

impl Record {
    pub fn is_realized(&self) -> bool {
        self.base.is_none() && self.update.values().all(Expr::is_realized)
    }

    pub fn get(&self, ident: &Ident) -> Result<&Expr, RecordAccessError> {
        use RecordAccessError::*;
        if let Some(value) = self.update.get(ident) {
            if value.is_realized() {
                Ok(value)
            } else {
                Err(Unrealized)
            }
        } else if let Some(base) = &self.base {
            if let Expr::Record(ref base) = **base {
                base.get(ident)
            } else if base.is_realized() {
                panic!("Base should be record but is {base:?}")
            } else {
                Err(Unknown)
            }
        } else {
            Err(Invalid)
        }
    }
}

#[derive(Debug, Clone)]
pub struct RecordAccess {
    pub record: Box<Expr>,
    pub member: Ident,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub enu: Ident,
    pub variant: Ident,
    pub body: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum EnumPattern {
    Variant { variant: Ident, bind: Option<Ident> },
    Any { bind: Ident },
}

#[derive(Debug, Clone)]
pub struct MatchBranch {
    pub pattern: EnumPattern,
    pub expr: Box<Expr>,
}

impl MatchBranch {
    pub fn matches(&self, enu: &EnumVariant) -> bool {
        match &self.pattern {
            EnumPattern::Variant { variant, bind: _ } => variant == &enu.variant,
            EnumPattern::Any { bind: _ } => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumMatching {
    pub value: Box<Expr>,
    pub branches: Vec<MatchBranch>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Expr>),
    Block(Block),
    Value(Ident),
    BinaryOperation(BinaryOperation),
    Function(Function),
    BuiltInFunction(BuiltInFunction),
    FunctionCall(FunctionCall),
    Record(Record),
    RecordAccess(RecordAccess),
    EnumVariant(EnumVariant),
    EnumMatching(EnumMatching),
}

impl Expr {
    /// Returns `true` if the `Expr` is fully known,
    /// that is: it doesn't have any references to other values
    /// and can't be reduced further.
    ///
    /// NOTE: assumes the `Expr` is already reduced!
    ///
    /// Int(5) => True
    /// Value("foo") => False
    pub fn is_realized(&self) -> bool {
        use Expr::*;
        match self {
            Expr::Record(r) => r.is_realized(),
            List(exprs) => exprs.iter().all(Expr::is_realized),
            Expr::EnumVariant(ev) => match &ev.body {
                Some(body) => body.is_realized(),
                None => true,
            },
            Int(_) | Float(_) | String(_) | Function(_) => true,
            // explicit so it reminds me when adding new variants
            Block(_) | Value(_) | BinaryOperation(_) | BuiltInFunction(_) | FunctionCall(_)
            | RecordAccess(_) | EnumMatching(_) => false,
        }
    }
}
