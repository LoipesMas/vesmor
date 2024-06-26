#![allow(dead_code)]

use std::collections::HashMap;
use std::rc::Rc;

use crate::eval::ScopeMap;
use crate::typ_check::TypeName;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct Ident(pub String);

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: Ident,
    pub body: RExpr,
}
#[derive(Debug, Clone)]
pub struct Block {
    pub definitions: Vec<Definition>,
    pub expr: Rc<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntMod,
    IntLT,
    IntGT,
    IntEQ,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatLT,
    FloatGT,
    FloatEQ,
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
            "<" => Ok(IntLT),
            ">" => Ok(IntGT),
            "==" => Ok(IntEQ),
            "+." => Ok(FloatAdd),
            "-." => Ok(FloatSub),
            "*." => Ok(FloatMul),
            "/." => Ok(FloatDiv),
            "<." => Ok(FloatLT),
            ">." => Ok(FloatGT),
            "==." => Ok(FloatEQ),
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
    pub left: Rc<Expr>,
    pub operator: BinaryOperator,
    pub right: Rc<Expr>,
}

#[derive(Debug, Clone)]
pub struct ArgDef {
    pub name: Ident,
    pub typ: TypeName,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arguments: Vec<ArgDef>,
    pub return_type: TypeName,
    pub body: Rc<Expr>,
}

// `BuiltInFunction`s only get access to local_scope
type BuiltInFunctionClosure = dyn Fn(&ScopeMap) -> RExpr;

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
    pub function: Rc<Expr>,
    pub arguments: Vec<RExpr>,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub base: Option<Rc<Expr>>,
    pub update: HashMap<Ident, RExpr>,
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
        self.base.is_none() && self.update.values().all(|v| v.is_realized())
    }

    pub fn get(&self, ident: &Ident) -> Result<RExpr, RecordAccessError> {
        use RecordAccessError::*;
        if let Some(value) = self.update.get(ident) {
            if value.is_realized() {
                Ok(value.clone())
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
    pub record: Rc<Expr>,
    pub member: Ident,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub enu: Ident,
    pub variant: Ident,
    pub body: Option<Rc<Expr>>,
}

#[derive(Debug, Clone)]
pub enum EnumPattern {
    Variant { variant: Ident, bind: Option<Ident> },
    Any { bind: Ident },
}

#[derive(Debug, Clone)]
pub struct MatchBranch {
    pub pattern: EnumPattern,
    pub expr: Rc<Expr>,
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
    pub value: Rc<Expr>,
    pub branches: Vec<MatchBranch>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<RExpr>),
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

pub type RExpr = Rc<Expr>;

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
            List(exprs) => exprs.iter().all(|v| v.is_realized()),
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

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: Ident,
    pub body: TypeName,
}
