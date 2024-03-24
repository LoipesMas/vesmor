#![allow(dead_code)]

use std::collections::HashMap;

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
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
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
            "+." => Ok(FloatAdd),
            "-." => Ok(FloatSub),
            "*." => Ok(FloatMul),
            "/." => Ok(FloatDiv),
            "~" => Ok(StringConcat),
            "~~" => Ok(ListConcat),
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
    pub typ: String,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub arguments: Vec<ArgDef>,
    pub return_type: String,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: Ident,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Record(pub HashMap<Ident, Expr>);

#[derive(Debug, Clone)]
pub struct RecordAccess {
    pub record: Box<Expr>,
    pub member: Ident,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    Block(Block),
    Value(Ident),
    BinaryOperation(BinaryOperation),
    Function(Function),
    FunctionCall(FunctionCall),
    Record(Record),
    RecordAccess(RecordAccess),
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
            Expr::Record(r) => r.0.values().all(Expr::is_realized),
            Int(_) | Float(_) | String(_) => true,
            _ => false,
        }
    }
}
