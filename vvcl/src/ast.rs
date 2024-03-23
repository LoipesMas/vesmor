#![allow(dead_code)]

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
            "<>" => Ok(ListConcat),
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
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    Block(Block),
    Value(Ident),
    BinaryOperation(BinaryOperation),
    Function(Function),
    FunctionCall(FunctionCall),
}

impl Expr {
    pub fn is_realized(&self) -> bool {
        use Expr::*;
        matches!(self, Int(_) | Float(_) | String(_))
    }
}
