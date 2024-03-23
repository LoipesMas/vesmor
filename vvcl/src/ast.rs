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
    StringConcat,
    ListConcat,
}

impl BinaryOperator {
    pub const fn valid_strs() -> &'static [&'static str] {
        &["+", "~", "<>"]
    }

    pub fn from_str(s: &str) -> Result<Self, ()> {
        use BinaryOperator::*;
        match s {
            "+" => Ok(IntAdd),
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
pub enum Expr {
    Int(i64),
    Float(f64),
    Block(Block),
    Value(Ident),
    BinaryOperation(BinaryOperation),
}

#[derive(Debug)]
pub struct ArgDef {
    pub name: Ident,
    pub typ: String,
}

#[derive(Debug)]
pub struct Function {
    pub name: Ident,
    pub arguments: Vec<ArgDef>,
    pub return_type: String,
    pub body: Expr,
}
