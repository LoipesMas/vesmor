#![allow(dead_code)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while, take_while1},
    character::{complete::space0, is_alphabetic},
    combinator::{map, map_res},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};

use crate::ast::{
    ArgDef, BinaryOperation, BinaryOperator, Block, Definition, Expr, Function, FunctionCall, Ident,
};

type PResult<'a, O> = IResult<&'a str, O>;

fn is_any_of(s: &'static str) -> impl Fn(char) -> bool {
    |e| s.contains([e])
}

fn is_valid_ident_char(c: char) -> bool {
    (char::is_ascii(&c) && is_alphabetic(c as u8)) || is_any_of("_")(c)
}

fn ident(input: &str) -> PResult<Ident> {
    map(
        delimited(space0, take_while1(is_valid_ident_char), space0),
        |s: &str| Ident(s.to_owned()),
    )(input)
}

fn arg_def(input: &str) -> PResult<ArgDef> {
    let (input, (name, typ)) = separated_pair(ident, tag(":"), take_till(is_any_of(",")))(input)?;
    Ok((
        input,
        ArgDef {
            name,
            typ: typ.trim().to_owned(),
        },
    ))
}

fn definition(input: &str) -> PResult<Definition> {
    let (input, _) = space0(input)?;
    let (input, (name, body)) = separated_pair(ident, tag("="), expr)(input)?;
    let (input, _) = tag(";")(input)?;
    Ok((input, Definition { name, body }))
}

fn block(input: &str) -> PResult<Block> {
    let (input, _) = space0(input)?;
    let (input, (definitions, expr)) = delimited(
        tag("{"),
        pair(many0(definition), expr),
        preceded(space0, tag("}")),
    )(input)?;
    Ok((
        input,
        Block {
            definitions,
            expr: Box::new(expr),
        },
    ))
}

fn block_expr(input: &str) -> PResult<Expr> {
    map(block, Expr::Block)(input)
}

fn int_expr(input: &str) -> PResult<Expr> {
    map(
        map_res(
            delimited(space0, take_while(is_any_of("0123456789")), space0),
            |s: &str| s.parse::<i64>(),
        ),
        Expr::Int,
    )(input)
}

fn value_expr(input: &str) -> PResult<Expr> {
    map(ident, Expr::Value)(input)
}

macro_rules! alt_tags {
    ($($tag:expr),*) => {
        alt(( $(tag($tag)),* ))
    };
}

fn binary_operator(input: &str) -> PResult<BinaryOperator> {
    map_res(
        delimited(
            space0,
            alt_tags!("+", "-", "*", "/", "+.", "-.", "*.", "/.", "~", "<>"),
            space0,
        ),
        BinaryOperator::from_str,
    )(input)
}

fn binary_operation(input: &str) -> PResult<Expr> {
    let (input, _) = space0(input)?;
    let (input, (left, operator, right)) =
        delimited(tag("("), tuple((expr, binary_operator, expr)), tag(")"))(input)?;
    let bo = BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    };
    Ok((input, Expr::BinaryOperation(bo)))
}

fn string_literal(input: &str) -> PResult<String> {
    let (input, _) = space0(input)?;
    map(
        delimited(tag("\""), take_until("\""), tag("\"")),
        str::to_owned,
    )(input)
}

fn string_literal_expr(input: &str) -> PResult<Expr> {
    map(string_literal, Expr::String)(input)
}

fn function_call(input: &str) -> PResult<FunctionCall> {
    map(
        pair(
            ident,
            delimited(tag("("), separated_list0(tag(","), expr), tag(")")),
        ),
        |(name, arguments)| FunctionCall { name, arguments },
    )(input)
}

fn function_call_expr(input: &str) -> PResult<Expr> {
    map(function_call, Expr::FunctionCall)(input)
}

fn expr(input: &str) -> PResult<Expr> {
    alt((
        block_expr,
        function_call_expr,
        binary_operation,
        string_literal_expr,
        int_expr,
        value_expr,
    ))(input)
}

pub fn fun(input: &str) -> PResult<Function> {
    let (input, _) = space0(input)?;
    let (input, name) = ident(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = space0(input)?;
    let (input, args) = delimited(tag("("), take_until(")"), tag(")"))(input)?;
    let (_, args) = separated_list0(tag(","), arg_def)(args)?;
    let (input, _) = delimited(space0, tag("->"), space0)(input)?;
    let (input, return_type) = take_until("{")(input)?;
    let (input, body) = expr(input)?;
    Ok((
        input,
        Function {
            name,
            body: Box::new(body),
            return_type: return_type.trim().to_owned(),
            arguments: args,
        },
    ))
}

pub fn all_funs(input: &str) -> PResult<Vec<Function>> {
    many0(delimited(space0, fun, space0))(input)
}
