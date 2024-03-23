#![allow(dead_code)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while},
    character::complete::{alpha1, space0},
    combinator::{map, map_res},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, separated_pair, terminated, tuple},
    IResult,
};

use crate::ast::{
    ArgDef, BinaryOperation, BinaryOperator, Block, Definition, Expr, Function, Ident,
};

fn is_any(s: &'static str) -> impl Fn(char) -> bool {
    |e| s.contains([e])
}

fn parse_ident(input: &str) -> IResult<&str, Ident> {
    dbg!("ident", input);
    map(delimited(space0, alpha1, space0), |s: &str| {
        Ident(s.to_owned())
    })(input)
}

fn parse_arg_def(input: &str) -> IResult<&str, ArgDef> {
    let (input, (name, typ)) =
        separated_pair(parse_ident, tag(":"), take_till(is_any(",")))(input)?;
    Ok((
        input,
        ArgDef {
            name,
            typ: typ.trim().to_owned(),
        },
    ))
}

fn parse_definition(input: &str) -> IResult<&str, Definition> {
    let (input, _) = space0(input)?;
    dbg!("def", input);
    let (input, (name, body)) = separated_pair(parse_ident, tag("="), parse_expr)(input)?;
    dbg!("def after1", input);
    let (input, _) = tag(";")(input)?;
    dbg!("def after2", input);
    Ok((input, Definition { name, body }))
}

fn parse_block(input: &str) -> IResult<&str, Block> {
    dbg!("block", input);
    let (input, _) = space0(input)?;
    dbg!("block1", input);
    // let (input, (definitions, expr)) = delimited(
    //     tag("{"),
    //     pair(many0(parse_definition), parse_expr),
    //     tag("}"),
    // )(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, definitions) = many0(parse_definition)(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = tag("}")(input)?;
    Ok((
        input,
        Block {
            definitions,
            expr: Box::new(expr),
        },
    ))
}

fn parse_block_expr(input: &str) -> IResult<&str, Expr> {
    map(parse_block, Expr::Block)(input)
}

fn parse_int_expr(input: &str) -> IResult<&str, Expr> {
    map(
        map_res(
            delimited(space0, take_while(is_any("0123456789")), space0),
            |s: &str| s.parse::<i64>(),
        ),
        Expr::Int,
    )(input)
}

fn parse_value_expr(input: &str) -> IResult<&str, Expr> {
    map(parse_ident, Expr::Value)(input)
}

fn parse_binary_operator(input: &str) -> IResult<&str, BinaryOperator> {
    map_res(
        delimited(space0, alt((tag("+"), tag("~"), tag("<>"))), space0),
        BinaryOperator::from_str,
    )(input)
}

fn parse_binary_operation(input: &str) -> IResult<&str, Expr> {
    let (input, _) = space0(input)?;
    dbg!("binop", input);
    let (input, (left, operator, right)) = delimited(
        tag("("),
        tuple((parse_expr, parse_binary_operator, parse_expr)),
        tag(")"),
    )(input)?;
    let bo = BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    };
    dbg!("binop res", &bo);
    Ok((input, Expr::BinaryOperation(bo)))
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    dbg!("expr", input);
    alt((
        parse_block_expr,
        parse_binary_operation,
        parse_int_expr,
        parse_value_expr,
    ))(input)
}

pub fn parse_fun(input: &str) -> IResult<&str, Function> {
    let (input, _) = tag("fun")(input)?;
    let (input, _) = space0(input)?;
    let (input, name) = parse_ident(input)?;
    let (input, args) = delimited(tag("("), take_until(")"), tag(")"))(input)?;
    let (_, args) = separated_list0(tag(","), parse_arg_def)(args)?;
    let (input, _) = delimited(space0, tag("->"), space0)(input)?;
    let (input, return_type) = take_until("{")(input)?;
    let (input, body) = parse_expr(input)?;
    Ok((
        input,
        Function {
            name,
            body,
            return_type: return_type.trim().to_owned(),
            arguments: args,
        },
    ))
}
