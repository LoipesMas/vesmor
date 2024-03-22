#![allow(dead_code)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while},
    character::{
        complete::{alpha1, anychar, space0},
        is_digit,
    },
    combinator::{map, map_res},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair},
    IResult,
};

#[derive(Debug)]
pub struct Ident(String);

#[derive(Debug)]
pub struct Definition {
    name: String,
    body: Expr,
}
#[derive(Debug)]
pub struct Block {
    definitions: Vec<Definition>,
    expr: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Block(Block),
    Value(Ident),
}

#[derive(Debug)]
struct ArgDef {
    name: String,
    typ: String,
}

#[derive(Debug)]
pub struct Function {
    name: String,
    arguments: Vec<ArgDef>,
    return_type: String,
    body: Expr,
}

fn is_any(s: &'static str) -> impl Fn(char) -> bool {
    |e| s.contains([e])
}

fn parse_ident(input: &str) -> IResult<&str, Ident> {
    map(delimited(space0, alpha1, space0), |s: &str| {
        Ident(s.to_owned())
    })(input)
}

fn parse_arg_def(input: &str) -> IResult<&str, ArgDef> {
    let (input, (name, typ)) =
        separated_pair(take_till(is_any(":")), tag(":"), take_till(is_any(",")))(input)?;
    Ok((
        input,
        ArgDef {
            name: name.trim().to_owned(),
            typ: typ.trim().to_owned(),
        },
    ))
}

fn parse_definition(input: &str) -> IResult<&str, Definition> {
    dbg!(input);
    let (input, (name, body)) = delimited(
        space0,
        separated_pair(take_until("="), tag("="), take_until(";")),
        tag(";"),
    )(input)?;
    dbg!((input, name, &body));
    let (_, body) = parse_expr(body)?;
    dbg!((input, name, &body));
    Ok((
        input,
        Definition {
            name: name.to_owned(),
            body,
        },
    ))
}

fn parse_block(input: &str) -> IResult<&str, Block> {
    let (input, _) = space0(input)?;
    dbg!(input);
    let (input, (definitions, expr)) = delimited(
        tag("{"),
        pair(many0(parse_definition), parse_expr),
        tag("}"),
    )(input)?;
    dbg!(input);
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
    dbg!(input);
    map(parse_ident, Expr::Value)(input)
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    dbg!(input);
    alt((parse_block_expr, parse_int_expr, parse_value_expr))(input)
}

pub fn parse_fun(input: &str) -> IResult<&str, Function> {
    let (input, _) = tag("fun")(input)?;
    let (input, _) = space0(input)?;
    let (input, name) = take_until("(")(input)?;
    let (input, args) = delimited(tag("("), take_until(")"), tag(")"))(input)?;
    let (_, args) = separated_list0(tag(","), parse_arg_def)(args)?;
    let (input, _) = delimited(space0, tag("->"), space0)(input)?;
    let (input, return_type) = take_until("{")(input)?;
    let (input, body) = parse_expr(input)?;
    Ok((
        input,
        Function {
            name: name.to_owned(),
            body,
            return_type: return_type.trim().to_owned(),
            arguments: args,
        },
    ))
}
