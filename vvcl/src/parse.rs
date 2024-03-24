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
    ArgDef, BinaryOperation, BinaryOperator, Block, Definition, Expr, Function, FunctionCall,
    Ident, Record, RecordAccess,
};
use crate::utils::map_from_defs;

type PResult<'a, O> = IResult<&'a str, O>;

fn is_any_of(s: &'static str) -> impl Fn(char) -> bool {
    |e| s.contains([e])
}

fn is_valid_ident_char(c: char) -> bool {
    (char::is_ascii(&c) && is_alphabetic(c as u8)) || is_any_of("_")(c)
}

fn ident(input: &str) -> PResult<Ident> {
    map(take_while1(is_valid_ident_char), |s: &str| {
        Ident(s.to_owned())
    })(input)
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
    let (input, (name, body)) = separated_pair(ident, tag("="), expr)(input)?;
    let (input, _) = tag(";")(input)?;
    Ok((input, Definition { name, body }))
}

fn block(input: &str) -> PResult<Block> {
    let (input, (definitions, expr)) =
        delimited(tag("{"), pair(many0(definition), expr), tag("}"))(input)?;
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
        map_res(take_while(is_any_of("0123456789")), |s: &str| {
            s.parse::<i64>()
        }),
        Expr::Int,
    )(input)
}

fn float_expr(input: &str) -> PResult<Expr> {
    map(
        map_res(
            separated_pair(
                take_while(is_any_of("0123456789")),
                tag("."),
                take_while(is_any_of("0123456789")),
            ),
            |(p1, p2): (&str, &str)| (p1.to_owned() + "." + p2).parse::<f64>(),
        ),
        Expr::Float,
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
        // HACK: order of those tags is important:
        // "+." has to be before "+", otherwise it would never be matched
        alt_tags!("+.", "-.", "*.", "/.", "+", "-", "*", "/", "~~", "~"),
        BinaryOperator::from_str,
    )(input)
}

fn binary_operation(input: &str) -> PResult<Expr> {
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

fn record(input: &str) -> PResult<Record> {
    let (input, definitions) = delimited(tag("<"), many0(definition), tag(">"))(input)?;
    Ok((input, Record(map_from_defs(definitions))))
}

fn record_expr(input: &str) -> PResult<Expr> {
    map(record, Expr::Record)(input)
}

fn record_access(input: &str) -> PResult<RecordAccess> {
    map(
        preceded(tag("@"), separated_pair(expr, tag("."), ident)),
        |(record, member)| RecordAccess {
            record: Box::new(record),
            member,
        },
    )(input)
}

fn record_access_expr(input: &str) -> PResult<Expr> {
    map(record_access, Expr::RecordAccess)(input)
}

fn expr(input: &str) -> PResult<Expr> {
    alt((
        record_access_expr,
        block_expr,
        record_expr,
        function_call_expr,
        binary_operation,
        string_literal_expr,
        float_expr,
        int_expr,
        value_expr,
    ))(input)
}

pub fn fun(input: &str) -> PResult<Function> {
    // TODO: this should be a standard `Definition` instead
    let (input, name) = ident(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, args) = delimited(tag("("), take_until(")"), tag(")"))(input)?;
    let (_, args) = separated_list0(tag(","), arg_def)(args)?;
    let (input, _) = tag("->")(input)?;
    let (input, return_type) = take_until("{")(input)?;
    let (input, body) = expr(input)?;
    let (input, _) = tag(";")(input)?;
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
    many0(fun)(input)
}
