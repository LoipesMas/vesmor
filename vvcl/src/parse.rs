#![allow(dead_code)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while},
    character::{complete::alphanumeric1, is_alphabetic},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many0_count, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::ast::{
    ArgDef, BinaryOperation, BinaryOperator, Block, Definition, EnumMatching, EnumVariant, Expr,
    Function, FunctionCall, Ident, MatchBranch, Record, RecordAccess,
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
    map(
        recognize(pair(
            alt((nom::character::complete::alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
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
            recognize(pair(
                opt(alt((tag("+"), tag("-")))),
                separated_pair(
                    take_while(is_any_of("0123456789")),
                    tag("."),
                    take_while(is_any_of("0123456789")),
                ),
            )),
            |s: &str| s.parse::<f64>(),
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
        alt_tags!("+.", "-.", "*.", "/.", "+", "-", "*", "/", "~~", "~", "<.", ">.", "||", "&&"),
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
    let (input, (from, definitions)) = delimited(
        tag("<"),
        pair(opt(terminated(expr, tag("|"))), many0(definition)),
        tag(">"),
    )(input)?;
    Ok((
        input,
        Record {
            base: from.map(Box::new),
            update: map_from_defs(definitions),
        },
    ))
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

fn list_expr(input: &str) -> PResult<Expr> {
    map(
        delimited(
            tag("["),
            terminated(separated_list0(tag(","), expr), opt(tag(","))),
            tag("]"),
        ),
        Expr::List,
    )(input)
}

fn enum_variant_constructor(input: &str) -> PResult<Expr> {
    map(
        separated_pair(separated_pair(ident, tag("::"), ident), tag("`"), opt(expr)),
        |((enu, variant), body)| {
            Expr::EnumVariant(EnumVariant {
                enu,
                variant,
                body: body.map(Box::new),
            })
        },
    )(input)
}

fn enum_matching_branch(input: &str) -> PResult<MatchBranch> {
    map(
        preceded(
            tag("|"),
            separated_pair(separated_pair(ident, tag("`"), opt(ident)), tag("=>"), expr),
        ),
        |((variant, bind), expr)| MatchBranch {
            variant,
            bind,
            expr: Box::new(expr),
        },
    )(input)
}

fn enum_matching_expr(input: &str) -> PResult<Expr> {
    map(
        preceded(tag("?"), pair(expr, many1(enum_matching_branch))),
        |(value, branches)| {
            Expr::EnumMatching(EnumMatching {
                value: Box::new(value),
                branches,
            })
        },
    )(input)
}

fn expr(input: &str) -> PResult<Expr> {
    alt((
        enum_variant_constructor,
        enum_matching_expr,
        list_expr,
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
