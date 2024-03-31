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
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};

use crate::ast::{
    ArgDef, BinaryOperation, BinaryOperator, Block, Definition, EnumMatching, EnumPattern,
    EnumVariant, Expr, Function, FunctionCall, Ident, MatchBranch, Record, RecordAccess,
};
use crate::utils::map_from_defs;

type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

type PResult<'a, O> = IResult<Span<'a>, O>;

fn is_any_of(s: &'static str) -> impl Fn(char) -> bool {
    |e| s.contains([e])
}

fn is_valid_ident_char(c: char) -> bool {
    (char::is_ascii(&c) && is_alphabetic(c as u8)) || is_any_of("_")(c)
}

fn ident(input: Span) -> PResult<Ident> {
    map(
        recognize(pair(
            alt((nom::character::complete::alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |s: Span| Ident(s.to_string()),
    )(input)
}

fn arg_def(input: Span) -> PResult<ArgDef> {
    let (input, (name, typ)) = separated_pair(ident, tag(":"), type_name)(input)?;
    Ok((
        input,
        ArgDef {
            name,
            typ: typ.trim().to_owned(),
        },
    ))
}

fn definition(input: Span) -> PResult<Definition> {
    let (input, (name, body)) = separated_pair(ident, tag("="), expr)(input)?;
    let (input, _) = tag(";")(input)?;
    Ok((input, Definition { name, body }))
}

fn block(input: Span) -> PResult<Block> {
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

fn block_expr(input: Span) -> PResult<Expr> {
    map(block, Expr::Block)(input)
}

fn int_expr(input: Span) -> PResult<Expr> {
    map(
        map_res(take_while(is_any_of("0123456789")), |s: Span| {
            s.to_string().parse::<i64>()
        }),
        Expr::Int,
    )(input)
}

fn float_expr(input: Span) -> PResult<Expr> {
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
            |s: Span| s.to_string().parse::<f64>(),
        ),
        Expr::Float,
    )(input)
}

fn value_expr(input: Span) -> PResult<Expr> {
    map(ident, Expr::Value)(input)
}

macro_rules! alt_tags {
    ($($tag:expr),*) => {
        alt(( $(tag($tag)),* ))
    };
}

fn binary_operator(input: Span) -> PResult<BinaryOperator> {
    map_res(
        // HACK: order of those tags is important:
        // "+." has to be before "+", otherwise it would never be matched
        alt_tags!(
            "+.", "-.", "*.", "/.", "+", "-", "*", "/", "~~", "~", "<.", ">.", "||", "&&", "%"
        ),
        |c: Span| BinaryOperator::from_str(&c),
    )(input)
}

fn binary_operation(input: Span) -> PResult<Expr> {
    let (input, (left, operator, right)) =
        delimited(tag("("), tuple((expr, binary_operator, expr)), tag(")"))(input)?;
    let bo = BinaryOperation {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    };
    Ok((input, Expr::BinaryOperation(bo)))
}

fn string_literal(input: Span) -> PResult<String> {
    map(
        delimited(tag("\""), take_until("\""), tag("\"")),
        |c: Span| c.to_string(),
    )(input)
}

fn string_literal_expr(input: Span) -> PResult<Expr> {
    map(string_literal, Expr::String)(input)
}

#[recursive_parser]
fn function_call(input: Span) -> PResult<FunctionCall> {
    map(
        pair(
            map(expr, Box::new),
            delimited(tag("("), separated_list0(tag(","), expr), tag(")")),
        ),
        |(function, arguments)| FunctionCall {
            function,
            arguments,
        },
    )(input)
}

fn function_call_expr(input: Span) -> PResult<Expr> {
    map(function_call, Expr::FunctionCall)(input)
}

fn record(input: Span) -> PResult<Record> {
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

fn record_expr(input: Span) -> PResult<Expr> {
    map(record, Expr::Record)(input)
}

fn record_access(input: Span) -> PResult<RecordAccess> {
    map(
        preceded(tag("@"), separated_pair(expr, tag("."), ident)),
        |(record, member)| RecordAccess {
            record: Box::new(record),
            member,
        },
    )(input)
}

fn record_access_expr(input: Span) -> PResult<Expr> {
    map(record_access, Expr::RecordAccess)(input)
}

fn list_expr(input: Span) -> PResult<Expr> {
    map(
        delimited(
            tag("["),
            terminated(separated_list0(tag(","), expr), opt(tag(","))),
            tag("]"),
        ),
        Expr::List,
    )(input)
}

fn enum_variant_constructor(input: Span) -> PResult<Expr> {
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

fn enum_pattern(input: Span) -> PResult<EnumPattern> {
    alt((
        map(
            separated_pair(ident, tag("`"), opt(ident)),
            |(variant, bind)| EnumPattern::Variant { variant, bind },
        ),
        map(ident, |bind| EnumPattern::Any { bind }),
    ))(input)
}

fn enum_matching_branch(input: Span) -> PResult<MatchBranch> {
    map(
        delimited(
            tag("|"),
            separated_pair(enum_pattern, tag("=>"), expr),
            tag(";"),
        ),
        |(pattern, expr)| MatchBranch {
            pattern,
            expr: Box::new(expr),
        },
    )(input)
}

fn enum_matching_expr(input: Span) -> PResult<Expr> {
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

fn type_name(input: Span) -> PResult<String> {
    map(
        recognize(pair(ident, opt(delimited(tag("<"), type_name, tag(">"))))),
        |s: Span| s.to_string(),
    )(input)
}

fn fun(input: Span) -> PResult<Function> {
    map(
        pair(
            separated_pair(
                delimited(tag("("), separated_list0(tag(","), arg_def), tag(")")),
                tag("->"),
                type_name,
            ),
            map(expr, Box::new),
        ),
        |((arguments, return_type), body)| Function {
            arguments,
            body,
            return_type,
        },
    )(input)
}

fn fun_expr(input: Span) -> PResult<Expr> {
    map(fun, Expr::Function)(input)
}

pub fn expr(input: Span) -> PResult<Expr> {
    alt((
        fun_expr,
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

pub fn top_definitions(input: Span) -> PResult<Vec<Definition>> {
    many0(definition)(input)
}
