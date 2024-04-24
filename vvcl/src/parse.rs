#![allow(dead_code)]

use std::rc::Rc;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::{complete::alphanumeric1, is_alphabetic},
    combinator::{all_consuming, map, map_res, opt, recognize},
    multi::{many0, many0_count, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};

use crate::{
    ast::{
        ArgDef, BinaryOperation, BinaryOperator, Block, Definition, EnumMatching, EnumPattern,
        EnumVariant, Expr, Function, FunctionCall, Ident, MatchBranch, RExpr, Record, RecordAccess,
        TypeDef,
    },
    typ_check::{EnumTypeName, FunctionTypeName, RecordTypeName, TypeName},
};
use crate::{typ_check::NormalTypeName, utils::map_from_defs};

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

fn normal_type_name(input: Span) -> PResult<NormalTypeName> {
    map(
        pair(ident, opt(delimited(tag("<"), normal_type_name, tag(">")))),
        |(name, subtype)| NormalTypeName {
            name,
            subtype: subtype.map(Box::new),
        },
    )(input)
}

fn function_type_name(input: Span) -> PResult<FunctionTypeName> {
    map(
        preceded(
            tag("Fun"),
            delimited(
                tag("<"),
                separated_pair(
                    delimited(
                        tag("("),
                        separated_list0(tag(","), normal_type_name),
                        tag(")"),
                    ),
                    tag("->"),
                    normal_type_name,
                ),
                tag(">"),
            ),
        ),
        |(args, return_type)| FunctionTypeName { args, return_type },
    )(input)
}

fn type_name(input: Span) -> PResult<TypeName> {
    alt((
        map(function_type_name, TypeName::Function),
        map(normal_type_name, TypeName::Normal),
    ))(input)
}

fn arg_def(input: Span) -> PResult<ArgDef> {
    let (input, (name, typ)) = separated_pair(ident, tag(":"), type_name)(input)?;
    Ok((input, ArgDef { name, typ }))
}

fn definition(input: Span) -> PResult<Definition> {
    let (input, (name, body)) = separated_pair(ident, tag("="), rexpr)(input)?;
    let (input, _) = tag(";")(input)?;
    Ok((input, Definition { name, body }))
}

fn block(input: Span) -> PResult<Block> {
    let (input, (definitions, expr)) =
        delimited(tag("{"), pair(many0(definition), rexpr), tag("}"))(input)?;
    Ok((input, Block { definitions, expr }))
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
    // TODO: group other ops as well
    let other_ops = alt_tags!("+.", "-.", "*.", "/.", "~~", "~", "<.", ">.", "==.", "||", "&&");
    let int_ops = alt_tags!("<", ">", "==", "+", "-", "/", "*", "%");
    map_res(
        // HACK: order of those tags is important:
        // "+." has to be before "+", otherwise it would never be matched
        alt((other_ops, int_ops)),
        |c: Span| BinaryOperator::from_str(&c),
    )(input)
}

fn binary_operation(input: Span) -> PResult<Expr> {
    let (input, (left, operator, right)) =
        delimited(tag("("), tuple((rexpr, binary_operator, rexpr)), tag(")"))(input)?;
    let bo = BinaryOperation {
        left,
        operator,
        right,
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
            rexpr,
            delimited(tag("("), separated_list0(tag(","), rexpr), tag(")")),
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
        pair(opt(terminated(rexpr, tag("|"))), many0(definition)),
        tag(">"),
    )(input)?;
    Ok((
        input,
        Record {
            base: from,
            update: map_from_defs(definitions),
        },
    ))
}

fn record_expr(input: Span) -> PResult<Expr> {
    map(record, Expr::Record)(input)
}

fn record_access(input: Span) -> PResult<RecordAccess> {
    map(
        preceded(tag("@"), separated_pair(rexpr, tag("."), ident)),
        |(record, member)| RecordAccess { record, member },
    )(input)
}

fn record_access_expr(input: Span) -> PResult<Expr> {
    map(record_access, Expr::RecordAccess)(input)
}

fn list_expr(input: Span) -> PResult<Expr> {
    map(
        delimited(
            tag("["),
            terminated(separated_list0(tag(","), rexpr), opt(tag(","))),
            tag("]"),
        ),
        Expr::List,
    )(input)
}

fn enum_variant_constructor(input: Span) -> PResult<Expr> {
    map(
        separated_pair(
            separated_pair(ident, tag("::"), ident),
            tag("`"),
            opt(rexpr),
        ),
        |((enu, variant), body)| Expr::EnumVariant(EnumVariant { enu, variant, body }),
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
            separated_pair(enum_pattern, tag("=>"), rexpr),
            tag(";"),
        ),
        |(pattern, expr)| MatchBranch { pattern, expr },
    )(input)
}

fn enum_matching_expr(input: Span) -> PResult<Expr> {
    map(
        preceded(tag("?"), pair(rexpr, many1(enum_matching_branch))),
        |(value, branches)| Expr::EnumMatching(EnumMatching { value, branches }),
    )(input)
}

fn fun(input: Span) -> PResult<Function> {
    map(
        pair(
            separated_pair(
                delimited(tag("("), separated_list0(tag(","), arg_def), tag(")")),
                tag("->"),
                normal_type_name,
            ),
            rexpr,
        ),
        |((arguments, return_type), body)| Function {
            arguments,
            body,
            return_type: TypeName::Normal(return_type),
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

pub fn rexpr(input: Span) -> PResult<RExpr> {
    map(expr, Rc::new)(input)
}

fn record_definition(input: Span) -> PResult<RecordTypeName> {
    map(
        delimited(
            tag("<"),
            terminated(separated_list1(tag(","), arg_def), opt(tag(","))),
            tag(">"),
        ),
        |members| RecordTypeName {
            members: members.into_iter().map(|a| (a.name, a.typ)).collect(),
        },
    )(input)
}

fn enum_variant_definition(input: Span) -> PResult<(Ident, Option<TypeName>)> {
    preceded(tag("|"), pair(terminated(ident, tag("`")), opt(type_name)))(input)
}

// enum definition requires it's name
// that's why we have to parse it together
// TODO: fix that ^
fn enum_definition(input: Span) -> PResult<TypeDef> {
    map(
        terminated(
            separated_pair(ident, tag(":"), many1(enum_variant_definition)),
            tag(";"),
        ),
        |(name, variants)| TypeDef {
            body: TypeName::Enum(EnumTypeName {
                enu: name.clone(),
                variants: variants.into_iter().collect(),
            }),
            name,
        },
    )(input)
}

fn type_definition_body(input: Span) -> PResult<TypeName> {
    alt((map(record_definition, TypeName::Record),))(input)
}

fn type_definition(input: Span) -> PResult<TypeDef> {
    map(
        terminated(
            separated_pair(ident, tag(":"), type_definition_body),
            tag(";"),
        ),
        |(name, body)| TypeDef { name, body },
    )(input)
}

pub enum TopLevelDefinition {
    Type(TypeDef),
    Expr(Definition),
}

pub fn top_definitions(input: Span) -> PResult<Vec<TopLevelDefinition>> {
    all_consuming(many0(alt((
        map(definition, TopLevelDefinition::Expr),
        map(type_definition, TopLevelDefinition::Type),
        map(enum_definition, TopLevelDefinition::Type),
    ))))(input)
}
