use crate::ast::{
    ArgDef, BinaryOperation, BinaryOperator, Block, Definition, EnumMatching, EnumPattern,
    EnumVariant, Expr, Function, FunctionCall, Ident, MatchBranch, Record, RecordAccess,
    RecordAccessError,
};
use crate::utils::{bool_to_enum, enum_to_bool, map_from_defs};
use std::collections::HashMap;

pub type ScopeMap = HashMap<Ident, Expr>;

trait Scope {
    fn get(&self, ident: &Ident) -> Option<&Expr>;
}

type TwoScopes<'a> = (&'a ScopeMap, &'a ScopeMap);

impl Scope for TwoScopes<'_> {
    fn get(&self, ident: &Ident) -> Option<&Expr> {
        self.1.get(ident).or_else(|| self.0.get(ident))
    }
}

// TODO: make this return `Result<Expr,_>` instead of panic!king
// TODO: maybe we could take `Expr` instead of `&Expr`
// so we wouldn't have to clone so much
// UPD: I think `&Expr` is faster than `Expr`
// maybe Cow would be better here?
pub fn beta_reduction(global_scope: &ScopeMap, local_scope: &ScopeMap, e: &Expr) -> Expr {
    let br = |s, e| beta_reduction(global_scope, s, e);
    let brl = |e| beta_reduction(global_scope, local_scope, e);
    let combined_scope = (global_scope, local_scope);
    match e {
        Expr::Block(b) => {
            let mut new_scope = local_scope.clone();
            let reduced_defs: Vec<Definition> = b
                .definitions
                .iter()
                .map(|d| Definition {
                    body: brl(&d.body),
                    name: d.name.clone(),
                })
                .collect();
            new_scope.extend(map_from_defs(reduced_defs.clone()));
            let new_expr = br(&new_scope, &b.expr);
            if new_expr.is_realized() {
                new_expr
            } else {
                // preserve scope for not-realized values
                Expr::Block(Block {
                    definitions: reduced_defs,
                    expr: Box::new(new_expr),
                })
            }
        }
        Expr::Value(v) => {
            if let Some(e) = combined_scope.get(v) {
                brl(e)
            } else {
                // println!("variable not in scope: {}", v.0);
                Expr::Value(v.clone())
            }
        }
        Expr::BinaryOperation(BinaryOperation {
            left,
            operator,
            right,
        }) => {
            let bo = BinaryOperation {
                left: Box::new(brl(left)),
                operator: *operator,
                right: Box::new(brl(right)),
            };
            apply_binary_operation(bo)
        }
        Expr::Function(_) => e.clone(),
        Expr::FunctionCall(fc) => {
            let function = if let Expr::Function(_) = *fc.function {
                *fc.function.clone()
            } else if fc.function.is_realized() {
                panic!("Tried to call non-function: {:?}", fc.function)
            } else if let Expr::Value(ref v) = *fc.function {
                if let Some(f) = combined_scope.get(v) {
                    brl(f)
                } else {
                    *fc.function.clone()
                }
            } else {
                brl(&fc.function)
            };
            if let Expr::Function(fun) = function {
                let reduced_args = Vec::from_iter(fc.arguments.iter().map(brl));
                if reduced_args.is_empty() {
                    println!("not reducing FunctionCall, because no arguments passed!");
                    e.clone()
                } else if !reduced_args.iter().all(Expr::is_realized) {
                    // println!("not reducing FunctionCall, because not all arguments are known!");
                    e.clone()
                } else {
                    let mut inner_scope = HashMap::new();
                    match reduced_args.len().cmp(&fun.arguments.len()) {
                        std::cmp::Ordering::Greater => {
                            panic!("Called function with too many arguments")
                        }
                        std::cmp::Ordering::Equal => {
                            for (i, arg_expr) in reduced_args.iter().enumerate() {
                                let target_arg = fun
                                    .arguments
                                    .get(i)
                                    .expect("Compared length of args already");
                                inner_scope.insert(target_arg.name.clone(), arg_expr.clone());
                            }
                            br(&HashMap::new(), &br(&inner_scope, &fun.body))
                        }
                        std::cmp::Ordering::Less => {
                            // partial application
                            let remaining_arguments = fun
                                .arguments
                                .iter()
                                .skip(reduced_args.len())
                                .cloned()
                                .collect::<Vec<_>>();
                            let inner_args_exprs = reduced_args
                                .into_iter()
                                .chain(
                                    remaining_arguments
                                        .iter()
                                        .map(|ad| Expr::Value(ad.name.clone())),
                                )
                                .collect::<Vec<_>>();
                            Expr::Function(Function {
                                arguments: remaining_arguments,
                                return_type: fun.return_type.clone(),
                                body: Box::new(Expr::FunctionCall(FunctionCall {
                                    function: Box::new(Expr::Function(fun)),
                                    arguments: inner_args_exprs,
                                })),
                            })
                        }
                    }
                }
            } else if function.is_realized() {
                panic!("Tried to call non-function: {:?}", function);
            } else {
                Expr::FunctionCall(FunctionCall {
                    function: Box::new(function),
                    arguments: fc.arguments.clone(),
                })
            }
        }
        Expr::BuiltInFunction(bif) => (bif.body)(local_scope),
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) => e.clone(),
        Expr::Record(rec) => {
            if e.is_realized() {
                e.clone()
            } else {
                let mut rec = rec.clone();
                for expr in rec.update.values_mut() {
                    let reduced = beta_reduction(global_scope, local_scope, expr);
                    *expr = reduced;
                }
                if let Some(ref from) = rec.base {
                    let from = brl(from);
                    if from.is_realized() {
                        if let Expr::Record(mut from) = from {
                            debug_assert!(from.base.is_none(), "Should be realized!");
                            from.update.extend(rec.update);
                            Expr::Record(Record {
                                base: None,
                                update: from.update,
                            })
                        } else {
                            panic!("Expected base record to be Record, got {:?}", from)
                        }
                    } else {
                        rec.base = Some(Box::new(from));
                        Expr::Record(rec)
                    }
                } else {
                    Expr::Record(rec)
                }
            }
        }
        Expr::RecordAccess(ra) => {
            let record = brl(&ra.record);
            if let Expr::Record(rec) = record {
                match rec.get(&ra.member) {
                    Ok(v) => v.clone(),
                    Err(e) => match e {
                        RecordAccessError::Invalid => {
                            panic!("Invalid member {:?} for record {:?}", ra.member, rec)
                        }
                        RecordAccessError::Unrealized => Expr::RecordAccess(ra.clone()),
                        RecordAccessError::Unknown => todo!("How to get HERE?"),
                    },
                }
            } else if record.is_realized() {
                panic!("Expected Record, got {:?}", record)
            } else {
                Expr::RecordAccess(RecordAccess {
                    record: Box::new(record),
                    member: ra.member.clone(),
                })
            }
        }
        Expr::List(exprs) => Expr::List(exprs.iter().map(brl).collect()),
        Expr::EnumVariant(EnumVariant { variant, enu, body }) => Expr::EnumVariant(EnumVariant {
            enu: enu.clone(),
            variant: variant.clone(),
            body: body.as_deref().map(brl).map(Box::new),
        }),
        Expr::EnumMatching(em) => {
            let value = brl(&em.value);
            if value.is_realized() {
                if let Expr::EnumVariant(ev) = value {
                    if let Some(branch) = em.branches.iter().find(|branch| branch.matches(&ev)) {
                        eval_enum_match_branch(global_scope, local_scope, ev, branch)
                    } else {
                        panic!("None of the branches matched {ev:?}")
                    }
                } else {
                    panic!("Expected EnumVariant, got {value:?}")
                }
            } else {
                Expr::EnumMatching(EnumMatching {
                    value: Box::new(value),
                    branches: em.branches.clone(),
                })
            }
        }
    }
}

fn eval_enum_match_branch(
    global_scope: &ScopeMap,
    local_scope: &ScopeMap,
    ev: EnumVariant,
    branch: &MatchBranch,
) -> Expr {
    match &branch.pattern {
        EnumPattern::Any { bind } => {
            let ev_expr = Expr::EnumVariant(ev.clone());
            let definition = Definition {
                name: bind.clone(),
                body: ev_expr,
            };
            let block = Expr::Block(Block {
                definitions: vec![definition],
                expr: branch.expr.clone(),
            });
            beta_reduction(global_scope, local_scope, &block)
        }
        EnumPattern::Variant { variant: _, bind } => {
            if let Some(bind_ident) = &bind {
                if let Some(ev_body) = ev.body {
                    // bind enum body
                    let definition = Definition {
                        name: bind_ident.clone(),
                        body: *ev_body,
                    };
                    let block = Expr::Block(Block {
                        definitions: vec![definition],
                        expr: branch.expr.clone(),
                    });
                    beta_reduction(global_scope, local_scope, &block)
                } else {
                    // tried to bind, failed
                    panic!("Tried to bind enum body, but enum haven't got one. {ev:?}")
                }
            } else if ev.body.is_none() {
                // nothing to bind, didn't try
                beta_reduction(global_scope, local_scope, &branch.expr)
            } else {
                // tried not to bind, failed
                panic!("Enum body was not bound {ev:?}")
            }
        }
    }
}

fn apply_binary_operation(
    BinaryOperation {
        left,
        operator,
        right,
    }: BinaryOperation,
) -> Expr {
    use BinaryOperator::*;
    use Expr::*;
    match (operator, &*left, &*right) {
        (IntAdd, &Int(a), &Int(b)) => Int(a + b),
        (IntSub, &Int(a), &Int(b)) => Int(a - b),
        (IntMul, &Int(a), &Int(b)) => Int(a * b),
        (IntDiv, &Int(a), &Int(b)) => Int(a / b),
        (IntMod, &Int(a), &Int(b)) => Int(a % b),
        (FloatAdd, &Float(a), &Float(b)) => Float(a + b),
        (FloatSub, &Float(a), &Float(b)) => Float(a - b),
        (FloatMul, &Float(a), &Float(b)) => Float(a * b),
        (FloatDiv, &Float(a), &Float(b)) => Float(a / b),
        (FloatLT, &Float(a), &Float(b)) => bool_to_enum(a < b),
        (FloatGT, &Float(a), &Float(b)) => bool_to_enum(a > b),
        (StringConcat, String(ref a), String(ref b)) => String(a.to_owned() + &b),
        (ListConcat, List(ref a), List(ref b)) => List(a.iter().chain(b).cloned().collect()),
        (BoolOr, EnumVariant(a), EnumVariant(b)) => {
            bool_to_enum(enum_to_bool(a) || enum_to_bool(b))
        }
        (BoolAnd, EnumVariant(a), EnumVariant(b)) => {
            bool_to_enum(enum_to_bool(a) && enum_to_bool(b))
        }
        (o, a, b) if a.is_realized() && b.is_realized() => {
            panic!("Invalid use of operator: {:?} {:?} {:?}", a, o, b)
        }
        _ => BinaryOperation(crate::ast::BinaryOperation {
            left,
            operator,
            right,
        }),
    }
}
