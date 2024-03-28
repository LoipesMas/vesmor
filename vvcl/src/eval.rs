use crate::ast::{
    BinaryOperation, BinaryOperator, Block, Definition, EnumMatching, EnumVariant, Expr, Function,
    Ident, Record, RecordAccess, RecordAccessError,
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
        Expr::Function(Function {
            name,
            arguments,
            return_type,
            body,
        }) => Expr::Function(Function {
            name: name.clone(),
            arguments: arguments.clone(),
            return_type: return_type.clone(),
            body: Box::new(brl(body)),
        }),
        Expr::FunctionCall(fc) => {
            if let Some(fun_expr) = combined_scope.get(&fc.name) {
                if let Expr::Function(fun) = fun_expr {
                    let reduced_args = Vec::from_iter(fc.arguments.iter().map(brl));
                    if reduced_args.is_empty() {
                        println!("not reducing FunctionCall, because no arguments passed!");
                        e.clone()
                    } else if !reduced_args.iter().all(Expr::is_realized) {
                        // println!("not reducing FunctionCall, because not all arguments are known!");
                        e.clone()
                    } else {
                        let mut inner_scope = HashMap::new();
                        for (i, arg_expr) in reduced_args.iter().enumerate() {
                            if let Some(target_arg) = fun.arguments.get(i) {
                                inner_scope.insert(target_arg.name.clone(), arg_expr.clone());
                            } else {
                                panic!(
                                    "Tried to call {} with more arguments that expected!",
                                    fc.name.0
                                )
                            }
                        }
                        br(&HashMap::new(), &br(&inner_scope, &fun.body))
                    }
                } else {
                    panic!("Tried to call non-function: {}", fc.name.0);
                }
            } else {
                // println!("missing function: {}", &fc.name.0);
                Expr::FunctionCall(fc.clone())
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
                    if let Some(branch) = em
                        .branches
                        .iter()
                        .find(|branch| branch.variant == ev.variant)
                    {
                        if let Some(bind_ident) = &branch.bind {
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
                                brl(&block)
                            } else {
                                // tried to bind, failed
                                panic!("Tried to bind enum body, but enum haven't got one. {ev:?}")
                            }
                        } else if ev.body.is_none() {
                            // nothing to bind, didn't try
                            brl(&branch.expr)
                        } else {
                            // tried not to bind, failed
                            panic!("Enum body was not bound {ev:?}")
                        }
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
