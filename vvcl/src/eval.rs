use crate::ast::{
    BinaryOperation, BinaryOperator, Block, Definition, Expr, Function, Ident, RecordAccess,
};
use crate::utils::map_from_defs;
use std::collections::HashMap;

type ScopeMap = HashMap<Ident, Expr>;

trait Scope {
    fn get(&self, ident: &Ident) -> Option<&Expr>;
}

type TwoScopes<'a> = (&'a ScopeMap, &'a ScopeMap);

impl Scope for TwoScopes<'_> {
    fn get(&self, ident: &Ident) -> Option<&Expr> {
        self.1.get(ident).or_else(|| self.0.get(ident))
    }
}

// TODO: maybe we could take `Expr` instead of `&Expr`
// so we wouldn't have to clone so much
pub fn beta_reduction(global_scope: &ScopeMap, local_scope: &ScopeMap, e: &Expr) -> Expr {
    let br = |s, e| beta_reduction(global_scope, s, e);
    let combined_scope = (global_scope, local_scope);
    match e {
        Expr::Block(b) => {
            let mut new_scope = local_scope.clone();
            let reduced_defs: Vec<Definition> = b
                .definitions
                .iter()
                .map(|d| Definition {
                    body: br(local_scope, &d.body),
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
                br(local_scope, e)
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
                left: Box::new(br(local_scope, left)),
                operator: *operator,
                right: Box::new(br(local_scope, right)),
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
            body: Box::new(br(local_scope, body)),
        }),
        Expr::FunctionCall(fc) => {
            if let Some(fun_expr) = combined_scope.get(&fc.name) {
                if let Expr::Function(fun) = fun_expr {
                    let reduced_args =
                        Vec::from_iter(fc.arguments.iter().map(|a| br(local_scope, a)));
                    if !reduced_args.iter().all(Expr::is_realized) {
                        println!("not reducing FunctionCall, because not all arguments are known!");
                        return e.clone();
                    }
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
                    br(&inner_scope, &fun.body)
                } else {
                    panic!("Tried to call non-function: {}", fc.name.0);
                }
            } else {
                println!("missing function: {}", &fc.name.0);
                Expr::FunctionCall(fc.clone())
            }
        }
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) => e.clone(),
        Expr::Record(rec) => {
            if e.is_realized() {
                e.clone()
            } else {
                let mut rec = rec.clone();
                for expr in rec.0.values_mut() {
                    let reduced = beta_reduction(global_scope, local_scope, expr);
                    *expr = reduced;
                }
                Expr::Record(rec)
            }
        }
        Expr::RecordAccess(ra) => {
            let record = br(local_scope, &ra.record);
            if let Expr::Record(ref r) = record {
                r.0.get(&ra.member)
                    .expect("nonexistent key in member")
                    .clone()
            } else if ra.record.is_realized() {
                panic!("expected Record, got {:?}", ra.record)
            } else {
                Expr::RecordAccess(RecordAccess {
                    record: Box::new(record),
                    member: ra.member.clone(),
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
        (StringConcat, String(ref a), String(ref b)) => String(a.to_owned() + &b),
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
