use crate::ast::{BinaryOperation, BinaryOperator, Block, Definition, Expr, Function, Ident};
use std::collections::HashMap;

type Scope = HashMap<Ident, Expr>;

fn scope_from_defs(defs: &Vec<Definition>) -> Scope {
    let mut scope = Scope::new();
    for def in defs {
        scope.insert(def.name.clone(), def.body.clone());
    }
    scope
}

pub fn beta_reduction(global_scope: &Scope, scope: &Scope, e: &Expr) -> Expr {
    let br = |s, e| beta_reduction(global_scope, s, e);
    let combined_scope = {
        let mut combined_scope = global_scope.clone();
        combined_scope.extend(scope.to_owned());
        combined_scope
    };
    match e {
        Expr::Block(b) => {
            let mut new_scope = scope.clone();
            let reduced_defs = b
                .definitions
                .iter()
                .map(|d| Definition {
                    body: br(&combined_scope, &d.body),
                    name: d.name.clone(),
                })
                .collect();
            new_scope.extend(scope_from_defs(&reduced_defs));
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
                br(&combined_scope, e)
            } else {
                println!("variable not in scope: {}", v.0);
                Expr::Value(v.clone())
            }
        }
        Expr::BinaryOperation(BinaryOperation {
            left,
            operator,
            right,
        }) => {
            let bo = BinaryOperation {
                left: Box::new(br(&combined_scope, left)),
                operator: *operator,
                right: Box::new(br(&combined_scope, right)),
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
            body: Box::new(br(&combined_scope, body)),
        }),
        Expr::FunctionCall(fc) => {
            if let Some(fun_expr) = combined_scope.get(&fc.name) {
                if let Expr::Function(fun) = fun_expr {
                    let reduced_args =
                        Vec::from_iter(fc.arguments.iter().map(|a| br(&combined_scope, a)));
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
                dbg!("missing function: ", &fc.name.0);
                Expr::FunctionCall(fc.clone())
            }
        }
        _ => e.clone(),
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
