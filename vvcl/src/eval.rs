use crate::ast::{BinaryOperation, BinaryOperator, Block, Definition, Expr, Function, Ident};
use std::collections::HashMap;

type Scope = HashMap<Ident, Expr>;

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
            let mut new_definitions = vec![];
            for def in &b.definitions {
                let reduced_body = br(&combined_scope, &def.body);
                let reduced_def = Definition {
                    name: def.name.clone(),
                    body: reduced_body.clone(),
                };
                new_definitions.push(reduced_def);
                let r = new_scope.insert(def.name.clone(), reduced_body);
                if r.is_some() {
                    panic!("redefined {}!", def.name.0)
                }
            }
            let new_expr = br(&new_scope, &b.expr);
            if new_expr.is_realized() {
                new_expr
            } else {
                // preserve scope for not-realized values
                Expr::Block(Block {
                    definitions: new_definitions,
                    expr: Box::new(new_expr),
                })
            }
        }
        Expr::Value(v) => {
            if let Some(e) = combined_scope.get(v) {
                match br(&combined_scope, e) {
                    // Does this do anything?
                    a @ Expr::Value(_) => br(&combined_scope, &a),
                    a => a,
                }
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
                    dbg!("foo");
                    if !reduced_args.iter().all(Expr::is_realized) {
                        dbg!("bar");
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
                    let r = br(&inner_scope, &fun.body);
                    dbg!(&inner_scope);
                    dbg!(&r);
                    r
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
    match (operator, &*left, &*right) {
        (BinaryOperator::IntAdd, &Expr::Int(a), &Expr::Int(b)) => Expr::Int(a + b),
        (BinaryOperator::StringConcat, Expr::String(ref a), Expr::String(ref b)) => {
            Expr::String(a.to_owned() + &b)
        }
        _ => Expr::BinaryOperation(BinaryOperation {
            left,
            operator,
            right,
        }),
    }
}
