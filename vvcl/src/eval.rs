use crate::ast::{BinaryOperation, BinaryOperator, Expr, Ident};
use std::collections::HashMap;

type Scope = HashMap<Ident, Expr>;

pub fn beta_reduction(orig_scope: &Scope, e: Expr) -> Expr {
    match e {
        Expr::Block(b) => {
            let mut new_scope = orig_scope.clone();
            for def in &b.definitions {
                let r = new_scope.insert(
                    def.name.clone(),
                    beta_reduction(orig_scope, def.body.clone()),
                );
                if r.is_some() {
                    panic!("redefined {}!", def.name.0)
                }
            }
            beta_reduction(&new_scope, *b.expr)
        }
        Expr::Value(v) => {
            if let Some(e) = orig_scope.get(&v) {
                beta_reduction(orig_scope, e.clone())
            } else {
                println!("variable not in scope: {}", v.0);
                Expr::Value(v)
            }
        }
        Expr::BinaryOperation(BinaryOperation {
            left,
            operator,
            right,
        }) => {
            let bo = BinaryOperation {
                left: Box::new(beta_reduction(orig_scope, *left)),
                operator,
                right: Box::new(beta_reduction(orig_scope, *right)),
            };
            apply_binary_operation(bo)
        }
        _ => e,
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
        _ => Expr::BinaryOperation(BinaryOperation {
            left,
            operator,
            right,
        }),
    }
}
