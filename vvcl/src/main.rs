use std::{collections::HashMap, fs, time::Instant};

mod ast;
mod builtin_functions;
mod eval;
mod parse;
mod utils;
use utils::ident;

use crate::utils::default_global_scope;

#[allow(non_upper_case_globals)]
const int_e: fn(i64) -> ast::Expr = ast::Expr::Int;

#[allow(dead_code)]
fn str_e(s: &str) -> ast::Expr {
    ast::Expr::String(s.to_owned())
}

fn main() {
    let file_path = "./test.vvc";
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    // FIXME: this also removes spaces inside strings...
    // and leaving it in will probably make it easier to show parser errors
    let contents = contents.replace(['\n', ' '], "");
    let (input, funs) = parse::all_funs(&contents).unwrap();

    if !input.is_empty() {
        eprintln!("parsing failed! input left:\n{input}");
        return;
    }

    let mut global_scope = default_global_scope();

    // insert builtin_functions

    // 1st pass of "compilation"
    // without global scope
    for f in &funs {
        let reduced_f = eval::beta_reduction(
            &HashMap::new(),
            &HashMap::new(),
            &ast::Expr::Function(f.clone()),
        );
        let res = global_scope.insert(f.name.clone(), reduced_f);
        if res.is_some() {
            panic!("redefined function {}", f.name.0);
        }
    }

    // debug
    let main_body = global_scope.get(&ident("main")).unwrap();
    dbg!(main_body);

    // 2nd pass of "compilation"
    // with global scope
    for f in funs {
        let reduced_f = eval::beta_reduction(
            &global_scope,
            &HashMap::new(),
            &ast::Expr::Function(f.clone()),
        );
        global_scope.insert(f.name.clone(), reduced_f);
    }

    // debug
    let main_body = global_scope.get(&ident("main")).unwrap();
    dbg!(main_body);

    println!("======= final call");

    let mut local_scope = HashMap::new();
    local_scope.insert(ident("a"), ast::Expr::Bool(false));
    local_scope.insert(ident("b"), int_e(10));

    let t0 = Instant::now();

    let res = eval::beta_reduction(&global_scope, &local_scope, main_body);
    let t1 = Instant::now();
    dbg!(t1 - t0);
    dbg!(&res);
    let fin = utils::get_function_value(res).unwrap();
    println!("output: {}", fin);
}
