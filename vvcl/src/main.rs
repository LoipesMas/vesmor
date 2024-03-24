use std::{
    collections::HashMap,
    fs,
    time::{Duration, Instant},
};

mod ast;
mod eval;
mod parse;

#[allow(non_upper_case_globals)]
const int_e: fn(i64) -> ast::Expr = ast::Expr::Int;

fn str_e(s: &str) -> ast::Expr {
    ast::Expr::String(s.to_owned())
}

fn ident(s: &str) -> ast::Ident {
    ast::Ident(s.to_owned())
}

fn main() {
    let file_path = "./test.vvc";
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    println!("{}", contents);

    let contents = contents.replace('\n', "");
    let (_input, funs) = parse::all_funs(&contents).unwrap();
    let mut global_scope = HashMap::new();
    for f in funs {
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

    println!("======= final call");

    let main_body = global_scope.get(&ident("main")).unwrap();
    let mut empty_scope = HashMap::new();
    empty_scope.insert(ident("a"), int_e(1));
    empty_scope.insert(ident("b"), int_e(10));

    dbg!(main_body);

    let t0 = Instant::now();

    for _ in 0..300000 {
        let res = eval::beta_reduction(&global_scope, &empty_scope, main_body);
    }
    let t1 = Instant::now();
    dbg!(t1 - t0);
    // dbg!(res);
}
