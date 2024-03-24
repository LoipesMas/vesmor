use std::{collections::HashMap, fs, time::Instant};

mod ast;
mod eval;
mod parse;
mod utils;

#[allow(non_upper_case_globals)]
const int_e: fn(i64) -> ast::Expr = ast::Expr::Int;

fn str_e(s: &str) -> ast::Expr {
    ast::Expr::String(s.to_owned())
}

fn ident(s: &str) -> ast::Ident {
    ast::Ident(s.to_owned())
}

fn get_function_value(expr: ast::Expr) -> Result<String, ()> {
    if let ast::Expr::Function(fun) = expr {
        match *fun.body {
            ast::Expr::Int(v) => Ok(v.to_string()),
            ast::Expr::Float(v) => Ok(v.to_string()),
            ast::Expr::String(v) => Ok(v),
            ast::Expr::Record(v) if fun.body.is_realized() => Ok(format!("{:?}", v)),
            _ => Err(()),
        }
    } else {
        Err(())
    }
}

fn main() {
    let file_path = "./test.vvc";
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let contents = contents.replace(['\n', ' '], "");
    let (input, funs) = parse::all_funs(&contents).unwrap();

    if !input.is_empty() {
        eprintln!("parsing failed! input left:\n{input}");
        return;
    }

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
    let mut local_scope = HashMap::new();
    local_scope.insert(ident("a"), int_e(1));
    local_scope.insert(ident("b"), int_e(10));

    let t0 = Instant::now();

    let res = eval::beta_reduction(&global_scope, &local_scope, main_body);
    let t1 = Instant::now();
    dbg!(t1 - t0);
    dbg!(&res);
    let fin = get_function_value(res).unwrap();
    println!("output: {}", fin);
}
