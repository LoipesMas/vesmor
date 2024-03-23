use std::{collections::HashMap, fs};

mod ast;
mod eval;
mod parse;

fn main() {
    let file_path = "./test.vvc";
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    println!("{}", contents);

    let contents = contents.replace('\n', "");
    let (_input, f) = parse::parse_fun(&contents).unwrap();
    dbg!(&f.body);
    let reduced = eval::beta_reduction(&HashMap::new(), f.body);
    dbg!(&reduced);
    let arguments = HashMap::from([
        (ast::Ident("a".to_string()), ast::Expr::Int(100)),
        (ast::Ident("b".to_string()), ast::Expr::Int(57)),
        (f.name, reduced.clone()),
    ]);
    let applied = eval::beta_reduction(&arguments, reduced);
    dbg!(&applied);
}
