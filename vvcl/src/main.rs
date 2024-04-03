use std::{collections::HashMap, fs, time::Instant};

mod ast;
mod builtin_functions;
mod eval;
mod parse;
mod utils;
use utils::ident;
mod typ_check;

use crate::builtin_functions::builtin_function_type_definitions;
use crate::{parse::TopLevelDefinition, typ_check::default_type_definitions};
use ast::Definition;
use utils::{default_global_scope, wrap_in_span};

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

    let input = wrap_in_span(&contents);
    let (input, defs) = parse::top_definitions(input).unwrap();

    if !input.is_empty() {
        eprintln!("parsing failed! input left:\n{input}");
        return;
    }

    let mut type_definitions = default_type_definitions();

    dbg!(type_definitions.keys());

    let mut exprs = vec![];

    for def in defs.into_iter() {
        match def {
            TopLevelDefinition::Type(t) => {
                type_definitions.insert(t.name, t.body.to_type(&type_definitions).unwrap());
            }
            TopLevelDefinition::Expr(e) => exprs.push(e),
        }
    }
    let mut global_scope_types = builtin_function_type_definitions(&type_definitions);

    for def in &exprs {
        let typ = dbg!(typ_check::check(
            &global_scope_types,
            &HashMap::new(),
            &type_definitions,
            &def.body
        ))
        .unwrap();
        global_scope_types.insert(def.name.clone(), typ);
    }
    return;

    // 1st pass of "compilation"
    // without global scope
    let reduced_defs: Vec<Definition> = exprs
        .iter()
        .map(|d| Definition {
            body: eval::beta_reduction(&HashMap::new(), &HashMap::new(), &d.body),
            name: d.name.clone(),
        })
        .collect();
    let mut global_scope = default_global_scope();
    global_scope.extend(utils::map_from_defs(reduced_defs));

    // debug
    let main_body = global_scope.get(&ident("main")).unwrap();
    dbg!(main_body);

    // // 2nd pass of "compilation"
    // // with global scope
    // for f in funs {
    //     let reduced_f = eval::beta_reduction(
    //         &global_scope,
    //         &HashMap::new(),
    //         &ast::Expr::Function(f.clone()),
    //     );
    //     global_scope.insert(f.name.clone(), reduced_f);
    // }
    //
    // debug
    let main_body = global_scope.get(&ident("main")).unwrap();
    dbg!(main_body);

    println!("======= final call");

    let mut local_scope = HashMap::new();
    local_scope.insert(ident("a"), utils::bool_to_enum(true));
    local_scope.insert(ident("b"), int_e(10));

    let t0 = Instant::now();

    let res = eval::beta_reduction(&global_scope, &local_scope, main_body);
    let t1 = Instant::now();
    dbg!(t1 - t0);
    dbg!(&res);
    let fin = utils::get_function_value(res).unwrap();
    println!("output: {}", fin);
}
