use nannou::prelude::*;
use vvcl::utils::ident;

fn main() {
    nannou::app(model).update(update).simple_window(view).run();
}

enum Command {
    DrawLine { start: Vec2, end: Vec2 },
}

// < x = 5.0; y = 7.0; > -> pt2(5.0,7.0)
fn rec_to_vec2(record: vvcl::ast::Record) -> Vec2 {
    let x = record.get(&ident("x")).unwrap();
    let y = record.get(&ident("y")).unwrap();
    if let (vvcl::ast::Expr::Float(x), vvcl::ast::Expr::Float(y)) = (x, y) {
        pt2(*x as f32, *y as f32)
    } else {
        panic!("expected <x: Float; y: Float;>, got {record:?}");
    }
}

fn command_from_record(ev: &vvcl::ast::EnumVariant) -> Command {
    if ev.enu.0 == "Command" {
        match ev.variant.0.as_str() {
            "DrawLine" => {
                let body = ev
                    .body
                    .as_ref()
                    .expect("Expected DrawLine body to be present!");
                if let vvcl::ast::Expr::Record(ref record) = **body {
                    let start_rec = record.get(&ident("start")).unwrap();
                    let end_rec = record.get(&ident("end")).unwrap();
                    if let (vvcl::ast::Expr::Record(start_rec), vvcl::ast::Expr::Record(end_rec)) =
                        (start_rec, end_rec)
                    {
                        let start = rec_to_vec2(start_rec.clone());
                        let end = rec_to_vec2(end_rec.clone());
                        Command::DrawLine { start, end }
                    } else {
                        panic!("expected records, got {:?}", (start_rec, end_rec))
                    }
                } else {
                    panic!("Expected DrawLine body to have a record")
                }
            }
            _ => panic!("Unknown Command variant {}", ev.variant.0),
        }
    } else {
        panic!("Expected Enum Variant of 'Command', got {}", ev.enu.0)
    }
}

struct Model {
    color: Rgb<u8>,
    line_weight: f32,
    commands: Vec<Command>,
    runtime: Runtime,
}

struct Runtime {
    global_scope: vvcl::eval::ScopeMap,
    game_state: vvcl::ast::Expr,
    update_function: vvcl::ast::Expr,
}

fn init_runtime() -> Runtime {
    let mut global_scope = vvcl::utils::default_global_scope();

    let file_path = "./game.vvc";
    let contents =
        std::fs::read_to_string(file_path).expect("Should have been able to read the file");

    // FIXME: this also removes spaces inside strings...
    // and leaving it in will probably make it easier to show parser errors
    let contents = contents.replace(['\n', ' '], "");
    let (input, funs) = vvcl::parse::all_funs(&contents).unwrap();

    if !input.is_empty() {
        panic!("parsing failed! input left:\n{input}");
    };

    // 1st pass of "compilation"
    // without global scope
    for f in &funs {
        let reduced_f = vvcl::eval::beta_reduction(
            &vvcl::eval::ScopeMap::new(),
            &vvcl::eval::ScopeMap::new(),
            &vvcl::ast::Expr::Function(f.clone()),
        );
        let res = global_scope.insert(f.name.clone(), reduced_f);
        if res.is_some() {
            panic!("redefined function {}", f.name.0);
        };
    }
    // 2nd pass of "compilation"
    // with global scope
    for f in funs {
        let reduced_f = vvcl::eval::beta_reduction(
            &global_scope,
            &vvcl::eval::ScopeMap::new(),
            &vvcl::ast::Expr::Function(f.clone()),
        );
        global_scope.insert(f.name.clone(), reduced_f);
    }

    let init_fun = global_scope
        .get(&vvcl::utils::ident("init"))
        .unwrap()
        .clone();
    let game_state = if let vvcl::ast::Expr::Function(f) = init_fun {
        *f.body
    } else {
        panic!("init should be a function! got {init_fun:?}")
    };
    let update_function = global_scope
        .get(&vvcl::utils::ident("update"))
        .unwrap()
        .clone();

    Runtime {
        global_scope,
        game_state,
        update_function,
    }
}

fn model(_app: &App) -> Model {
    Model {
        color: GREEN,
        line_weight: 4.0,
        commands: vec![],
        runtime: init_runtime(),
    }
}

fn update(_app: &App, model: &mut Model, update: Update) {
    let delta = update.since_last.as_secs_f64();
    let mut local_scope = vvcl::eval::ScopeMap::new();
    local_scope.insert(vvcl::utils::ident("delta"), vvcl::ast::Expr::Float(delta));
    local_scope.insert(vvcl::utils::ident("self"), model.runtime.game_state.clone());

    let updated = vvcl::eval::beta_reduction(
        &model.runtime.global_scope,
        &local_scope,
        &model.runtime.update_function,
    );
    let (game_state, commands) = if let vvcl::ast::Expr::Function(f) = updated {
        if let vvcl::ast::Expr::Record(r) = *f.body {
            let game_state = r.get(&vvcl::utils::ident("self")).unwrap().clone();
            let commands =
                if let vvcl::ast::Expr::List(l) = r.get(&vvcl::utils::ident("commands")).unwrap() {
                    let mut ret = vec![];
                    for command in l {
                        if let vvcl::ast::Expr::EnumVariant(ev) = command {
                            ret.push(command_from_record(ev));
                        } else {
                            panic!("expected Enum Variant of Command, got {command:?}")
                        }
                    }
                    ret
                } else {
                    panic!("expected list of commands, got `IDK` lol");
                };
            (game_state, commands)
        } else {
            panic!("Expected Record from update, got {:?}", *f.body)
        }
    } else {
        // this should be unreachable, unless I made some mistakes ;p
        unreachable!()
    };

    model.runtime.game_state = game_state;
    model.commands = commands;
}

fn view(app: &App, model: &Model, frame: Frame) {
    frame.clear(BLACK);
    let draw = app.draw();

    for command in &model.commands {
        match command {
            Command::DrawLine { start, end } => {
                draw.line()
                    .start(*start)
                    .end(*end)
                    .weight(model.line_weight)
                    .caps_round()
                    .color(model.color);
            }
        }
    }

    draw.to_frame(app, &frame).unwrap();
}
