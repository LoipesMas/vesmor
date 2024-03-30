use std::{cell::RefCell, str::FromStr};

use nannou::{
    prelude::*,
    wgpu::{Backends, DeviceDescriptor, Limits},
};
use vvcl::utils::ident;

#[derive(Clone)]
pub struct SourceCode {
    pub code: String,
    pub acknowledged: bool,
    pub hot_reload: bool,
}

impl Default for SourceCode {
    fn default() -> Self {
        Self {
            code: include_str!("../game.vvc").to_string(),
            acknowledged: false,
            hot_reload: false,
        }
    }
}

thread_local! {
    pub static SOURCE_CODE: RefCell<SourceCode> = Default::default();
}

enum Command {
    DrawLine { start: Vec2, end: Vec2 },
}

fn web_print(s: &str) {
    web_sys::console::log_1(&web_sys::js_sys::JsString::from_str(s).unwrap())
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

pub struct Model {
    color: Rgb<u8>,
    line_weight: f32,
    commands: Vec<Command>,
    runtime: Runtime,
}

struct Runtime {
    global_scope: vvcl::eval::ScopeMap,
    game_state: vvcl::ast::Expr,
    update_function: vvcl::ast::Expr,
    event_function: vvcl::ast::Expr,
}

fn init_runtime(source_code: &SourceCode) -> Runtime {
    let mut global_scope = vvcl::utils::default_global_scope();

    // let file_path = "./game.vvc";
    // let contents =
    //     std::fs::read_to_string(file_path).expect("Should have been able to read the file");
    let contents = &source_code.code;

    // FIXME: this also removes spaces inside strings...
    // and leaving it in will probably make it easier to show parser errors
    let contents = contents.replace(['\n', ' '], "");
    let contents = vvcl::utils::wrap_in_span(&contents);
    let (input, funs) = vvcl::parse::all_funs(contents).unwrap();

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
        .get(&vvcl::utils::ident("update_handler"))
        .unwrap()
        .clone();
    let event_function = global_scope
        .get(&vvcl::utils::ident("event_handler"))
        .unwrap()
        .clone();

    Runtime {
        global_scope,
        game_state,
        update_function,
        event_function,
    }
}
fn key_pressed(app: &App, model: &mut Model, key: Key) {
    let key_str = format!("{key:?}");
    let key_enum = vvcl::utils::enum_variant("Key", &key_str, None);
    let event_enum = vvcl::utils::enum_variant("Event", "KeyPressed", Some(key_enum));

    let mut local_scope = vvcl::eval::ScopeMap::new();
    local_scope.insert(vvcl::utils::ident("event"), event_enum);
    local_scope.insert(vvcl::utils::ident("self"), model.runtime.game_state.clone());

    let evaled = vvcl::eval::beta_reduction(
        &model.runtime.global_scope,
        &local_scope,
        &model.runtime.event_function,
    );

    let (game_state, commands) = extract_state_and_commands(evaled);
    model.runtime.game_state = game_state;
    model.commands = commands;
}

pub fn model(app: &App) -> Model {
    SOURCE_CODE.with_borrow_mut(|sc| {
        sc.acknowledged = true;
        Model {
            color: GREEN,
            line_weight: 4.0,
            commands: vec![],
            runtime: init_runtime(sc),
        }
    })
}

fn extract_state_and_commands(function: vvcl::ast::Expr) -> (vvcl::ast::Expr, Vec<Command>) {
    if let vvcl::ast::Expr::Function(f) = function {
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
    }
}

fn update(_app: &App, model: &mut Model, update: Update) {
    SOURCE_CODE.with_borrow_mut(|sc| {
        if !sc.acknowledged {
            sc.acknowledged = true;
            if sc.hot_reload {
                let game_state = model.runtime.game_state.clone();
                model.runtime = init_runtime(sc);
                model.runtime.game_state = game_state;
            } else {
                model.runtime = init_runtime(sc);
            }
        }
    });
    let delta = update.since_last.as_secs_f64().min(1.0 / 60.0);
    let mut local_scope = vvcl::eval::ScopeMap::new();
    local_scope.insert(vvcl::utils::ident("delta"), vvcl::ast::Expr::Float(delta));
    local_scope.insert(vvcl::utils::ident("self"), model.runtime.game_state.clone());

    let updated = vvcl::eval::beta_reduction(
        &model.runtime.global_scope,
        &local_scope,
        &model.runtime.update_function,
    );

    let (game_state, commands) = extract_state_and_commands(updated);

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

pub async fn run_app() {
    app::Builder::new_async(|app| {
        Box::new(async move {
            create_window(app).await;
            model(app)
        })
    })
    .backends(Backends::PRIMARY | Backends::GL)
    .update(update)
    .run_async()
    .await;
}

async fn create_window(app: &App) {
    let device_desc = DeviceDescriptor {
        limits: Limits {
            max_texture_dimension_2d: 8192,
            ..Limits::downlevel_webgl2_defaults()
        },
        ..Default::default()
    };

    app.new_window()
        .device_descriptor(device_desc)
        .size(720, 720)
        .title("VVC")
        // .raw_event(raw_event)
        .key_pressed(key_pressed)
        // .key_released(key_released)
        // .mouse_pressed(mouse_pressed)
        // .mouse_moved(mouse_moved)
        // .mouse_released(mouse_released)
        // .mouse_wheel(mouse_wheel)
        // .touch(touch)
        .view(view)
        .build_async()
        .await
        .unwrap();
}
