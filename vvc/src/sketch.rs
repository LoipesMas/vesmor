use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    str::FromStr,
};

use nannou::{
    prelude::*,
    wgpu::{Backends, DeviceDescriptor, Limits},
};
use vvcl::{
    ast::Ident,
    typ_check::{float_type, Type},
    utils::ident,
};

#[derive(Clone)]
pub struct SourceCode {
    pub code: String,
    pub acknowledged: bool,
    pub hot_reload: bool,
}

impl Default for SourceCode {
    fn default() -> Self {
        let code = if cfg!(target_family = "wasm") {
            include_str!("../static/game.vvc").to_string()
        } else {
            let file_path = "static/game.vvc";
            std::fs::read_to_string(file_path).expect("Should have been able to read the file")
        };
        Self {
            code,
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

const DEFAULT_VIEWPORT_SIZE: (u32, u32) = (720, 720);

pub fn check_source_code(code: &str) -> Result<(), String> {
    let contents = code.replace(['\n', ' '], "");
    let contents = vvcl::utils::wrap_in_span(&contents);
    let (input, defs) = vvcl::parse::top_definitions(contents).map_err(|e| e.to_string())?;

    if !input.is_empty() {
        return Err(format!("parsing failed! input left:\n{input}"));
    };
    let mut type_definitions = vvcl::typ_check::default_type_definitions();
    extend_type_definitions(&mut type_definitions);

    let mut exprs = vec![];

    for def in defs.into_iter() {
        match def {
            vvcl::parse::TopLevelDefinition::Type(t) => {
                let body_type = t
                    .body
                    .to_type(&type_definitions)
                    .map_err(|e| format!("In type {}: {}", t.name, e))?;
                type_definitions.insert(t.name, body_type);
            }
            vvcl::parse::TopLevelDefinition::Expr(e) => exprs.push(e),
        }
    }
    let mut global_scope_types =
        vvcl::builtin_functions::builtin_function_type_definitions(&type_definitions);

    for def in &exprs {
        let typ = vvcl::typ_check::check(
            &global_scope_types,
            &HashMap::new(),
            &type_definitions,
            &def.body,
        )
        .map_err(|e| format!("In definition of {}: {}", def.name, e))?;
        global_scope_types.insert(def.name.clone(), typ);
    }

    let init_type = global_scope_types
        .get(&ident("init"))
        .ok_or("`init` value should be defined at top-level".to_string())?;

    let update_function_type = global_scope_types
        .get(&ident("update_handler"))
        .ok_or("`update_handler` function should be defined at top-level".to_string())?;

    if let Type::Function {
        args,
        return_type: _,
    } = update_function_type
    {
        let first_type = args
            .first()
            .ok_or("`update_handler` function should take 2 arguments: game state and delta")?;
        let second_type = args
            .get(1)
            .ok_or("`update_handler` function should take 2 arguments: game state and delta")?;
        if first_type != init_type {
            return Err(format!("First argument to `update_handler` should be of the same type as `init`, got {first_type} and {init_type}."));
        }
        if *second_type != float_type() {
            return Err(format!(
                "Second argument to `update_handler` should be of type Float, got {second_type}."
            ));
        }
        // TODO: check return type
    }

    // TODO: check `event_handler`

    Ok(())
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
    bg_color: Rgb<u8>,
    line_weight: f32,
    commands: Vec<Command>,
    runtime: Runtime,
    // TODO: I think we could just use app.keys instead
    pressed_keys: HashSet<Key>,
    viewport_resolution: Vec2,
}

struct Runtime {
    global_scope: vvcl::eval::ScopeMap,
    game_state: vvcl::ast::Expr,
    update_function: vvcl::ast::Expr,
    event_function: vvcl::ast::Expr,
}

macro_rules! keys {
    ($($tag:ident),*) => {
        [ $(Key::$tag),*]
    };
}

fn event_enum() -> Type {
    let keys = keys![A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z];
    let keys_enum = Type::Enum {
        enu: ident("Key"),
        variants: keys.map(|k| (ident(&format!("{k:?}")), None)).into(),
    };
    let event_variants = ["KeyPressed", "KeyDown"]
        .map(|v| (ident(v), Some(keys_enum.clone())))
        .into();
    Type::Enum {
        enu: ident("Event"),
        variants: event_variants,
    }
}

fn extend_type_definitions(type_definitions: &mut HashMap<Ident, Type>) {
    let vec2_type = Type::Record([(ident("x"), float_type()), (ident("y"), float_type())].into());
    type_definitions.insert(ident("Vec2"), vec2_type.clone());
    let draw_line_body = Type::Record(
        [
            (ident("start"), vec2_type.clone()),
            (ident("end"), vec2_type.clone()),
        ]
        .into(),
    );
    let command_enum = Type::Enum {
        enu: ident("Command"),
        variants: [(ident("DrawLine"), Some(draw_line_body))].into(),
    };
    type_definitions.insert(ident("Command"), command_enum.clone());
    type_definitions.insert(ident("Event"), event_enum().clone());
}

fn init_runtime(source_code: &SourceCode) -> Runtime {
    let contents = &source_code.code;

    // FIXME: this also removes spaces inside strings...
    // and leaving it in will probably make it easier to show parser errors
    let contents = contents.replace(['\n', ' '], "");
    let contents = vvcl::utils::wrap_in_span(&contents);
    let (input, defs) = vvcl::parse::top_definitions(contents).unwrap();

    if !input.is_empty() {
        panic!("parsing failed! input left:\n{input}");
    };

    let mut type_definitions = vvcl::typ_check::default_type_definitions();
    extend_type_definitions(&mut type_definitions);

    let mut exprs = vec![];

    for def in defs.into_iter() {
        match def {
            vvcl::parse::TopLevelDefinition::Type(t) => {
                type_definitions.insert(t.name, t.body.to_type(&type_definitions).unwrap());
            }
            vvcl::parse::TopLevelDefinition::Expr(e) => exprs.push(e),
        }
    }
    let mut global_scope_types =
        vvcl::builtin_functions::builtin_function_type_definitions(&type_definitions);

    for def in &exprs {
        let typ = vvcl::typ_check::check(
            &global_scope_types,
            &HashMap::new(),
            &type_definitions,
            &def.body,
        )
        .unwrap();
        global_scope_types.insert(def.name.clone(), typ);
    }

    // 1st pass of "compilation"
    // without global scope
    let reduced_defs: Vec<vvcl::ast::Definition> = exprs
        .iter()
        .map(|d| vvcl::ast::Definition {
            body: vvcl::eval::beta_reduction(&HashMap::new(), &HashMap::new(), &d.body),
            name: d.name.clone(),
        })
        .collect();
    let mut global_scope = vvcl::utils::default_global_scope();
    global_scope.extend(vvcl::utils::map_from_defs(reduced_defs));

    let game_state = global_scope.get(&ident("init")).unwrap().clone();
    let update_function =
        if let vvcl::ast::Expr::Function(f) = global_scope.get(&ident("update_handler")).unwrap() {
            *f.body.clone()
        } else {
            panic!("`update_handler` should have been a function.")
        };
    let event_function =
        if let vvcl::ast::Expr::Function(f) = global_scope.get(&ident("event_handler")).unwrap() {
            *f.body.clone()
        } else {
            panic!("`event_handler` should have been a function.")
        };

    Runtime {
        global_scope,
        game_state,
        update_function,
        event_function,
    }
}

fn key_down(model: &mut Model, key: Key) {
    let key_str = format!("{key:?}");
    let key_enum = vvcl::utils::enum_variant("Key", &key_str, None);
    let event_enum = vvcl::utils::enum_variant("Event", "KeyDown", Some(key_enum));

    let mut local_scope = vvcl::eval::ScopeMap::new();
    local_scope.insert(ident("event"), event_enum);
    local_scope.insert(ident("game"), model.runtime.game_state.clone());

    let evaled = vvcl::eval::beta_reduction(
        &model.runtime.global_scope,
        &local_scope,
        &model.runtime.event_function,
    );

    let (game_state, commands) = extract_state_and_commands(evaled);
    model.runtime.game_state = game_state;
    model.commands.extend(commands);
}

fn key_pressed(app: &App, model: &mut Model, key: Key) {
    let is_new = model.pressed_keys.insert(key);
    if !is_new {
        // only send event if key wasn't already pressed
        return;
    }
    let key_str = format!("{key:?}");
    let key_enum = vvcl::utils::enum_variant("Key", &key_str, None);
    let event_enum = vvcl::utils::enum_variant("Event", "KeyPressed", Some(key_enum));

    let mut local_scope = vvcl::eval::ScopeMap::new();
    local_scope.insert(ident("event"), event_enum);
    local_scope.insert(ident("game"), model.runtime.game_state.clone());

    let evaled = vvcl::eval::beta_reduction(
        &model.runtime.global_scope,
        &local_scope,
        &model.runtime.event_function,
    );

    let (game_state, commands) = extract_state_and_commands(evaled);
    model.runtime.game_state = game_state;
    model.commands.extend(commands);
}

fn key_released(app: &App, model: &mut Model, key: Key) {
    model.pressed_keys.remove(&key);
}

pub fn model(app: &App) -> Model {
    SOURCE_CODE.with_borrow_mut(|sc| {
        sc.acknowledged = true;
        Model {
            color: rgb8(255, 125, 0),
            bg_color: rgb8(0, 21, 36),
            line_weight: 1.0,
            commands: vec![],
            runtime: init_runtime(sc),
            pressed_keys: HashSet::new(),
            viewport_resolution: pt2(
                DEFAULT_VIEWPORT_SIZE.0 as f32,
                DEFAULT_VIEWPORT_SIZE.1 as f32,
            ),
        }
    })
}

fn extract_state_and_commands(result: vvcl::ast::Expr) -> (vvcl::ast::Expr, Vec<Command>) {
    if let vvcl::ast::Expr::Record(r) = result {
        let game_state = r.get(&ident("game")).unwrap().clone();
        let commands = if let vvcl::ast::Expr::List(l) = r.get(&ident("commands")).unwrap() {
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
        panic!("Expected Record from update, got {:?}", result)
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
    local_scope.insert(ident("delta"), vvcl::ast::Expr::Float(delta));
    local_scope.insert(ident("game"), model.runtime.game_state.clone());

    let updated = vvcl::eval::beta_reduction(
        &model.runtime.global_scope,
        &local_scope,
        &model.runtime.update_function,
    );

    let (game_state, commands) = extract_state_and_commands(updated);

    model.runtime.game_state = game_state;
    model.commands = commands;

    for key in model.pressed_keys.clone() {
        key_down(model, key)
    }
}

fn view(app: &App, model: &Model, frame: Frame) {
    frame.clear(model.bg_color);
    let draw = app.draw();

    for command in &model.commands {
        match command {
            Command::DrawLine { start, end } => {
                let start = *start * model.viewport_resolution;
                let end = *end * model.viewport_resolution;
                draw.line()
                    .start(start)
                    .end(end)
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
    .loop_mode(LoopMode::rate_fps(60.0))
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
        .size(DEFAULT_VIEWPORT_SIZE.0, DEFAULT_VIEWPORT_SIZE.1)
        .title("VVC")
        // .raw_event(raw_event)
        .key_pressed(key_pressed)
        .key_released(key_released)
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
