use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    str::FromStr,
};

use nannou::{
    prelude::*,
    wgpu::{Backends, DeviceDescriptor, Limits},
};
use vesmish::{
    ast::Ident,
    parse::ParseError,
    typ_check::{concrete_list_type, float_type, Type},
    utils::{enum_variant, ident, ErrWithContext},
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
            include_str!("../static/pong.ves").to_string()
        } else {
            let file_path = "static/pong.ves";
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

// used in web build
#[allow(dead_code)]
pub fn check_source_code(code: &str) -> Result<(), String> {
    let contents = vesmish::utils::wrap_in_span(code);
    let (input, defs) = vesmish::parse::top_definitions(contents).map_err(|e| match e {
        ParseError::Failure(e) | ParseError::Error(e) => {
            format!("Syntax error in definition at line {}", e.0.location_line())
        }
        ParseError::Incomplete(_) => unreachable!("only complete parsers used"),
    })?;

    if !input.is_empty() {
        return Err(format!("parsing failed! input left:\n{input}"));
    };
    let mut type_definitions = vesmish::typ_check::default_type_definitions();
    extend_type_definitions(&mut type_definitions);

    let mut exprs = vec![];

    for def in defs.into_iter() {
        match def {
            vesmish::parse::TopLevelDefinition::Type(t) => {
                let body_type = t
                    .body
                    .to_type(&type_definitions)
                    .map_err(|e| format!("In type {}: {}", t.name, e))?;
                if type_definitions.contains_key(&t.name) {
                    return Err(format!("Redefinition of type '{}'", t.name));
                }
                type_definitions.insert(t.name, body_type);
            }
            vesmish::parse::TopLevelDefinition::Expr(e) => exprs.push(e),
        }
    }
    let mut global_scope_types =
        vesmish::builtin_functions::builtin_function_type_definitions(&type_definitions);

    // FIXME: just having two passes doesn't solve it...
    // first type check pass
    for def in &exprs {
        if matches!(def.body.borrow(), vesmish::ast::Expr::Function(_)) {
            let typ =
                vesmish::typ_check::Type::from_function_def_unchecked(&def.body, &type_definitions)
                    .with_context(format!("In signature of {}: ", def.name))?;
            global_scope_types.insert(def.name.clone(), typ.clone());
        }
        let typ = dbg!(vesmish::typ_check::check(
            &global_scope_types,
            &HashMap::new(),
            &type_definitions,
            def.body.clone()
        ));
        // type checking can fail, because not all types are known yet
        if let Ok(typ) = typ {
            let old_typ = global_scope_types.insert(def.name.clone(), typ.clone());
            assert!(old_typ.map_or(true, |o| o == typ));
        }
    }
    // second type check pass, with global scope
    for def in &exprs {
        if matches!(def.body.borrow(), vesmish::ast::Expr::Function(_)) {
            let typ =
                vesmish::typ_check::Type::from_function_def_unchecked(&def.body, &type_definitions)
                    .with_context(format!("In signature of {}: ", def.name))?;
            global_scope_types.insert(def.name.clone(), typ.clone());
        }
        let typ = dbg!(vesmish::typ_check::check(
            &global_scope_types,
            &HashMap::new(),
            &type_definitions,
            def.body.clone()
        ))
        .map_err(|e| format!("In definition of {}: {}", def.name, e))?;
        let old_typ = global_scope_types.insert(def.name.clone(), typ.clone());
        assert!(old_typ.map_or(true, |o| o == typ));
    }

    let init_type = global_scope_types
        .get(&ident("init"))
        .ok_or("`init` value should be defined at top-level".to_string())?;

    let event_function_type = global_scope_types
        .get(&ident("event_handler"))
        .ok_or("`event_handler` function should be defined at top-level".to_string())?;

    // check event_handler
    if let Type::Function { args, return_type } = event_function_type {
        if args.len() != 2 {
            return Err(
                "`event_handler` function should take 2 arguments: game state and event"
                    .to_string(),
            );
        }
        let first_type = args
            .first()
            .ok_or("`event_handler` function should take 2 arguments: game state and event")?;
        let second_type = args
            .get(1)
            .ok_or("`event_handler` function should take 2 arguments: game state and event")?;
        if first_type != init_type {
            return Err(format!("First argument to `event` should be of the same type as `init`, got '{first_type}' and '{init_type}'."));
        }
        if *second_type != event_enum() {
            return Err(format!(
                "Second argument to `event_handler` should be of type Event, got {second_type}."
            ));
        }

        let ret_typ_err_msg =
            format!("Return type of 'event_handler' should be '< game = GameState; commands = List<Command>;>', got '{return_type}'");
        if let Type::Record(r) = return_type.borrow() {
            let game_state_type = r
                .get(&ident("game"))
                .ok_or_else(|| ret_typ_err_msg.clone())?;
            if game_state_type != init_type {
                return Err(ret_typ_err_msg);
            }
            let commands_type = r
                .get(&ident("commands"))
                .ok_or_else(|| ret_typ_err_msg.clone())?;
            if commands_type != &concrete_list_type(command_enum()) {
                return Err(ret_typ_err_msg);
            }
        } else {
            return Err(ret_typ_err_msg);
        }
    } else {
        return Err(format!(
            "`event_handler` should be a function, got '{event_function_type}'"
        ));
    }

    Ok(())
}

// < x = 5.0; y = 7.0; > -> pt2(5.0,7.0)
fn rec_to_vec2(record: vesmish::ast::Record) -> Vec2 {
    let x = record.get(&ident("x")).unwrap();
    let y = record.get(&ident("y")).unwrap();
    if let (vesmish::ast::Expr::Float(x), vesmish::ast::Expr::Float(y)) = (x.borrow(), y.borrow()) {
        pt2(*x as f32, *y as f32)
    } else {
        panic!("expected <x: Float; y: Float;>, got {record:?}");
    }
}

fn command_from_record(ev: &vesmish::ast::EnumVariant) -> Command {
    if ev.enu.0 == "Command" {
        match ev.variant.0.as_str() {
            "DrawLine" => {
                let body = ev
                    .body
                    .as_ref()
                    .expect("Expected DrawLine body to be present!");
                if let vesmish::ast::Expr::Record(ref record) = **body {
                    let start_rec = record.get(&ident("start")).unwrap();
                    let end_rec = record.get(&ident("end")).unwrap();
                    if let (
                        vesmish::ast::Expr::Record(start_rec),
                        vesmish::ast::Expr::Record(end_rec),
                    ) = (start_rec.borrow(), end_rec.borrow())
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
    global_scope: vesmish::eval::ScopeMap,
    game_state: vesmish::ast::RExpr,
    event_function: vesmish::ast::RExpr,
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
    let mut event_variants: HashMap<_, _> = ["KeyPressed", "KeyDown", "KeyReleased"]
        .map(|v| (ident(v), Some(keys_enum.clone())))
        .into();
    event_variants.insert(ident("Tick"), Some(float_type()));
    Type::Enum {
        enu: ident("Event"),
        variants: event_variants,
    }
}

fn vec2_type() -> Type {
    Type::Record([(ident("x"), float_type()), (ident("y"), float_type())].into())
}

fn command_enum() -> Type {
    let draw_line_body =
        Type::Record([(ident("start"), vec2_type()), (ident("end"), vec2_type())].into());
    Type::Enum {
        enu: ident("Command"),
        variants: [(ident("DrawLine"), Some(draw_line_body))].into(),
    }
}

fn extend_type_definitions(type_definitions: &mut HashMap<Ident, Type>) {
    type_definitions.insert(ident("Vec2"), vec2_type());
    type_definitions.insert(ident("Command"), command_enum());
    type_definitions.insert(ident("Event"), event_enum());
}

fn init_runtime(source_code: &SourceCode) -> Runtime {
    let contents = &source_code.code;

    let contents = vesmish::utils::wrap_in_span(&contents);
    let (input, defs) = vesmish::parse::top_definitions(contents).unwrap();

    if !input.is_empty() {
        panic!("parsing failed! input left:\n{input}");
    };

    let mut exprs = vec![];

    for def in defs.into_iter() {
        match def {
            vesmish::parse::TopLevelDefinition::Type(_) => {}
            vesmish::parse::TopLevelDefinition::Expr(e) => exprs.push(e),
        }
    }

    // 1st pass of "compilation"
    // without global scope
    let reduced_defs: Vec<vesmish::ast::Definition> = exprs
        .iter()
        .map(|d| vesmish::ast::Definition {
            body: vesmish::eval::beta_reduction(&HashMap::new(), &HashMap::new(), &d.body),
            name: d.name.clone(),
        })
        .collect();
    let mut global_scope = vesmish::utils::default_global_scope();
    global_scope.extend(vesmish::utils::map_from_defs(reduced_defs));

    let game_state = global_scope.get(&ident("init")).unwrap().clone();
    let event_function = if let vesmish::ast::Expr::Function(f) =
        global_scope.get(&ident("event_handler")).unwrap().borrow()
    {
        f.body.clone()
    } else {
        panic!("`event_handler` should have been a function.")
    };
    Runtime {
        global_scope,
        game_state,
        event_function,
    }
}

fn run_event_handler(
    global_scope: &HashMap<Ident, vesmish::ast::RExpr>,
    event_function: vesmish::ast::RExpr,
    game_state: vesmish::ast::RExpr,
    event: vesmish::ast::RExpr,
) -> (vesmish::ast::RExpr, Vec<Command>) {
    let mut local_scope = vesmish::eval::ScopeMap::new();
    local_scope.insert(ident("event"), event);
    local_scope.insert(ident("game"), game_state);

    let evaled = vesmish::eval::beta_reduction(global_scope, &local_scope, &event_function);

    extract_state_and_commands(evaled)
}

fn key_down(model: &mut Model, key: Key) {
    let key_str = format!("{key:?}");
    let key_enum = vesmish::utils::enum_variant("Key", &key_str, None);
    let event_enum = vesmish::utils::enum_variant("Event", "KeyDown", Some(key_enum));

    let (game_state, commands) = run_event_handler(
        &model.runtime.global_scope,
        model.runtime.event_function.clone(),
        model.runtime.game_state.clone(),
        event_enum,
    );

    model.runtime.game_state = game_state;
    model.commands.extend(commands);
}

fn key_pressed(app: &App, model: &mut Model, key: Key) {
    if model.pressed_keys.contains(&key) {
        // already pressed, nothing to do
        return;
    }
    model.pressed_keys.insert(key);
    let key_str = format!("{key:?}");
    let key_enum = vesmish::utils::enum_variant("Key", &key_str, None);
    let event_enum = vesmish::utils::enum_variant("Event", "KeyPressed", Some(key_enum));

    let (game_state, commands) = run_event_handler(
        &model.runtime.global_scope,
        model.runtime.event_function.clone(),
        model.runtime.game_state.clone(),
        event_enum,
    );
    model.runtime.game_state = game_state;
    model.commands.extend(commands);
}

fn key_released(app: &App, model: &mut Model, key: Key) {
    model.pressed_keys.remove(&key);
    let key_str = format!("{key:?}");
    let key_enum = vesmish::utils::enum_variant("Key", &key_str, None);
    let event_enum = vesmish::utils::enum_variant("Event", "KeyReleased", Some(key_enum));

    let (game_state, commands) = run_event_handler(
        &model.runtime.global_scope,
        model.runtime.event_function.clone(),
        model.runtime.game_state.clone(),
        event_enum,
    );
    model.runtime.game_state = game_state;
    model.commands.extend(commands);
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

fn extract_state_and_commands(result: vesmish::ast::RExpr) -> (vesmish::ast::RExpr, Vec<Command>) {
    if let vesmish::ast::Expr::Record(r) = result.borrow() {
        let game_state = r.get(&ident("game")).unwrap().clone();
        let commands =
            if let vesmish::ast::Expr::List(l) = r.get(&ident("commands")).unwrap().borrow() {
                let mut ret = vec![];
                for command in l {
                    if let vesmish::ast::Expr::EnumVariant(ev) = command.borrow() {
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
    let event = enum_variant(
        "Event",
        "Tick",
        Some(vesmish::ast::Expr::Float(delta).into()),
    );

    let (game_state, commands) = run_event_handler(
        &model.runtime.global_scope,
        model.runtime.event_function.clone(),
        model.runtime.game_state.clone(),
        event,
    );

    model.runtime.game_state = game_state;
    // TODO: does this lose key events?
    // if key event happens between update and view
    // we should check if nannou makes any guarentees about order
    // if it doesn't, we should maybe store commands_to_be_drawn separately
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
        .title("vesmor")
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
