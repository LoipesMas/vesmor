use async_std::task::block_on;
use wasm_bindgen::prelude::wasm_bindgen;

use sketch::{run_app, SOURCE_CODE};

mod sketch;

// web app entry_point
#[wasm_bindgen]
pub async fn main_web() {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    block_on(async {
        run_app().await;
    });
}

#[wasm_bindgen]
pub async fn update_source_code(code: &str, hot_reload: bool) -> String {
    let code = code.replace(['\n', ' '], "");
    let code = vvcl::utils::wrap_in_span(&code);
    match vvcl::parse::top_definitions(code) {
        Ok((input, _defs)) => {
            if input.is_empty() {
                let ret = "Parsing successful!".to_owned();
                SOURCE_CODE.with_borrow_mut(|sc| {
                    sc.code = code.to_string();
                    sc.acknowledged = false;
                    sc.hot_reload = hot_reload;
                });
                ret
            } else {
                format!("Parsing failed, input left: {input}")
            }
        }
        Err(x) => format!("Parsing failed: {x}"),
    }
}
