import {
    get_code,
    set_output,
    reset_code,
    init as init_editor,
} from "./editor";
import {
    full_reload,
    hot_reload,
    check_code,
    init as init_game,
} from "./game_loader";

import "./style.scss";

function full_reload_() {
    let source = get_code();
    if (source != undefined && source != null) {
        full_reload(source).then(set_output);
    }
}

function check_source_code() {
    let source = get_code();
    if (source) {
        check_code(source).then((result) => {
            set_output(result);
        });
    }
}

function show_manual() {
    const manual_dialog = document.getElementById("manual-dialog");
    if (manual_dialog) {
        if (manual_dialog instanceof HTMLDialogElement) {
            manual_dialog.showModal();
        } else {
            throw "manual_dialog not a dialog";
        }
    } else {
        throw "No manual_dialog";
    }
}

function init() {
    const full_reload_button = document.getElementById("full_reload");
    if (full_reload_button) {
        full_reload_button.addEventListener("click", full_reload_);
    } else {
        throw "No full_reload_button";
    }

    const hot_reload_button = document.getElementById("hot_reload");
    if (hot_reload_button) {
        hot_reload_button.addEventListener("click", () => {
            let source = get_code();
            if (source != undefined && source != null) {
                hot_reload(source).then(set_output);
            }
        });
    } else {
        throw "No hot_reload_button";
    }

    const reset_code_button = document.getElementById("reset_code");
    if (reset_code_button) {
        reset_code_button.addEventListener("click", () => {
            reset_code("game.vvc").then(full_reload_);
        });
    } else {
        throw "No reset_code_button";
    }

    const check_source_code_button =
        document.getElementById("check_source_code");
    if (check_source_code_button) {
        check_source_code_button.addEventListener("click", check_source_code);
    } else {
        throw "No check_source_code_button";
    }

    const show_manual_button = document.getElementById("show_manual");
    if (show_manual_button) {
        show_manual_button.addEventListener("click", show_manual);
    } else {
        throw "No show_manual_button";
    }

    init_game();
    init_editor();
    reset_code("game.vvc");
}

setTimeout(init, 0);
