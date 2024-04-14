import { get_code, set_output, reset_code, init as init_editor } from "./editor.ts";
import { full_reload, hot_reload, check_code, init as init_game } from "./game_loader.ts";

import "./style.scss";

window.full_reload = () => {
    let source = get_code();
    if (source != undefined && source != null) {
        full_reload(source).then(set_output);
    }
};
window.hot_reload = () => {
    let source = get_code();
    if (source !== undefined) {
        hot_reload(source).then(set_output);
    }
};
window.reset_code = () => {
    reset_code("game.vvc").then(window.full_reload);
};

function check_source_code() {
    let source = get_code();
    if (source) {
        check_code(source).then((result) => {
            set_output(result);
        });
    }
}
window.check_source_code = check_source_code;

setTimeout(() => {init_game().then(() => console.log("foo"))},0)
setTimeout(() => {init_editor(); reset_code("game.vvc")},0)
