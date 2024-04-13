import { full_reload, hot_reload, check_code } from "./game_loader.ts";
import { get_code, set_output, reset_code } from "./editor.ts";

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

reset_code("game.vvc");
