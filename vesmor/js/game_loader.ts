type Game = {
    main_web(): Promise<void>;
    update_source_code(code: string, hot_reload: boolean): Promise<string>;
    check_code(code: string): Promise<string>;
};
let game: Game | null = null;

const error_msg = "no game :(";
export async function full_reload(source: string): Promise<string> {
    if (game) {
        return game.update_source_code(source, false);
    } else {
        return error_msg;
    }
}
export async function hot_reload(source: string): Promise<string> {
    if (game) {
        return game.update_source_code(source, true);
    } else {
        return error_msg;
    }
}
export async function check_code(source: string): Promise<string> {
    if (game) {
        return game.check_code(source);
    } else {
        return error_msg;
    }
}

export async function init() {
    game = await import("../pkg/index.js");
    await game.main_web();
    let childrens = document.body.children;
    // canvas is inserted as last element
    let canvas = childrens.item(childrens.length - 1);
    if (canvas === null) {
        throw "no canvas";
    }

    let game_container = document.getElementById("game-container");
    if (game_container === null) {
        throw "no game-container";
    }

    game_container.replaceChildren(canvas);
}
