
type Game = {
    main_web(): Promise<void>;
    update_source_code(code: string, hot_reload: boolean): Promise<string>;
    check_code(code: string): Promise<string>;
};
let game: Game | null = null;
export async function full_reload(source: string): Promise<string> {
    if (game) {
    return game.update_source_code(source, false);
    } else {
        return ""
    }
}
export async function hot_reload(source: string): Promise<string> {
    if (game) {
    return game.update_source_code(source, true);
    } else {
        return ""
    }
}
export async function check_code(source: string): Promise<string> {
    if (game) {
    return game.check_code(source);
    } else {
        return ""
    }
}

export async function init() {
    game = await import("../pkg/index.js");
    game.main_web()
}
