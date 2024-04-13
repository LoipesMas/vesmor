import * as game from "../pkg/index.js";
game.main_web();
export async function full_reload(source: string): Promise<string> {
    return game.update_source_code(source, false);
}
export async function hot_reload(source: string): Promise<string> {
    return game.update_source_code(source, true);
}
export async function check_code(source: string): Promise<string> {
    return game.check_code(source);
}
