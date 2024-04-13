import { basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { defaultKeymap } from "@codemirror/commands";
import { vvcl } from "./parser/index";

let theme = EditorView.theme({
  "&": {
    width: "70vh",
    height: "75vh !important",
    backgroundColor: "#001524",
    fontSize: "larger",
    color: "#ff7d00",
    fontWeight: "bold",
    border: "1px solid #ff7d00",
    padding: "0.5rem",
  },
  ".cm-content": {
    caretColor: "#ff7d00"
  },
  "&.cm-focused .cm-cursor": {
    borderLeftColor: "#0e9"
  },
  "&.cm-focused .cm-selectionBackground, ::selection": {
    backgroundColor: "#074"
  },
  ".cm-gutters": {
    backgroundColor: "#045",
    color: "#ddd",
    border: "none"
  }
}, {dark: true})

function state_from_code(code: string): EditorState {
    return EditorState.create({
        doc: code,
        extensions: [basicSetup, keymap.of(defaultKeymap), vvcl(), theme],
    });
}

let startState = state_from_code("Loading...");

let output_el = document.getElementById("output");

let el = document.getElementById("codemirror-holder");
export let view: EditorView | null = null;
if (el !== null) {
    view = new EditorView({
        state: startState,
        parent: el,
    });
}

export function get_code(): string | undefined {
    return view?.state.doc.toString();
}
export function set_code(code: string) {
    if (view) {
        view.setState(state_from_code(code));
    }
}

export function set_output(output: string) {
    if (output_el) {
        output_el.innerText = output;
    }
}

export async function reset_code(source: URL): Promise<void> {
    return fetch(source)
        .then((resp) => resp.text())
        .then(set_code);
}
