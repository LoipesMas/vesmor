import { basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { vvcl } from "./parser/index";

import { tags } from "@lezer/highlight";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";

const highlights = HighlightStyle.define([
  { tag: tags.typeName, color: "#c96abc" },
  { tag: tags.operatorKeyword, color: "#71bc8c" },
  { tag: tags.punctuation, color: "#9b9992" },
  { tag: tags.integer, color: "#4e92b7" },
]);

const keymaps = [
  {
    key: "Ctrl-r",
    run: (ev: EditorView) => {
      let selection = ev.state.selection.main;
      let current_pos = selection.from;
      console.log(current_pos);
      let t = ev.state.update(
        {
          changes: { from: current_pos, insert: "<  >" },
          sequential: true,
        },
        {
          selection: {
            anchor: current_pos + 2,
            head: current_pos + 2,
          },
          sequential: true,
        },
      );
      ev.update([t]);
      return true;
    },
    preventDefault: true,
  },
];

const theme = EditorView.theme(
  {
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
      caretColor: "#ff7d00",
    },
    "&.cm-focused .cm-cursor": {
      borderLeftColor: "#0e9",
    },
    "&.cm-focused .cm-selectionBackground, ::selection": {
      backgroundColor: "#074",
    },
    ".cm-gutters": {
      backgroundColor: "#045",
      color: "#ddd",
      border: "none",
    },
  },
  { dark: true },
);

function state_from_code(code: string): EditorState {
  return EditorState.create({
    doc: code,
    extensions: [
      basicSetup,
      // keymap.of(defaultKeymap),
      vvcl(),
      theme,
      syntaxHighlighting(highlights),
      keymap.of(keymaps),
    ],
  });
}

let startState = state_from_code("Loading...");

let output_el: HTMLElement | null = null;

export let view: EditorView | null = null;

export function init() {
  let el = document.getElementById("codemirror-holder");
  output_el = document.getElementById("output");
  if (el !== null) {
    view = new EditorView({
      state: startState,
      parent: el,
    });
  }
  return view;
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
