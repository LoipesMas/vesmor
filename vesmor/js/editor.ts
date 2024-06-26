import { basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";
import { EditorView, keymap } from "@codemirror/view";
import { vesmish } from "./parser/index";

import { tags } from "@lezer/highlight";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { vim } from "@replit/codemirror-vim";

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
      height: "75vh",
      backgroundColor: "#001524",
      fontSize: "larger",
      color: "#ff7d00",
      fontWeight: "bold",
      border: "1px solid #ff7d00",
      padding: "0.5rem",
      overflow: "auto",
      resize: "both",
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

function state_from_code(code: string, vim_mode: boolean): EditorState {
  return EditorState.create({
    doc: code,
    extensions: (vim_mode ? [vim()] : []).concat([
      basicSetup,
      // keymap.of(defaultKeymap),
      vesmish(),
      theme,
      syntaxHighlighting(highlights),
      keymap.of(keymaps),
    ]),
  });
}

const STORAGE_KEY = "source_code";

const PLACEHOLDER_CODE = "Loading...";

export const DEFAULT_CODE_LOCATION = "pong.ves";

const saved_code = localStorage.getItem(STORAGE_KEY);

export const config = { vim_mode: false };

let startState = state_from_code(
  saved_code || PLACEHOLDER_CODE,
  config.vim_mode,
);

let output_el: HTMLElement | null = null;

export let view: EditorView | null = null;

function save() {
  const code = get_code();
  if (code) {
    localStorage.setItem(STORAGE_KEY, code);
  }
}

setInterval(save, 3000);

export function init() {
  let el = document.getElementById("codemirror-holder");
  output_el = document.getElementById("output");
  if (el !== null) {
    view = new EditorView({
      state: startState,
      parent: el,
    });
  }
  if (saved_code === null) {
    load_code(DEFAULT_CODE_LOCATION);
  }
  return view;
}

export function set_vim_mode(vim_mode: boolean) {
  config.vim_mode = vim_mode;
  const code = get_code() || "";
  set_code(code);
}

export function get_code(): string | undefined {
  return view?.state.doc.toString();
}
export function set_code(code: string) {
  if (view) {
    view.setState(state_from_code(code, config.vim_mode));
    const t = view.state.update({
      effects: [EditorView.scrollIntoView(0)],
    });
    view.update([t]);
  }
}

export function set_output(output: string) {
  if (output_el) {
    output_el.innerText = output;
  }
}

export async function load_code(source: string): Promise<boolean> {
  const resp = await fetch(source);
  if (resp.status === 200) {
    const code = await resp.text();
    set_code(code);
    return true;
  } else {
    set_output("Failed to load game code :c " + resp.status);
    return false;
  }
}
