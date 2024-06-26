import { parser } from "./vesmish";
import { vesmishIndentService } from "./indent-service";
import {
  LRLanguage,
  LanguageSupport,
  indentNodeProp,
  foldNodeProp,
  foldInside,
  delimitedIndent,
} from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";

export const vesmishLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      indentNodeProp.add({
        Application: delimitedIndent({ closing: ")", align: false }),
      }),
      foldNodeProp.add({
        Application: foldInside,
      }),
      styleTags({
        Tick: t.typeName,
        TypeName: t.typeName,
        Number: t.integer,
        String: t.string,
        "( ) , : . ; [ ]": t.punctuation,
        FatArrow: t.operatorKeyword,
        "= { } -> ? | < > @ ~~ ~ + - -.": t.operatorKeyword,
      }),
    ],
  }),
});

export function vesmish() {
  return new LanguageSupport(vesmishLanguage, [vesmishIndentService()]);
}
