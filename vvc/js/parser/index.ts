import { parser } from "./vvcl.js";
import {
    LRLanguage,
    LanguageSupport,
    indentNodeProp,
    foldNodeProp,
    foldInside,
    delimitedIndent,
} from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";

console.log(parser.parse("min = foo(bar, b);").toString());

export const vvclLanguage = LRLanguage.define({
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

export function vvcl() {
    return new LanguageSupport(vvclLanguage);
}
