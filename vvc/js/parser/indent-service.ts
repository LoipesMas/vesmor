import { indentService } from '@codemirror/language'

export const vvclIndentService = () => {
    return indentService.of((indentContext, pos) => {
        const previousLine = indentContext.state.doc.lineAt(pos)
        const whitespace = previousLine.text.match(/^\s*/)
        if (whitespace) {
            return whitespace[0].length
        }
        return null
    })
}
