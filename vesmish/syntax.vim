syn match @operator '[%`?&~\|\=\$\<\>\-+/\*@]'

syn match @punctuation.bracket '[\[\]\{\}\(\)\;:,.]'

syn match @variable.vesmish '[a-z]\w*'

syn match @type '[A-Z]\w*'

syn match @constant '[A-Z]\w*`'

syn match @number '\d\+'
syn match @number.float '\d\+\.\d\+'

syn match @string '"[^"]*"'
