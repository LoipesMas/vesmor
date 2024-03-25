syn match @operator '[\=\<\>\-+/\*@]'

syn match @punctuation.bracket '[\[\]\{\}\(\)\;:,.]'

syn match @variable.vvc '[a-z]\w*'

syn match @type '[A-Z]\w*'

syn match @number '\d\+'
syn match @number.float '\d\+\.\d\+'
