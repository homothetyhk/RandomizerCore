The following symbols are reserved for use in infix logic:
- Left parens `(`, right parens `)`, plus `+`, and vertical bar `|` (used for basic operations)
- Less than `<`, equals `=`, and greater than `>` (used for comparison operations)
- Asterisk `*` (used to create reference tokens)
- Question mark `?` (used to create coallescing tokens)
- Forward slash `/` (used for projecting state providers to bool values)
- Additionally, `!` and `` ` `` are used in item strings
- Some symbols are not currently used, but are reserved for future use: `!`, `~`, `\`, `@`, `#`, `%`, `^`, `&`, `.`



The following symbols are safe to include in logic:
- Underscore `_`
- Hyphen `-`
- Square brackets `[` and `]`
- Curly brackets `{` and `}`
- Apostrophe `'`
- Comma `,`
- Semicolon `;`
- Colon `:`
- Dollar sign `$`

Also, it should be noted that leading and trailing white space is stripped from tokens when an infix expression is tokenized. Internal white space should be avoided.

Names which contain illegal symbols can be used to create tokens and logic in code, but will result in infix expressions which do not correctly deserialize. Thus, names of terms, macros, and logic variables should not use illegal symbols. Names of logic defs are encouraged to avoid illegal symbols, or else they will be unusable in reference tokens.