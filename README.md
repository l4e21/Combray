# Combray

Yet Another Parser Combinator Library.

--- 

## Main Functions

`prepare-string-for-parsing` turns a string into a state that is ready for parsing.

`with-state` anaphoric macro that provides a closure with a state variable for easy error railroading

`pchar` Parse a character

`plet*` Like let* but each body of the binding is a parser-fn that is applied to the state.

## Testing
`(asdf:test-system :combray)`
