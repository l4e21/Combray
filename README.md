# Combray

Yet Another Parser Combinator Library.

--- 

## Primitive Parsers

`pchar` parses a character
`pnumber` parses a number
`pinteger` parses an integer
`pbool` parses a bool
`puppercase` parses an uppercase letter
`plowercase` parses a lowercase letter
`pletter` parses any letter
`pcommasep` parses comma-separated values (like in json) 

## Parser Combinators

`pconcat` Parses and then concatenates the result into a list
`pchoice` Returns the first parser that passes, otherwise fails
`p*` Parses zero or more
`p+` Parses one or more
`pnoresult` Parses and then returns nil in the result
`pbetween` Takes 3 parsers, and returns the result of the second

## Writing Parsers

`prepare-string-for-parsing` turns a string into a state that is ready for parsing.

`with-state` anaphoric macro that provides a closure with a state variable for easy error railroading

`defparser` defines a function that puts your body into a `with-state`

`plet*` A macro that works like let* but each body of the binding is a parser-fn that is applied to the state subsequently, the body is what will be contained in the result (should all the parsers pass)

## Testing
`(asdf:test-system :combray)`


## TODO

- Add debugging middleware (add tags, add history)
- Add more primitives
- Add more combinators
- Provide more examples/docs
