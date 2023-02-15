# Combray

Yet Another Parser Combinator Library.

--- 

## Main Functions

`prepare-string-for-parsing` turns a string into a state that is ready for parsing.

`with-state` anaphoric macro that provides a closure with a state variable for easy error railroading

`pchar` Parse a character

`pcharexcept` Parse any character except the character

`pmany` Parse in serial

`pchoice` Either/or parser

`poptional` Parse optionally 

`pstring` Attempt to parse a specific string
`p+` Parse one or more of

`p*` Parse zero or more of

## Testing
`(asdf:test-system :combray)`
