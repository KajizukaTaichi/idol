# idol
Goal-directed evaluation programming language inspired by Icon

## Features
There's I describe key featutes of idol programming language.

Expressions are surrounded with parentheses.
```
(1 + 2 * 3)
```

Statements are surrounded with braces if it's included in expressions.
```
{ cast "123" to number } + 5
```
Many statements returns value.

### If Statement

`if` is not simply conditional branch statement, it can error handling by the Goal-directed evaluation.
```
if { cast { input "> " } to number } (it * 2) else -1
```
It's easy to take condition value with use `it` keyword

### Function Define

Use `func` keyword to define functions
```
func { inc n } (n + 1)
```

### Call Function

User-defined functions are usable like other statement
```
inc { inc 5 }
```
