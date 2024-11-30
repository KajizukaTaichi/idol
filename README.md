# idol

A goal-directed evaluation programming language inspired by Icon.

## Features

### Expression Syntax

In idol, expressions are surrounded with parentheses:
```idol
(1 + 2 * 3)
```
This keeps the syntax clean and allows for more consistent evaluation.

### Statement Syntax

Statements in idol are enclosed in braces, especially when they are part of an expression:
```idol
{ cast "123" to number } + 5
```
The key idea is that many statements **return values**, making them versatile and useful in various contexts.

---

### Goal-directed Evaluation

idol uses goal-directed evaluation, inspired by Icon. This approach makes error handling straightforward and intuitive.

For instance, you can handle errors in a compact form:
```idol
if { cast { input "> " } to number } (it * 2) else -1
```
In this example:
- The `input` is cast to a number.
- If successful, the result is multiplied by 2.
- If the input is invalid (e.g., not a number), the `else` block returns `-1`.

The `it` keyword is used to easily access the result of a successful evaluation.

---

### Function Definitions

Functions are defined using the `func` keyword, which makes it easy to define reusable blocks of logic:
```idol
func { inc n } (n + 1)
```
This defines a function called `inc` that takes `n` as input and returns `n + 1`.

---

### Function Calls

User-defined functions can be called just like any other statement:
```idol
inc { inc 5 }
```
This calls the `inc` function twice on the number `5`, resulting in `7`.

---

## Example Code

Here is a more complete example showcasing idol's syntax and features:

```idol
func ( fizzbuzz n ) {
    if (n % 15 == 0) "FizzBuzz"
    else {
        if (n % 3 == 0) "Fizz"
        else {
            if (n % 5 == 0) "Buzz"
            else { cast n to text }
        }
    }
};

let i = 1;
while (1 <= i <= 100) {
    print { fizzbuzz i } + new-line;
    let i = i + 1;
}
```

This simple FizzBuzz example demonstrates how to:
- Define a function (`fizzbuzz`).
- Use conditional logic inside a function.
- Use goal-directed evaluation for efficient error handling.
- Iterate through a range of numbers with `while`.

---

## Conclusion

idol is designed to be a simple yet powerful language that emphasizes readability, error handling, and expressive functions. Whether you're handling errors gracefully or defining concise functions, idol’s syntax and goal-directed evaluation provide a unique and enjoyable programming experience.

