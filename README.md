# Peggy's Parser

`peggy` is a small and efficient parser generator based on PEG grammars.

It can parse nested grammars and supports repetition operators. Error reporting is tailored to be as intuitive and readable as possible.

There are two crates: [`peggy`](peggy/), which contains the library's base code as well as the grammar parser and the runtime engine, and [`peggy_derive/`](peggy_derive/) which is the parser generator.

## Examples

You can find several examples in the source directories for [`peggy`](peggy/examples) and [`peggy_derive`](peggy_derive/examples), notably:

* [`peggy/prn`](peggy/examples/prn.rs) - A Reverse Polish Notation (RPN) evaluator, using the runtime engine
* [`peggy_derive/prn`](peggy_derive/examples/prn.rs) - The same RPN evaluator but using a parser generator

## RPN example

A Reverse Polish Notation (RPN) grammar may look like this:

```
S = °B_WHITESPACE                           # Whitespace
DEC_SEP = °("." | ",")                      # Decimal separator

int = @(B_ASCII_DIGIT+)                     # Integer
float = int DEC_SEP int                     # Floating-point number
number = int | float                        # Number

operator = "+" | "-" | "*" | "/"            # Operator
operand = number | paren_expr               # Operand
operation = operand S+ operand S* operator  # Complete operation

paren_expr = °"(" S* expr S* °")"           # Expression wrapped between parenthesis
expr = number | operation | paren_expr      # Complete expression

main = expr                                 # Grammar's entrypoint
```

This will be able to match complex operations like `(3 (9.3 3 /) +) (5 (2 3 /) /) /`.

## Parser generator usage

Here is an example usage of the parser generator:

```rust
#[macro_use]
extern crate peggy_derive;

#[peggy_grammar(filename = "prn.peggy")]
pub mod prn_grammar {}

fn main() {
    // Evaluate the expression
    let success = prn_grammar::exec("(3 (9.3 3 /) +) (5 (2 3 /) /) /").unwrap();

    // Do your stuff
}
```

The main advantage of using parser generators is the better performances (~ 10 times than the optimized runtime engine), as well as the easier use and safety: you will directly extract the informations from your grammar without having to check unreachable statements. This also means that updating your grammar will instantly show what parts of your code needs to be updated.

The generated types are optimized to be as lightweight and easy-to-use as possible ; the only corner case being the use of the `Rc` type to store informations in recursive patterns.

The success type returned by `::exec` is generated depending on the input grammar ; if your IDE doesn't expand procedural macros and doesn't provide you informations about the generated types, you can take a look at the result's content by using the `dbg!()` macro (or `format!("{:#?}")` for formatting purposes).

All of the generated types implement the `Debug` and `Clone` traits.

## Performances

On my computer (Intel Core i7-9700F), in release mode the grammar is parsed in 16 microseconds (0.016 milliseconds) while the runtime engine takes about 128 microseconds (0.128 milliseconds).

As you can guess, these increases linearly with the size of the inputs, which can lead to a time of multiple seconds if you parse tens of thousands of kilobytes.

With the parser generator, we go down from 128 microseconds to only 6.5 (so 0.065 milliseconds).

## Elegant error reporting

A simple grammar like the one shown in [`peggy/src/lib.rs`](peggy/src/lib.rs) will give the following error message:

```
ERROR: At line 1, column 7:

1 | Hello worlf !!
          ^

In rule [world]: Expected string literal "world"
```

## Grammar specifications

General syntax rules:

* Comments start with `#` and go to the end of the current line. They can be put everywhere, although in strings they will be treated as a part of it.
* Lines can start and end with whitespaces, which will be ignored
* Whitespaces can be put everywhere unless stated otherwise

If a line is only made of three `#` symbols (optionally wrapped by whitespaces), it will open a multi-line comments. Every line will be ignored until another 3-`#`-only line is found.

The grammar is made of multiple lines, which can either be:

* Empty
* Made of whitespaces
* Empty or made of whitespaces AND of a comment
* A rule declaration

Rule declarations start with the rule's name, which must respect the following rules:

* Only alphanumeric and underscores are allowed
* The name cannot start with a digit
* The name cannot start with `B_` as this is reserved for builtin rules
* The name cannot start with `E_` as this is reserved for external rules
* They must contain at least one character
* Two rules cannot have the same name

They continue with the assignment operator (`=`) and the rule's content, which is made of a _pattern_. Patterns can either be:

* A fixed string, between double quotes - there is no escaping machnism, newline symbols and double quotes can be matched using [builtin rules](#builtin-rules)
* Another rule's name (the provided rule will be used for matching)
* A group (a pattern wrapped between parenthesis)
* A list of patterns separated by whitespaces (all patterns will need to match the input)
* An union of patterns separated by vertical bars `|` (at least one the pattern will need to match the input)

Patterns can be decorated with a _repetition model_ (no whitespace must be present between the end of the pattern and the model). It can either be:

* `+`: match this pattern as much as possible, but at least once
* `*`: match this pattern as much as possible, zero matching is allowed
* `?`: match this pattern one time if possible, zero matching is allowed

Patterns can also be set a _mode_ by prefixing them with a character (no space allowed):

* `°`: silent pattern - will not capture anything
* `~`: peek patterns - does not capture or consume anything
* `!`: negative pattern - will match only if the inner pattern doesn't ; does not capture or consume anything
* `@`: atomic patterns - will be returned as a single string if matching

Please note that, unlike any other feature, atomic patterns will add a lifetime to the success type to be able to store the input slice. This avoids any form of heap allocation, but will make a lifetime appear in all parent patterns (and so, forcibly in the global success type) if you suddenly introduce an atomic pattern. This shouldn't be a problem in most cases, but keep that in mind.

## Builtin rules

There are multiple builtin rules, which will only match at most one single character:

| Rule's name            | Description                     |
| ---------------------- | ------------------------------- |
| `B_ANY`                | Any character                   |
| `B_NEWLINE_CR`         | Match `\r` newline characters   |
| `B_NEWLINE_LF`         | Match `\n` newline characters   |
| `B_DOUBLE_QUOTE`       | Match a double quote            |
| `B_ASCII`              | ASCII characters                |
| `B_ASCII_ALPHABETIC`   | ASCII alphabetic characters     |
| `B_ASCII_ALPHANUMERIC` | ASCII alphanumeric characters   |
| `B_ASCII_CONTROL`      | ASCII control characters        |
| `B_ASCII_DIGIT`        | ASCII digits                    |
| `B_ASCII_GRAPHIC`      | ASCII graphic characters        |
| `B_ASCII_HEXDIGIT`     | ASCII hexidecimal digits        |
| `B_ASCII_LOWERCASE`    | ASCII lowercase characters      |
| `B_ASCII_PUNCTUATION`  | ASCII punctuation characters    |
| `B_ASCII_UPPERCASE`    | ASCII uppercase characters      |
| `B_ASCII_WHITESPACE`   | ASCII whitespaces               |
| `B_ALPHABETIC`         | Unicode alphabetic characters   |
| `B_ALPHANUMERIC`       | Unicode alphanumeric characters |
| `B_CONTROL`            | Unicode control characters      |
| `B_LOWERCASE`          | Unicode lowercase characters    |
| `B_NUMERIC`            | Unicode numeric characters      |
| `B_UPPERCASE`          | Unicode uppercase characters    |
| `B_WHITESPACE`         | Unicode whitespaces             |

## External characters

A callback can be provided to the execution engine to handle external rules, which are prefixed with `E_`. See the documentation for more informations.

## License

This project is released under the [Apache-2.0](LICENSE.md) license terms.
