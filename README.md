# Peggy's Parser

`peggy` is a small and efficient tool to parse and execute PEG-based grammars.

It can parse nested grammars and supports repetition operators. Error reporting is tailored to be as intuitive and readable as possible.

As a middle term-goal, I plan to make this a parser generator to get native Rust code from the grammar files, which would allow for easier output handling and faster performances with large grammars and input texts.

## Usage

## RPN example

A Reverse Polish Notation (RPN) grammar may look like this:

```
S = B_WHITESPACE                            # Whitespace
DEC_SEP = "." | ","                         # Decimal separator

int = B_ASCII_DIGIT+                        # Integer
float = int DEC_SEP int                     # Floating-point number
number = int | float                        # Number

operator = "+" | "-" | "*" | "/"            # Operator
operand = number | paren_expr               # Operand
operation = operand S+ operand S* operator  # Complete operation

paren_expr = ("(" S* expr S* ")")           # Expression wrapped between parenthesis
expr = number | operation | paren_expr      # Complete expression

main = expr                                 # Grammar's entrypoint
```

This will be able to match complex operations like `(3 (9.3 3 /) +) (5 (2 3 /) /) /`.

## Performances

On my computer (Intel Core i7-9700F), in release mode the grammar is parsed in 16 microseconds (0.016 milliseconds) while the parsing takes about 128 microseconds (0.128 milliseconds).

As you can guess, these increases linearly with the size of the inputs, which can lead to a time of multiple seconds if you parse tens of thousands of kilobytes.

This is one of the main motivations for the future parser generator feature.

## Elegant error reporting

A simple grammar like the one shown in [`src/lib.rs`](src/lib.rs) will give the following error message:

```
ERROR: At line 1, column 7:

1 | Hello worlf !!
          ^

In pattern [world]: Expected string literal "world"
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
* A pattern declaration

Pattern declarations start with the pattern's name, which must respect the following rules:

* Only alphanumeric and underscores are allowed
* The name cannot start with a digit
* The name cannot start with `B_` as this is reserved for builtin patterns
* The name cannot start with `E_` as this is reserved for external patterns
* They must contain at least one character
* Two patterns cannot have the same name

They continue with the assignment operator (`=`) and the pattern's content, which is made of a _piece_. Pieces can either be:

* A fixed string, between double quotes - there is no escaping machnism, newline symbols and double quotes can be matched using [builtin patterns](#builtin-patterns)
* Another pattern's name (the provided pattern will be used for matching)
* A group (a piece wrapped between parenthesis)
* A list of pieces separated by whitespaces (all pieces will need to match the input)
* An union of pieces separated by vertical bars `|` (at least one the piece will need to match the input)

Pieces can be decorated with a _repetition model_ (no whitespace must be present between the end of the piece and the model). It can either be:

* `+`: match this piece as much as possible, but at least once
* `*`: match this piece as much as possible, zero matching is allowed
* `?`: match this piece one time if possible, zero matching is allowed

Pieces can also be made _silent_ to avoid capturing anything, by prefixing them with `_:` (no space allowed).

## Builtin patterns

There are multiple builtin patterns, which will only match at most one single character:

| Pattern's name         | Description                                     |
| ---------------------- | ----------------------------------------------- |
| `B_EOI`                | Only if the input is finished                   |
| `B_TRUE`               | Always, even if there is no available character |
| `B_FALSE`              | Never                                           |
| `B_ANY`                | Any character                                   |
| `B_NEWLINE_CR`         | Match `\r` newline characters                   |
| `B_NEWLINE_LF`         | Match `\n` newline characters                   |
| `B_DOUBLE_QUOTE`       | Match a double quote                            |
| `B_ASCII`              | ASCII characters                                |
| `B_ASCII_ALPHABETIC`   | ASCII alphabetic characters                     |
| `B_ASCII_ALPHANUMERIC` | ASCII alphanumeric characters                   |
| `B_ASCII_CONTROL`      | ASCII control characters                        |
| `B_ASCII_DIGIT`        | ASCII digits                                    |
| `B_ASCII_GRAPHIC`      | ASCII graphic characters                        |
| `B_ASCII_HEXDIGIT`     | ASCII hexidecimal digits                        |
| `B_ASCII_LOWERCASE`    | ASCII lowercase characters                      |
| `B_ASCII_PUNCTUATION`  | ASCII punctuation characters                    |
| `B_ASCII_UPPERCASE`    | ASCII uppercase characters                      |
| `B_ASCII_WHITESPACE`   | ASCII whitespaces                               |
| `B_CONTROL`            | Unicode control characters                      |
| `B_LOWERCASE`          | Unicode lowercase characters                    |
| `B_NUMERIC`            | Unicode numeric characters                      |
| `B_UPPERCASE`          | Unicode uppercase characters                    |
| `B_WHITESPACE`         | Unicode whitespaces                             |

## External characters

A callback can be provided to the execution engine to handle external patterns, which are prefixed with `E_`. See the documentation for more informations.

## License

This project is released under the [Apache-2.0](LICENSE.md) license terms.
