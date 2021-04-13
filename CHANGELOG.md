# CHANGELOG

## Pre-1.0 versions

### Version 0.5.0 (April 13th, 2021)

* Introduce atomic rules

### Version 0.4.2 (April 13th, 2021)

* Detect unused rules

### Version 0.4.1 (April 13th, 2021)

* Rename the `RuleContent` type to `Rule`
* Add declaration location and name to the `Rule` type

### Version 0.4.0 (April 13th, 2021)

* Rename:
  * `Patterns` to `Rules`
  * `Pieces` to `Patterns`
  * `Sub-pieces` to `Pieces`
  * So now we have rules being made of patterns, and themselves of pieces

### Version 0.3.0 (April 12th, 2021)

* Introduce the [parser generator](peggy_derive/)

### Version 0.2.2 (April 12th, 2021)

* Make the `GRAMMAR_ENTRYPOINT_PATTERN` constant public
* Remove unstable builtin patterns `B_EOI`, `B_TRUE` and `B_FALSE`

### Version 0.2.1 (April 12th, 2021)

* Change visibility of generators

### Version 0.2.0 (April 12th, 2021)

* Add support for silent captures (`_:`)

### Version 0.1.0 (April 9th, 2021)

* Complete parser with support for:
  * Raw strings
  * Nested groups
  * Suites
  * Unions
  * Repetition operators: `*` `+` `?`
  * Single-line comments
  * Multi-line comments
* Complete runtime engine
* Complete Peggy regenerator (generate a text-based grammar from a parsed grammar)
* Complete human-readable error reporting
* Helping tips displaying in errors
* Ability to set unions to eager or lazy
* Support for external patterns
* Support for the following builtin patterns:
  * `B_ANY`
  * `B_EOI`
  * `B_TRUE`
  * `B_FALSE`
  * `B_ASCII`
  * `B_ASCII_ALPHABETIC`
  * `B_ASCII_ALPHANUMERIC`
  * `B_ASCII_CONTROL`
  * `B_ASCII_DIGIT`
  * `B_ASCII_GRAPHIC`
  * `B_ASCII_HEXDIGIT`
  * `B_ASCII_LOWERCASE`
  * `B_ASCII_PUNCTUATION`
  * `B_ASCII_UPPERCASE`
  * `B_ASCII_WHITESPACE`
  * `B_CONTROL`
  * `B_LOWERCASE`
  * `B_NUMERIC`
  * `B_UPPERCASE`
  * `B_WHITESPACE`
