# CHANGELOG

## Pre-1.0 versions

### Version 0.5.4 (April 13th, 2021)

* **Minor:** Added `B_ALPHABETIC` and `B_ALPHANUMERIC` builtin rules

### Version 0.5.3 (April 13th, 2021)

* **BREAKING:** Store absolute position instead of relative in patterns

### Version 0.5.2 (April 13th, 2021)

* **BREAKING:** Rename error variants
* **QoF:** Improve some error messages

### Version 0.5.1 (April 13th, 2021)

* **BREAKING:** Forbid empty strings in patterns
* **BREAKING:** Forbid potentially-empty union members (which would lead to infinite loops)
* **Minor:** Add length in patterns
* **Fix:** wrong column displayed in patterns declaration

### Version 0.5.0 (April 13th, 2021)

* **MAJOR:** Introduce atomic rules
* **Fix:** Various bug fixes

### Version 0.4.2 (April 13th, 2021)

* **BREAKING:** Detect unused rules

### Version 0.4.1 (April 13th, 2021)

* **BREAKING:** Rename the `RuleContent` type to `Rule`
* **Minor:** Add declaration location and name to the `Rule` type

### Version 0.4.0 (April 13th, 2021)

* **BREAKING:** Rename:
  * `Patterns` to `Rules`
  * `Pieces` to `Patterns`
  * `Sub-pieces` to `Pieces`
  * So now we have rules being made of patterns, and themselves of pieces
* **BREAKING:** Types have been renamed accordingly

### Version 0.3.0 (April 12th, 2021)

* **MAJOR:** Introduce the [parser generator](peggy_derive/)

### Version 0.2.2 (April 12th, 2021)

* **BREAKING:** Remove unstable builtin patterns `B_EOI`, `B_TRUE` and `B_FALSE`
* **Minor:** Make the `GRAMMAR_ENTRYPOINT_PATTERN` constant public

### Version 0.2.1 (April 12th, 2021)

* **Minor:** Change visibility of generators

### Version 0.2.0 (April 12th, 2021)

* **MAJOR:** Add support for silent patterns (`_:`)

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
