# CHANGELOG

## Pre-1.0 versions

### Version 0.6.x (April 13th, 2021)

* **MAJOR:** Introduce negative (`!`) patterns
* **MAJOR:** Introduce peek (`~`) patterns

* **BREAKING:** Change silent patterns syntax from `_:` to just `°`
* **BREAKING:** Change atomic patterns syntax from `@:` to just `@`
* **BREAKING:** Reordered the parser's type into a new `compiler::data` module

* **Minor:** Return correct error from repeated pattern at end of input
* **Minor:** Return matched content when matching inside a negative pattern
* **Minor:** Add the rule's name inside generated parsers' error values
* **Minor:** Make the generated parsers' error values displayable
* **Minor:** Attached invalid character (if any) when failing to match against a builtin rule
* **Minor:** Add methods to map generated unions' variants in generated parsers

* **QoF:** Speed up checking of recursive patterns when generating a parser

* **Fix:** Character comparisons in generated builtin patterns
* **Fix:** Some unused rules weren't reported as unused
* **Fix:** Correctly handle all Unicode characters
* **Fix:** Incorrect string slicing in generated parsers
* **Fix:** Infinite loop while checking some recursive patterns

### Version 0.5.x (April 13th, 2021)

* **MAJOR:** Introduce atomic patterns

* **BREAKING:** Forbid empty strings in patterns
* **BREAKING:** Forbid potentially-empty union members (which would lead to infinite loops)
* **BREAKING:** Store absolute position instead of relative in patterns
* **BREAKING:** Rename error variants

* **QoF:** Improve some error messages
* **QoF:** Speed up validation of union members

* **Minor:** Add length in patterns
* **Minor:** Added `B_ALPHABETIC` and `B_ALPHANUMERIC` builtin rules

* **Fix:** wrong column displayed in patterns declaration
* **Fix:** Various bug fixes
* **Fix:** Stack overflow when validating recursive patterns

### Version 0.4.x (April 13th, 2021)

* **BREAKING:** Rename:
  * `Patterns` to `Rules`
  * `Pieces` to `Patterns`
  * `Sub-pieces` to `Pieces`
  * So now we have rules being made of patterns, and themselves of pieces
* **BREAKING:** Rename the `RuleContent` type to `Rule`
* **BREAKING:** Types have been renamed accordingly
* **BREAKING:** Detect unused rules

* **Minor:** Add declaration location and name to the `Rule` type

### Version 0.3.0 (April 12th, 2021)

* **MAJOR:** Introduce the [parser generator](peggy_derive/)

### Version 0.2.x (April 12th, 2021)

* **MAJOR:** Introduce silent patterns

* **BREAKING:** Remove unstable builtin patterns `B_EOI`, `B_TRUE` and `B_FALSE`

* **Minor:** Make the `GRAMMAR_ENTRYPOINT_PATTERN` constant public
* **Minor:** Change visibility of generators

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
