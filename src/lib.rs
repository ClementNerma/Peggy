//! # Peggy
//!
//! Peggy is a small parser generator based on PEG syntaxes. It is heavily inspired by [Pest](https://github.com/pest-parser/pest).
//!
//! It provides functions to [parse text-based grammars](`compiler::parse_peg`) as well as a quick [runtime](`runtime::execute`).
//!
//! To get the best possible performances, allocations are reduced to the string minimum, especially on the heap's side.  
//! All string manipulations are performed using slices and lifetimes, except for error reporting which doesn't require as much performances.
//!
//! ## Usage
//!
//! ```rust
//! use peggy::compiler::{parse_peg, pretty_format_parser_err};
//! use peggy::runtime::{execute, RuntimeContext, RuntimeOptions};
//!
//! // 1. Define the grammar
//! static GRAMMAR: &str = r#"
//! s = B_WHITESPACE+
//!
//! hello = "Hello"
//! world = "world"
//! excl  = "!"
//!
//! main = hello s world (s? excl)*
//! "#;
//!
//! // 2. Compile the grammar
//! let grammar = parse_peg(GRAMMAR)
//!     .unwrap_or_else(|err| panic!("{}", pretty_format_parser_err(GRAMMAR, err)));
//!
//! // 3. Define the subject to use the compiled grammar on
//! let subject = "Hello world !!";
//!
//! // 4. Execute using the provided context
//! let parsed = execute(&RuntimeContext {
//!     grammar: &grammar,
//!     subject,
//!     external_patterns: None,
//!     options: RuntimeOptions::new(),
//! })
//! .unwrap_or_else(|err| panic!("{}", err));
//!
//! // 5. Play with the parsed content!
//! ```

#![forbid(unsafe_code)]
#![forbid(unused_must_use)]

pub mod compiler;
pub mod generators;
pub mod runtime;
