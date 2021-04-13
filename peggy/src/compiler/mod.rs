//! # Peggy's Compiler
//!
//! This module contains the compiler, which turns Peggy grammars to syntax trees.
//!
//! These can then be used either with the [generators](`crate::generators`), or with the [built-in runtime](`crate::runtime`).

pub mod data;
mod errors;
mod parser;
mod report;
mod singles;
pub(crate) mod utils;
mod validator;

pub use data::*;
pub use errors::*;
pub use parser::*;
pub use report::*;
pub use validator::*;
