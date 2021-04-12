//! # Generators
//!
//! This module contains generators for [already-parsed grammars](`super::compiler::parse_peg`).

pub mod peggy;

pub use self::peggy::gen_peggy;
