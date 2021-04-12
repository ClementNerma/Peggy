//! # Peggy's Runtime Engine
//!
//! This module contains a small and fast [runtime](executor::execute) to executed [already-parsed grammars](crate::compiler::parse_peg).

mod builtin;
mod data;
mod errors;
mod executor;

pub use builtin::*;
pub use data::*;
pub use errors::*;
pub use executor::*;
