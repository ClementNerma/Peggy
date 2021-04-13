use super::parser::ParserLoc;
use super::utils::{add_parser_loc, BUILTIN_RULES};
use std::fmt;

/// Add base location to an error instance.
///
/// If the error's content contains a location value, it will be updated as well.
pub fn add_base_err_loc(base_line: usize, base_col: usize, err: ParserError) -> ParserError {
    ParserError {
        loc: add_parser_loc(base_line, base_col, err.loc),
        length: err.length,
        content: match err.content {
            ParserErrorContent::UnclosedGroup { started_at } => ParserErrorContent::UnclosedGroup {
                started_at: add_parser_loc(base_line, base_col, started_at),
            },
            ParserErrorContent::UnterminatedCstString { started_at } => {
                ParserErrorContent::UnterminatedCstString {
                    started_at: add_parser_loc(base_line, base_col, started_at),
                }
            }
            ParserErrorContent::ExpectedRuleAssignmentOp
            | ParserErrorContent::ExpectedPattern
            | ParserErrorContent::ExpectedRuleDeclaration
            | ParserErrorContent::ExpectedPatternSeparatorOrEndOfLine
            | ParserErrorContent::ExpectedFollowContinuation
            | ParserErrorContent::ExpectedUnionContinuation
            | ParserErrorContent::ReservedUppercaseRuleName
            | ParserErrorContent::DuplicateRuleName
            | ParserErrorContent::MissingMainRule
            | ParserErrorContent::UnknownRule
            | ParserErrorContent::UnknownBuiltinRule
            | ParserErrorContent::UnusedRule
            | ParserErrorContent::EmptyConstantString
            | ParserErrorContent::PotentiallyEmptyUnionMember
            | ParserErrorContent::UnterminatedMultiLineComment { started_at: _ } // As this one is global, it does not adaptation
            | ParserErrorContent::IllegalSymbol(_) => err.content,
        },
        tip: err.tip,
    }
}

/// Global parsing error
#[derive(Debug)]
pub struct ParserError {
    loc: ParserLoc,
    length: usize,
    content: ParserErrorContent,
    tip: Option<&'static str>,
}

impl ParserError {
    /// Create a new parsing error
    pub(crate) fn new(
        loc: ParserLoc,
        length: usize,
        content: ParserErrorContent,
        tip: Option<&'static str>,
    ) -> Self {
        Self {
            loc,
            length,
            content,
            tip,
        }
    }

    /// Get the location of an error
    pub fn loc(&self) -> &ParserLoc {
        &self.loc
    }

    /// Get the line number of an error
    pub fn line(&self) -> usize {
        self.loc.line()
    }

    /// Get the column number of an error
    pub fn col(&self) -> usize {
        self.loc.col()
    }

    /// Get the input length the error applies on
    pub fn length(&self) -> usize {
        self.length
    }

    /// Get the error's content
    pub fn content(&self) -> &ParserErrorContent {
        &self.content
    }

    /// Get the optional error's tip
    pub fn tip(&self) -> Option<&'static str> {
        self.tip
    }
}

/// Content of a [`ParserError`]
#[derive(Debug)]
pub enum ParserErrorContent {
    ExpectedRuleDeclaration,
    IllegalSymbol(char),
    ExpectedRuleAssignmentOp,
    ReservedUppercaseRuleName,
    DuplicateRuleName,
    ExpectedPattern,
    UnclosedGroup { started_at: ParserLoc },
    ExpectedPatternSeparatorOrEndOfLine,
    ExpectedFollowContinuation,
    ExpectedUnionContinuation,
    UnterminatedCstString { started_at: ParserLoc },
    UnknownRule,
    UnknownBuiltinRule,
    UnterminatedMultiLineComment { started_at: ParserLoc },
    MissingMainRule,
    UnusedRule,
    EmptyConstantString,
    PotentiallyEmptyUnionMember,
}

impl fmt::Display for ParserErrorContent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExpectedRuleDeclaration => write!(f, "Expected rule declaration"),
            Self::IllegalSymbol(s) => write!(f, "Illegal symbol '{}'", s),
            Self::ExpectedRuleAssignmentOp => {
                write!(f, "Expected rule assignment operator (=)")
            }
            Self::ReservedUppercaseRuleName => {
                write!(
                    f,
                    "All-uppercase rule names are reserved for external rules"
                )
            }
            Self::DuplicateRuleName => {
                write!(f, "Another rule was already declared with this name")
            }
            Self::ExpectedPattern => write!(f, "Expected a pattern"),
            Self::UnclosedGroup { started_at } => write!(
                f,
                "Unclosed group starting at line {}, column {}",
                started_at.line() + 1,
                started_at.col() + 1
            ),
            Self::ExpectedPatternSeparatorOrEndOfLine => {
                write!(f, "Expected rule separator or end of line")
            }
            Self::ExpectedFollowContinuation => write!(
                f,
                "Expected continuation of list of rule patterns (white space)"
            ),
            Self::ExpectedUnionContinuation => {
                write!(f, "Expected continuation of rule patterns union (|)")
            }
            Self::UnterminatedCstString { started_at } => write!(
                f,
                "Unterminated constant string starting at line {}, column {}",
                started_at.line() + 1,
                started_at.col() + 1
            ),
            Self::UnknownRule => write!(f, "Unknown rule"),
            Self::UnknownBuiltinRule => write!(
                f,
                "Unknown builtin rule. Here is the list of all available ones:{}",
                BUILTIN_RULES
                    .iter()
                    .map(|candidate| format!("\n  * {}", candidate))
                    .collect::<String>()
            ),
            Self::UnterminatedMultiLineComment { started_at } => write!(
                f,
                "Unterminated multi-line comment opened at line {}, column {}",
                started_at.line() + 1,
                started_at.col() + 1
            ),
            Self::MissingMainRule => write!(f, "Main rule is missing"),
            Self::UnusedRule => write!(f, "This rule is declared but never used"),
            Self::EmptyConstantString => write!(
                f,
                "Empty constant strings are forbidden as they match nothing"
            ),
            Self::PotentiallyEmptyUnionMember => {
                write!(f, "Detected potentially-empty union member")
            }
        }
    }
}
