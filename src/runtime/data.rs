use std::rc::Rc;

/// Data matched against a pattern
#[derive(Debug)]
pub struct MatchingPattern<'a> {
    pub(crate) name: &'a str,
    pub(crate) data: MatchedData<'a>,
}

impl<'a> MatchingPattern<'a> {
    /// Get a reference to matched pattern's name
    pub fn name(&self) -> &'a str {
        self.name
    }

    /// Get a reference to matched pattern's data
    pub fn data(&self) -> &MatchedData<'a> {
        &self.data
    }
}

/// Matched data (used in [`MatchingPattern`])
#[derive(Debug)]
pub enum MatchedData<'a> {
    /// Matched a constant string
    CstString(&'a str),

    /// Matched against another pattern
    Pattern(Rc<MatchingPattern<'a>>),

    /// Matched a suite of elements
    SuiteOf(Vec<MatchedData<'a>>),

    /// Matched an optional piece
    OptionalPiece(Option<Rc<MatchedData<'a>>>),

    /// Matched repeated data from a single piece
    RepeatedPiece(Vec<MatchedData<'a>>),

    /// Matched a builtin pattern
    BuiltinPattern { name: &'a str, symbol: Option<char> },

    /// Matched an external pattern
    ExternalPattern { name: &'a str, matched: &'a str },
}