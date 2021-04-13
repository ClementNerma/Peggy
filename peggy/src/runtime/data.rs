use std::rc::Rc;

/// Data matched against a rule
#[derive(Debug)]
pub struct MatchedRule<'a> {
    /// Name of the matched rule
    pub(super) name: &'a str,

    /// Data captured in the rule
    /// Will be `None` if the rule is silent
    pub(super) data: MatchedData<'a>,
}

impl<'a> MatchedRule<'a> {
    /// Get a reference to matched rule's name
    pub fn name(&self) -> &'a str {
        self.name
    }

    /// Get a reference to matched rule's data
    /// Will be `None` if the rule is silent
    pub fn data(&self) -> &MatchedData<'a> {
        &self.data
    }
}

/// Matched data (used in [`MatchedRule`])
#[derive(Debug)]
pub enum MatchedData<'a> {
    /// Matched a constant string
    CstString(&'a str),

    /// Matched against another rule
    Rule(Rc<MatchedRule<'a>>),

    /// Matched a suite of elements
    SuiteOf(Vec<MatchedData<'a>>),

    /// Matched an optional pattern
    /// The Rc's inner value will be `None` if it's non-capturing
    OptionalPattern(Option<Rc<Option<MatchedData<'a>>>>),

    /// Matched repeated data from a single pattern
    RepeatedPattern(Vec<MatchedData<'a>>),

    /// Matched an atomic pattern
    AtomicPattern(&'a str),

    /// Matched a silent pattern
    SilentPattern,

    /// Matched a builtin rule
    BuiltinRule { name: &'a str, symbol: Option<char> },

    /// Matched an external rule
    ExternalRule { name: &'a str, matched: &'a str },
}
