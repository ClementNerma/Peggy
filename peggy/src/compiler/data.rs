use std::collections::HashMap;
use std::rc::Rc;

/// A grammar's entrypoint rule
pub const GRAMMAR_ENTRYPOINT_RULE: &str = "main";

/// Syntax tree generated by [the compiler](`parse_peg`)
#[derive(Debug)]
pub struct PegSyntaxTree<'a> {
    pub(super) rules: Rules<'a>,
}

impl<'a> PegSyntaxTree<'a> {
    /// Get the rules of the syntax tree (the map's keys are the rules' name)
    pub fn rules(&self) -> &Rules<'a> {
        &self.rules
    }

    /// Get the rule's main rule
    pub fn main_rule(&self) -> &Rule<'a> {
        &self.rules[GRAMMAR_ENTRYPOINT_RULE]
    }
}

/// Rules from a [`PegSyntaxTree`]
pub type Rules<'a> = HashMap<&'a str, Rule<'a>>;

/// A rule's content, parsed by the [`parse_peg`] function
#[derive(Debug)]
pub struct Rule<'a> {
    /// Rule's name
    pub(crate) name: &'a str,

    /// Inner pattern
    pub(crate) pattern: Pattern<'a>,

    /// Declaration location
    pub(crate) decl_loc: ParserLoc,
}

impl<'a> Rule<'a> {
    pub fn name(&self) -> &'a str {
        &self.name
    }

    pub fn pattern(&self) -> &Pattern<'a> {
        &self.pattern
    }

    pub fn decl_loc(&self) -> ParserLoc {
        self.decl_loc
    }
}

/// A rule's pattern, parsed by the [`parse_rule_pattern`] function
#[derive(Debug)]
pub struct Pattern<'a> {
    /// Pattern's beginning, relative to its parent
    pub(super) loc: ParserLoc,

    /// Length of the pattern
    pub(super) decl_length: usize,

    /// Pattern mode
    pub(super) mode: Option<PatternMode>,

    /// Repetition model
    pub(super) repetition: Option<PatternRepetition>,

    /// The pattern's value
    pub(super) value: RulePatternValue<'a>,
}

impl<'a> Pattern<'a> {
    /// Get the pattern's beginning, relatively to its parent
    pub fn loc(&self) -> ParserLoc {
        self.loc
    }

    /// Get the length of the pattern, which is the size of its declaration in the input grammar
    pub fn decl_length(&self) -> usize {
        self.decl_length
    }

    /// Get the pattern's repetition model
    pub fn repetition(&self) -> Option<PatternRepetition> {
        self.repetition
    }

    /// Get the pattern's mode
    pub fn mode(&self) -> Option<PatternMode> {
        self.mode
    }

    /// Is the pattern silent?
    pub fn is_silent(&self) -> bool {
        matches!(self.mode, Some(PatternMode::Silent))
    }

    /// Is the pattern atomic?
    pub fn is_atomic(&self) -> bool {
        matches!(self.mode, Some(PatternMode::Atomic))
    }

    /// Is the pattern negative?
    pub fn is_negative(&self) -> bool {
        matches!(self.mode, Some(PatternMode::Negative))
    }

    /// Is the pattern non-consuming?
    pub fn is_dataless(&self) -> bool {
        match self.mode {
            Some(PatternMode::Silent) => true,
            Some(PatternMode::Peek) => true,
            Some(PatternMode::Negative) => true,
            Some(PatternMode::Atomic) => false,
            None => false,
        }
    }

    /// Get the pattern's value
    pub fn value(&self) -> &RulePatternValue<'a> {
        &self.value
    }
}

/// [Rule pattern](`RulePattern`)'s repetition
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternRepetition {
    // The pattern can be provided any number of times
    Any,

    // The pattern can be provided one or more times
    OneOrMore,

    // The pattern can be provided or not
    Optional,
}

impl PatternRepetition {
    /// Check if a symbol is a valid repetition symbol
    pub fn is_valid_symbol(symbol: char) -> bool {
        symbol == '*' || symbol == '+' || symbol == '?'
    }

    /// Try to parse a repetition symbol
    pub fn parse(symbol: char) -> Option<Self> {
        match symbol {
            '*' => Some(Self::Any),
            '+' => Some(Self::OneOrMore),
            '?' => Some(Self::Optional),
            _ => None,
        }
    }

    /// Get the symbol associated to a rule's repetition model
    pub fn symbol(self) -> char {
        match self {
            Self::Any => '*',
            Self::OneOrMore => '+',
            Self::Optional => '?',
        }
    }
}

/// Pattern mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternMode {
    /// Silent pattern - captures nothing
    Silent,

    /// Peek patterns - matches only if inner pattern matches, but consumes and captures nothing
    Peek,

    /// Negative pattern - reverse the match's result, captures nothing
    Negative,

    /// Atomic pattern - captured data is turned into a single string
    Atomic,
}

/// A single [`RulePattern`]'s value, indicating which content it must match
#[derive(Debug)]
pub enum RulePatternValue<'a> {
    /// Match a constant string
    CstString(&'a str),

    /// Match using another rule's content
    Rule(&'a str),

    /// Match using a group (will match on the inner pattern)
    Group(Rc<Pattern<'a>>),

    /// Match a suite of patterns
    Suite(Vec<Pattern<'a>>),

    /// Match one of the provided patterns
    /// Evaluation is performed in order, and the first matching pattern will be used
    Union(Vec<Pattern<'a>>),
}

/// Location in the input grammar
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParserLoc {
    /// Line number
    pub(super) line: usize,

    /// Column number
    pub(super) col: usize,
}

impl ParserLoc {
    /// Create a new location
    pub(crate) fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    /// Get the location's line number
    pub fn line(&self) -> usize {
        self.line
    }

    /// Get the location's column number
    pub fn col(&self) -> usize {
        self.col
    }

    pub(super) fn add_cols(&mut self, cols: usize) {
        self.col += cols;
    }

    pub(super) fn with_add_cols(&self, cols: usize) -> Self {
        Self {
            line: self.line,
            col: self.col + cols,
        }
    }
}