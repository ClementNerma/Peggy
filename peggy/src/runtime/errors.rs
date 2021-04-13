use super::executor::RuntimeCursor;
use std::fmt;

/// Error raised by the [runtime](super::execute)
#[derive(Debug)]
pub struct RuntimeError<'a> {
    /// Error's subject (the input the provided grammar was matched against)
    subject: &'a str,

    /// Optional cursor to indicate the location of the error in the input
    cursor: Option<RuntimeCursor<'a>>,

    /// Error's content
    pub(crate) content: RuntimeErrorContent<'a>,
}

impl<'a> RuntimeError<'a> {
    /// Create a new runtime error
    pub(crate) fn new(
        subject: &'a str,
        cursor: Option<RuntimeCursor<'a>>,
        content: RuntimeErrorContent<'a>,
    ) -> Self {
        Self {
            subject,
            cursor,
            content,
        }
    }

    /// Get the runtime's optional cursor, used to indicate the location of the error in the input
    pub fn cursor(&self) -> &Option<RuntimeCursor<'a>> {
        &self.cursor
    }

    /// Get the error's content
    pub fn content(&self) -> &RuntimeErrorContent<'a> {
        &self.content
    }

    /// Format the error
    pub fn format(&self) -> String {
        self.format_with_shortening(0)
    }

    /// (Internal) Format with path shortening to avoid getting too long paths
    fn format_with_shortening(&self, shorten_path: usize) -> String {
        // Get the error's displayable path, the last rule of the path, the current path's length which will be used for children shortening,
        // as well as the error's offset in the stored input.
        let (display_path, last_rule_item, sub_path_shortening, offset) = match &self.cursor {
            Some(cursor) => {
                let segments = cursor
                    .path()
                    .iter()
                    .skip(shorten_path)
                    .map(|item| format!("{}", item))
                    .collect::<Vec<_>>();

                let joined = segments.join(" => ");

                let last_rule_item = cursor.path().iter().rev().find_map(|item| match item {
                    RuntimeTreeItem::Rule(rule) => Some(format!("In rule [{}]: ", rule)),
                    _ => None,
                });

                (
                    if shorten_path > 0 && !segments.is_empty() {
                        format!("... => {}", joined)
                    } else {
                        joined
                    },
                    last_rule_item,
                    segments.len(),
                    cursor.offset(),
                )
            }
            None => (String::new(), None, 0, 0),
        };

        // Get all lines above the one the error is located on
        let head_lines_count = self.subject[0..offset].lines().count();

        // Deduce from it from the error line's number
        let line_index = if head_lines_count > 0 {
            head_lines_count - 1
        } else {
            0
        };

        // Get the length of the lines above the one the error is located on
        let head_lines_len = self
            .subject
            .lines()
            .take(line_index)
            .fold(0, |acc, value| acc + value.len());

        // Deduce from it the column of the error
        let column = offset - head_lines_len;

        // Do the formatting
        format!(
            "[Path: {}]\nERROR: At line {}, column {}: \n\n{} | {}\n{}^\n\n{}{}",
            display_path,
            line_index + 1,
            column + 1,
            line_index + 1,
            self.subject.lines().nth(line_index).unwrap(),
            " ".repeat(column + (line_index + 1).to_string().len() + 3),
            last_rule_item.unwrap_or_else(String::new),
            self.content.format_with_shortening(sub_path_shortening)
        )
    }
}

impl<'a> fmt::Display for RuntimeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

/// Runtime error's content (used in [`RuntimeError`])
#[derive(Debug)]
pub enum RuntimeErrorContent<'a> {
    /// A rule was not found
    ///
    /// This may only happen with [unchecked grammars](crate::compiler::parse_peg_nocheck)
    RuleNotFound(&'a str),

    /// Did not match a constant string
    CstStringNotMatching(&'a str),

    /// Did not match any of the member of an union
    NoMatchInUnion(Vec<RuntimeError<'a>>),

    /// Remaining content was found after the end of the grammar
    UnexpectedContent,

    /// Failed to match against builtin rule
    BuiltinRule(&'a str),

    /// Failed to match against external rule
    ExternalRule { name: &'a str, message: String },
}

impl<'a> RuntimeErrorContent<'a> {
    /// Format the error's content
    pub fn format(&self) -> String {
        self.format_with_shortening(0)
    }

    /// (Internal) Format with path shortening to avoid getting too long paths
    fn format_with_shortening(&self, shorten_path: usize) -> String {
        match self {
            Self::RuleNotFound(name) => {
                format!("Rule [{}] was not found", name)
            }
            Self::CstStringNotMatching(string) => {
                format!("Expected string literal {:?}", string)
            }
            Self::NoMatchInUnion(errors) => format!(
                "No match in union:\n{}",
                errors
                    .iter()
                    .enumerate()
                    .map(|(i, err)| {
                        format!(
                            "  {}. {}",
                            i,
                            err.format_with_shortening(shorten_path)
                                .lines()
                                .enumerate()
                                .map(|(j, line)| format!(
                                    "{}{}",
                                    " ".repeat(if j == 0 { 0 } else { i.to_string().len() + 4 }),
                                    line
                                ))
                                .collect::<Vec<_>>()
                                .join("\n")
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n\n")
            ),
            Self::UnexpectedContent => {
                "End of content was expected, found additional symbol".to_string()
            }
            Self::BuiltinRule(name) => {
                format!("Failed to match against builtin rule \"{}\"", name)
            }
            Self::ExternalRule { name, message } => {
                format!(
                    "Failed to match against external rule \"{}\": {}",
                    name, message
                )
            }
        }
    }
}

impl<'a> fmt::Display for RuntimeErrorContent<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format())
    }
}

/// Runtime tree item, used to build the visit path of a [`RuntimeError`]
#[derive(Debug, Clone, Copy)]
pub enum RuntimeTreeItem<'a> {
    /// Rule
    Rule(&'a str),

    /// Group
    Group,

    /// Nth member of a contiguous group (whitespace-separated patterns)
    FollowedMember(usize),

    /// Nth member of an union group (| -separated patterns)
    UnionMember(usize),
}

impl<'a> fmt::Display for RuntimeTreeItem<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Rule(rule) => write!(f, "Rule [{}]", rule),
            Self::Group => write!(f, "Group"),
            Self::FollowedMember(i) => write!(f, "Consecutive pattern {}", i + 1),
            Self::UnionMember(i) => write!(f, "Union member {}", i + 1),
        }
    }
}
