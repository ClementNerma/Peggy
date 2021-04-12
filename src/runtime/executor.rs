use super::builtin;
use super::data::{MatchedData, MatchingPattern};
use super::errors::{RuntimeError, RuntimeErrorContent, RuntimeTreeItem};
use crate::compiler::utils::{is_builtin_pattern_name, is_external_pattern_name};
use crate::compiler::{PatternPiece, PatternPieceValue, PatternRepetition, PegSyntaxTree};
use std::rc::Rc;

/// Match a subject string against a [parsed grammar](crate::compiler::parse_peg).
pub fn execute<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
) -> Result<MatchingPattern<'a>, RuntimeError<'a>> {
    let main_pattern = ctx.grammar.main_pattern();

    // Match the whole string against the main pattern
    let (data, len) = match_piece(
        &ctx,
        ctx.subject,
        RuntimeCursor::new(0, vec![]),
        main_pattern.inner_piece(),
    )?;

    // Ensure there is no remaining data
    if len < ctx.subject.len() {
        return Err(RuntimeError::new(
            ctx.subject,
            Some(RuntimeCursor::new(len, vec![])),
            RuntimeErrorContent::UnexpectedContent,
        ));
    }

    // Success!
    Ok(MatchingPattern {
        name: "main",
        data: data.unwrap_or(MatchedData::SilentPattern),
    })
}

/// Match the given input against a single [`PatternPiece`]
///
/// Arguments are the syntax tree, the original subject string, the input to use for the match, the cursor indicating the match's location,
/// and finally the piece used for the match.
pub fn match_piece<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    mut input: &'a str,
    cursor: RuntimeCursor<'a>,
    piece: &PatternPiece<'b>,
) -> Result<(Option<MatchedData<'a>>, usize), RuntimeError<'a>> {
    // Try to match the piece's value against a sub-input (see usages below)
    let try_match = |input: &'a str, cursor: RuntimeCursor<'a>| {
        match_piece_value(ctx, input, cursor, piece.value(), piece.is_silent())
            .map(|(data, len)| (data.filter(|_| !piece.is_silent()), len))
    };

    // The matching method depends on the repetition model (see [`crate::compiler::PatternRepetition`])
    match piece.repetition() {
        // If there is no pattern, match and fail in case of error
        None => try_match(input, cursor),

        // Otherwise...
        Some(rep) => match rep {
            // If the pattern can be provided multiple times...
            PatternRepetition::Any | PatternRepetition::OneOrMore => {
                // Initialize the total data and consumed length
                let mut total_data = vec![];
                let mut total_len = 0;
                let mut matched_once = false;

                // Match as much as possible
                loop {
                    let (data, len) =
                        match try_match(input, cursor.with_additional_offset(total_len)) {
                            err
                            @
                            Err(RuntimeError {
                                content: RuntimeErrorContent::PatternNotFound(_),
                                ..
                            }) => return err,
                            Err(err) => {
                                if rep == PatternRepetition::OneOrMore && !matched_once {
                                    return Err(err);
                                } else {
                                    break;
                                }
                            }
                            Ok(result) => result,
                        };

                    matched_once = true;

                    if let Some(data) = data {
                        total_data.push(data);
                    }

                    total_len += len;
                    input = &input[len..];
                }

                // Success!
                Ok((
                    if piece.is_silent() {
                        None
                    } else {
                        Some(MatchedData::RepeatedPiece(total_data))
                    },
                    total_len,
                ))
            }

            // For optional matching, succeed in any case (except for ' errors)
            PatternRepetition::Optional => Ok(match try_match(input, cursor) {
                Ok((data, len)) => (
                    if piece.is_silent() {
                        None
                    } else {
                        Some(MatchedData::OptionalPiece(Some(Rc::new(data))))
                    },
                    len,
                ),
                err
                @
                Err(RuntimeError {
                    content: RuntimeErrorContent::PatternNotFound(_),
                    ..
                }) => return err,
                Err(_) => (
                    if piece.is_silent() {
                        None
                    } else {
                        Some(MatchedData::OptionalPiece(None))
                    },
                    0,
                ),
            }),
        },
    }
}

/// Match the given input against a single [`PatternPieceValue`]
///
/// Arguments are the syntax tree, the original subject string, the input to use for the match, the cursor indicating the match's location,
/// and finally the piece's value used for the match.
pub fn match_piece_value<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    input: &'a str,
    cursor: RuntimeCursor<'a>,
    piece_value: &PatternPieceValue<'b>,
    is_silent: bool,
) -> Result<(Option<MatchedData<'a>>, usize), RuntimeError<'a>> {
    match piece_value {
        // Match against a constant string
        PatternPieceValue::CstString(string) => {
            if input.starts_with(string) {
                Ok((
                    if is_silent {
                        None
                    } else {
                        Some(MatchedData::CstString(string))
                    },
                    string.len(),
                ))
            } else {
                Err(RuntimeError::new(
                    ctx.subject,
                    Some(cursor),
                    RuntimeErrorContent::CstStringNotMatching(string),
                ))
            }
        }

        // Match against another pattern
        PatternPieceValue::Pattern(name) => {
            let cursor = cursor.with_child(RuntimeTreeItem::Pattern(name));

            // Builtin patterns
            if is_builtin_pattern_name(name) {
                return match_builtin_pattern(ctx, input, cursor, name)
                    .map(|(data, len)| (if is_silent { None } else { Some(data) }, len));
            }

            // External patterns
            if is_external_pattern_name(name) {
                return match_external_pattern(ctx, input, cursor, name)
                    .map(|(data, len)| (if is_silent { None } else { Some(data) }, len));
            }

            // Declared patterns
            let pattern = match ctx.grammar.patterns().get(name) {
                Some(pattern) => pattern,
                None => {
                    return Err(RuntimeError::new(
                        ctx.subject,
                        Some(cursor),
                        RuntimeErrorContent::PatternNotFound(name),
                    ))
                }
            };

            match_piece(ctx, input, cursor, pattern.inner_piece()).map(|(data, len)| {
                (
                    if is_silent {
                        None
                    } else {
                        Some(MatchedData::Pattern(Rc::new(MatchingPattern {
                            name,
                            data: data.unwrap_or(MatchedData::SilentPattern),
                        })))
                    },
                    len,
                )
            })
        }

        // Match against a group (= against the group's content)
        PatternPieceValue::Group(inner_piece) => match_piece(
            ctx,
            input,
            cursor.with_child(RuntimeTreeItem::Group),
            &inner_piece,
        ),

        // Match a suite of pieces
        PatternPieceValue::Suite(pieces) => {
            let mut input = input;
            let mut column = 0;
            let mut matched = vec![];

            for (i, piece) in pieces.iter().enumerate() {
                let (data, len) = match_piece(
                    ctx,
                    input,
                    cursor.with_child_and_additional_offset(
                        RuntimeTreeItem::FollowedMember(i),
                        column,
                    ),
                    piece,
                )?;

                if let Some(data) = data {
                    matched.push(data);
                }

                column += len;
                input = &input[len..];
            }

            Ok((
                if is_silent {
                    None
                } else {
                    Some(MatchedData::SuiteOf(matched))
                },
                column,
            ))
        }

        // Match any of an union's members, in order
        PatternPieceValue::Union(pieces) => {
            let mut errors = vec![];
            let mut greedy_candidate = None;

            for (i, piece) in pieces.iter().enumerate() {
                match match_piece(
                    ctx,
                    input,
                    cursor.with_child(RuntimeTreeItem::UnionMember(i)),
                    piece,
                ) {
                    Ok(result) => {
                        if ctx.options.lazy_unions {
                            return Ok(result);
                        } else if let Some((_, current_candidate_len)) = greedy_candidate {
                            if result.1 > current_candidate_len {
                                greedy_candidate = Some(result);
                            }
                        } else {
                            greedy_candidate = Some(result);
                        }
                    }
                    Err(err) => errors.push(err),
                }
            }

            if let Some(result) = greedy_candidate {
                Ok(result)
            } else {
                Err(RuntimeError::new(
                    ctx.subject,
                    Some(cursor),
                    RuntimeErrorContent::NoMatchInUnion(errors),
                ))
            }
        }
    }
}

/// Match the input against a builtin pattern
pub fn match_builtin_pattern<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    input: &'a str,
    cursor: RuntimeCursor<'a>,
    pattern_name: &'a str,
) -> Result<(MatchedData<'a>, usize), RuntimeError<'a>> {
    let next_char = input.chars().next();

    // Look for builtin patterns
    match builtin::match_builtin_pattern(pattern_name, next_char) {
        Some(true) => Ok((
            MatchedData::BuiltinPattern {
                name: pattern_name,
                symbol: next_char,
            },
            if next_char.is_some() { 1 } else { 0 },
        )),
        Some(false) => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::BuiltinPattern(pattern_name),
        )),
        None => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::PatternNotFound(pattern_name),
        )),
    }
}

/// Match the input against an external pattern
pub fn match_external_pattern<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    input: &'a str,
    cursor: RuntimeCursor<'a>,
    pattern_name: &'a str,
) -> Result<(MatchedData<'a>, usize), RuntimeError<'a>> {
    let external_fn = match ctx.external_patterns.as_ref() {
        Some(func) => func,
        None => {
            return Err(RuntimeError::new(
                ctx.subject,
                Some(cursor.clone()),
                RuntimeErrorContent::PatternNotFound(pattern_name),
            ))
        }
    };

    // Look for builtin patterns
    match (external_fn)(pattern_name, input) {
        Some(Ok(len)) => Ok((
            MatchedData::ExternalPattern {
                name: pattern_name,
                matched: &pattern_name[..len],
            },
            len,
        )),
        Some(Err(message)) => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::ExternalPattern {
                name: pattern_name,
                message,
            },
        )),
        None => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::PatternNotFound(pattern_name),
        )),
    }
}

/// Runtime execution context
pub struct RuntimeContext<'a, 'b: 'a> {
    /// Syntax tree
    pub grammar: &'b PegSyntaxTree<'b>,

    /// Initial subject
    pub subject: &'a str,

    /// External patterns (arguments are the pattern's name, followed by the input to process)
    pub external_patterns: Option<ExternalPatternsHandler>,

    /// Options
    pub options: RuntimeOptions,
}

/// Runtime options
#[derive(Debug, Clone, Copy)]
pub struct RuntimeOptions {
    /// Make the patterns as lazy as possible.
    /// By default, all patterns in unions are evaluated, and the one consuming the most data is kept.
    //  With this setting enabled, as soon as a pattern matches, its content will be returned.
    /// This can improve performances a little, but may cause hard to debug problems if you grammar isn't optimized for this setting.
    /// With this enabled, unions will always need to have the most-consuming patterns first, and only after the less-consuming ones.
    pub lazy_unions: bool,
}

impl RuntimeOptions {
    /// Create a new set of runtime options
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for RuntimeOptions {
    fn default() -> Self {
        RuntimeOptions { lazy_unions: false }
    }
}

/// External patterns handler
///
/// This handler is called each time a pattern with a name starting with `E_` is used.
///
/// The first argument is the pattern's name, and the second the input it is applied on.
///
/// The return value can be `None` if the pattern does not exist.
///
/// If the matching suceeds, the number of consumed characters must be returned as an `Some(Ok(usize))`.
///
/// In case of error, a string can be returned wrapped in a `Some(Err(String))`.
///
/// ## Example usage
///
/// ```rust
/// use peggy::runtime::ExternalPatternsHandler;
///
/// let handler: ExternalPatternsHandler = Box::new(|pattern, input| {
///     match pattern {
///         "E_LETTER_A" => Some(match input.chars().next() {
///             Some('a') | Some('A') => Ok(1),
///             Some(_) => Err("Next character is not the 'A' letter".to_string()),
///             None => Err("Remaining input is empty".to_string())
///         }),
///         _ => None
///    }
/// });
/// ```
pub type ExternalPatternsHandler = Box<dyn Fn(&str, &str) -> Option<Result<usize, String>>>;

/// Runtime cursor, used to indicate the location of a match
#[derive(Debug, Clone)]
pub struct RuntimeCursor<'a> {
    /// Offset in the initial subject string
    offset: usize,

    /// Item visit path
    path: Vec<RuntimeTreeItem<'a>>,
}

impl<'a> RuntimeCursor<'a> {
    /// Create a new cursor
    pub(crate) fn new(offset: usize, path: Vec<RuntimeTreeItem<'a>>) -> Self {
        Self { offset, path }
    }

    /// Get the error's offset in the initial subject string
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Get the visit path where the error occurred
    pub fn path(&self) -> &Vec<RuntimeTreeItem<'a>> {
        &self.path
    }

    /// Create a new cursor with an additional offset
    ///
    /// Used for matching in items' children
    pub(crate) fn with_additional_offset(&self, offset: usize) -> Self {
        let mut cloned = self.clone();
        cloned.offset += offset;
        cloned
    }

    /// Create a new cursor with an additional child item
    ///
    /// Used for matching in items' children
    pub(crate) fn with_child(&self, item: RuntimeTreeItem<'a>) -> Self {
        let mut cloned = self.clone();
        cloned.path.push(item);
        cloned
    }

    /// Create a new cursor with an additional offset *and* child item
    ///
    /// Used for matching in items' children
    pub(crate) fn with_child_and_additional_offset(
        &self,
        item: RuntimeTreeItem<'a>,
        offset: usize,
    ) -> Self {
        let mut cloned = self.clone();
        cloned.path.push(item);
        cloned.offset += offset;
        cloned
    }
}
