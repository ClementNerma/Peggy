use super::builtin;
use super::data::{MatchedData, MatchedRule};
use super::errors::{RuntimeError, RuntimeErrorContent, RuntimeTreeItem};
use crate::compiler::utils::{is_builtin_rule_name, is_external_rule_name};
use crate::compiler::{Pattern, PatternRepetition, PegSyntaxTree, RulePatternValue};
use std::rc::Rc;

/// Match a subject string against a [parsed grammar](crate::compiler::parse_peg).
pub fn execute<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
) -> Result<MatchedRule<'a>, RuntimeError<'a>> {
    let main_rule = ctx.grammar.main_rule();

    // Match the whole string against the main rule
    let (data, len) = match_pattern(
        &ctx,
        ctx.subject,
        RuntimeCursor::new(0, vec![]),
        main_rule.pattern(),
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
    Ok(MatchedRule {
        name: "main",
        data: data.unwrap_or(MatchedData::SilentPattern),
    })
}

/// Match the given input against a single [`RulePattern`]
///
/// Arguments are the syntax tree, the original subject string, the input to use for the match, the cursor indicating the match's location,
/// and finally the pattern used for the match.
pub fn match_pattern<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    mut input: &'a str,
    cursor: RuntimeCursor<'a>,
    pattern: &Pattern<'b>,
) -> Result<(Option<MatchedData<'a>>, usize), RuntimeError<'a>> {
    // Try to match the pattern's value against a sub-input (see usages below)
    let try_match = |input: &'a str, cursor: RuntimeCursor<'a>| {
        match_pattern_value(ctx, input, cursor, pattern.value(), pattern.is_silent())
            .map(|(data, len)| (data.filter(|_| !pattern.is_silent()), len))
    };

    // The matching method depends on the repetition model (see [`crate::compiler::RuleRepetition`])
    let result = match pattern.repetition() {
        // If there is no rule, match and fail in case of error
        None => try_match(input, cursor.clone()),

        // Otherwise...
        Some(rep) => match rep {
            // If the rule can be provided multiple times...
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
                                content: RuntimeErrorContent::RuleNotFound(_),
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
                    if pattern.is_silent() {
                        None
                    } else {
                        Some(MatchedData::RepeatedPattern(total_data))
                    },
                    total_len,
                ))
            }

            // For optional matching, succeed in any case (except for ' errors)
            PatternRepetition::Optional => Ok(match try_match(input, cursor.clone()) {
                Ok((data, len)) => (
                    if pattern.is_silent() {
                        None
                    } else {
                        Some(MatchedData::OptionalPattern(Some(Rc::new(data))))
                    },
                    len,
                ),
                err
                @
                Err(RuntimeError {
                    content: RuntimeErrorContent::RuleNotFound(_),
                    ..
                }) => return err,
                Err(_) => (
                    if pattern.is_silent() {
                        None
                    } else {
                        Some(MatchedData::OptionalPattern(None))
                    },
                    0,
                ),
            }),
        },
    };

    if pattern.is_negative() {
        match result {
            Ok(_) => Err(RuntimeError::new(
                ctx.subject,
                Some(cursor),
                RuntimeErrorContent::MatchedInnerNegativePattern,
            )),
            Err(_) => Ok((Some(MatchedData::NegativePattern), 0)),
        }
    } else {
        let (data, consumed) = result?;

        Ok((
            if pattern.is_atomic() {
                data.map(|_| MatchedData::AtomicPattern(&input[..consumed]))
            } else {
                data
            },
            consumed,
        ))
    }
}

/// Match the given input against a single [`RulePatternValue`]
///
/// Arguments are the syntax tree, the original subject string, the input to use for the match, the cursor indicating the match's location,
/// and finally the pattern's value used for the match.
pub fn match_pattern_value<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    input: &'a str,
    cursor: RuntimeCursor<'a>,
    pattern_value: &RulePatternValue<'b>,
    is_silent: bool,
) -> Result<(Option<MatchedData<'a>>, usize), RuntimeError<'a>> {
    match pattern_value {
        // Match against a constant string
        RulePatternValue::CstString(string) => {
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

        // Match against another rule
        RulePatternValue::Rule(name) => {
            let cursor = cursor.with_child(RuntimeTreeItem::Rule(name));

            // Builtin rules
            if is_builtin_rule_name(name) {
                return match_builtin_rule(ctx, input, cursor, name)
                    .map(|(data, len)| (if is_silent { None } else { Some(data) }, len));
            }

            // External rules
            if is_external_rule_name(name) {
                return match_external_rule(ctx, input, cursor, name)
                    .map(|(data, len)| (if is_silent { None } else { Some(data) }, len));
            }

            // Declared rules
            let rule = match ctx.grammar.rules().get(name) {
                Some(rule) => rule,
                None => {
                    return Err(RuntimeError::new(
                        ctx.subject,
                        Some(cursor),
                        RuntimeErrorContent::RuleNotFound(name),
                    ))
                }
            };

            match_pattern(ctx, input, cursor, rule.pattern()).map(|(data, len)| {
                (
                    if is_silent {
                        None
                    } else {
                        Some(MatchedData::Rule(Rc::new(MatchedRule {
                            name,
                            data: data.unwrap_or(MatchedData::SilentPattern),
                        })))
                    },
                    len,
                )
            })
        }

        // Match against a group (= against the group's content)
        RulePatternValue::Group(inner_pattern) => match_pattern(
            ctx,
            input,
            cursor.with_child(RuntimeTreeItem::Group),
            &inner_pattern,
        ),

        // Match a suite of patterns
        RulePatternValue::Suite(patterns) => {
            let mut input = input;
            let mut column = 0;
            let mut matched = vec![];

            for (i, pattern) in patterns.iter().enumerate() {
                let (data, len) = match_pattern(
                    ctx,
                    input,
                    cursor.with_child_and_additional_offset(
                        RuntimeTreeItem::FollowedMember(i),
                        column,
                    ),
                    pattern,
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
        RulePatternValue::Union(patterns) => {
            let mut errors = vec![];
            let mut greedy_candidate = None;

            for (i, pattern) in patterns.iter().enumerate() {
                match match_pattern(
                    ctx,
                    input,
                    cursor.with_child(RuntimeTreeItem::UnionMember(i)),
                    pattern,
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

/// Match the input against a builtin rule
pub fn match_builtin_rule<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    input: &'a str,
    cursor: RuntimeCursor<'a>,
    rule_name: &'a str,
) -> Result<(MatchedData<'a>, usize), RuntimeError<'a>> {
    let next_char = input.chars().next();

    // Look for builtin rules
    match builtin::match_builtin_rule(rule_name, next_char) {
        Some(true) => Ok((
            MatchedData::BuiltinRule {
                name: rule_name,
                symbol: next_char,
            },
            if next_char.is_some() { 1 } else { 0 },
        )),
        Some(false) => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::BuiltinRule(rule_name),
        )),
        None => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::RuleNotFound(rule_name),
        )),
    }
}

/// Match the input against an external rule
pub fn match_external_rule<'a, 'b: 'a>(
    ctx: &RuntimeContext<'a, 'b>,
    input: &'a str,
    cursor: RuntimeCursor<'a>,
    rule_name: &'a str,
) -> Result<(MatchedData<'a>, usize), RuntimeError<'a>> {
    let external_fn = match ctx.external_rules.as_ref() {
        Some(func) => func,
        None => {
            return Err(RuntimeError::new(
                ctx.subject,
                Some(cursor.clone()),
                RuntimeErrorContent::RuleNotFound(rule_name),
            ))
        }
    };

    // Look for builtin rules
    match (external_fn)(rule_name, input) {
        Some(Ok(len)) => Ok((
            MatchedData::ExternalRule {
                name: rule_name,
                matched: &rule_name[..len],
            },
            len,
        )),
        Some(Err(message)) => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::ExternalRule {
                name: rule_name,
                message,
            },
        )),
        None => Err(RuntimeError::new(
            ctx.subject,
            Some(cursor),
            RuntimeErrorContent::RuleNotFound(rule_name),
        )),
    }
}

/// Runtime execution context
pub struct RuntimeContext<'a, 'b: 'a> {
    /// Syntax tree
    pub grammar: &'b PegSyntaxTree<'b>,

    /// Initial subject
    pub subject: &'a str,

    /// External rules (arguments are the rule's name, followed by the input to process)
    pub external_rules: Option<ExternalRulesHandler>,

    /// Options
    pub options: RuntimeOptions,
}

/// Runtime options
#[derive(Debug, Clone, Copy)]
pub struct RuntimeOptions {
    /// Make the rules as lazy as possible.
    /// By default, all rules in unions are evaluated, and the one consuming the most data is kept.
    //  With this setting enabled, as soon as a rule matches, its content will be returned.
    /// This can improve performances a little, but may cause hard to debug problems if you grammar isn't optimized for this setting.
    /// With this enabled, unions will always need to have the most-consuming rules first, and only after the less-consuming ones.
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

/// External rules handler
///
/// This handler is called each time a rule with a name starting with `E_` is used.
///
/// The first argument is the rule's name, and the second the input it is applied on.
///
/// The return value can be `None` if the rule does not exist.
///
/// If the matching suceeds, the number of consumed characters must be returned as an `Some(Ok(usize))`.
///
/// In case of error, a string can be returned wrapped in a `Some(Err(String))`.
///
/// ## Example usage
///
/// ```rust
/// use peggy::runtime::ExternalRulesHandler;
///
/// let handler: ExternalRulesHandler = Box::new(|rule, input| {
///     match rule {
///         "E_LETTER_A" => Some(match input.chars().next() {
///             Some('a') | Some('A') => Ok(1),
///             Some(_) => Err("Next character is not the 'A' letter".to_string()),
///             None => Err("Remaining input is empty".to_string())
///         }),
///         _ => None
///    }
/// });
/// ```
pub type ExternalRulesHandler = Box<dyn Fn(&str, &str) -> Option<Result<usize, String>>>;

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
