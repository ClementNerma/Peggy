use super::data::*;
use super::errors::{ParserError, ParserErrorContent};
use super::singles;
use super::utils::*;
use super::validator::validate_parsed_peg;
use std::collections::HashMap;

/// Compile a Peggy grammar to a [syntax tree](`PegSyntaxTree`)
pub fn parse_peg(grammar: &str) -> Result<PegSyntaxTree, ParserError> {
    let parsed = parse_peg_nocheck(grammar)?;

    // Ensure the syntax tree is valid
    validate_parsed_peg(&parsed)?;

    Ok(parsed)
}

/// Compile a Peggy grammar but don't check for validity (e.g. inexistant rule names, etc.)
///
/// A bit faster than [`parse_peg`] but less safe due to the lack of check.
///
/// Near-instant checks are still performed, like ensuring the presence of a `main` rule.
pub fn parse_peg_nocheck(input: &str) -> Result<PegSyntaxTree, ParserError> {
    // Collected rules
    let mut rules = HashMap::new();

    // Is a multi-line comment opened?
    let mut multi_line_comment_opened = None;

    // Iterate over each line, as there should be one rule per non-empty line
    for (l, line) in input.lines().enumerate() {
        // Left trim
        let (line, trimmed) = trim_start_and_count(line);

        if line.trim_end() == "###" {
            multi_line_comment_opened = if multi_line_comment_opened.is_none() {
                Some((l, trimmed))
            } else {
                None
            };
            continue;
        }

        if multi_line_comment_opened.is_some() {
            continue;
        }

        // Ignore empty lines
        if is_finished_line(line) {
            continue;
        }

        let mut chars = line.chars();

        // Get the first character of the line...
        let c = chars.next().unwrap();

        // ...which must be a rule's name (syntax: `rule = <content>`)
        if !c.is_alphabetic() {
            return Err(ParserError::new(
                ParserLoc::new(l, trimmed),
                1,
                ParserErrorContent::ExpectedRuleDeclaration,
                Some(match c {
                    '0'..='9' => "digits are not allowed to begin a rule's name",
                    _ => "only alphabetic characters are allowed to begin a rule's name",
                }),
            ));
        }

        // Length of the rule's name (1 as we already checked the first character)
        let mut rule_name_length = 1;

        // Indicate if the name is finished, but the loop is still active.
        // Used to count spaces separating the name and the assignment operator (=)
        let mut rule_name_ended = false;

        // Number of spaces separating the name and the assignment operator (=)
        let mut rule_op_spaces_sep = 0;

        // Iterate over characters
        loop {
            match chars.next() {
                // Identifier-compliant characters
                Some(c) if !rule_name_ended && (c.is_alphanumeric() || c == '_') => {
                    rule_name_length += c.len_utf8();
                }

                // Assignment operator (indicates the beginning of the rule's content)
                Some('=') => break,

                // Whitespaces (optional, can be used to separate the name and the assignment operator)
                Some(c) if c.is_whitespace() => {
                    rule_name_ended = true;
                    rule_op_spaces_sep += 1;
                }

                // Other characters (= non-compliant)
                Some(c) => {
                    return Err(ParserError::new(
                        ParserLoc::new(l, trimmed + rule_name_length),
                        1,
                        ParserErrorContent::IllegalSymbol(c),
                        Some("only alphanumeric and underscore characters are allowed in a rule's name")
                    ));
                }

                // End of line without an assignment operator
                None => {
                    return Err(ParserError::new(
                        ParserLoc::new(l, trimmed + rule_name_length),
                        1,
                        ParserErrorContent::ExpectedRuleAssignmentOp,
                        Some("you may have forgot to add the rule assignment operator '='"),
                    ));
                }
            }
        }

        // Collect the rule's name
        let rule_name = &line[..rule_name_length];

        // Detect reserved rule names
        if is_reserved_rule_name(rule_name) {
            return Err(ParserError::new(
                ParserLoc::new(l, trimmed),
                rule_name_length,
                ParserErrorContent::ReservedUppercaseRuleName,
                Some("try to use a name that doesn't start by 'B_' (builtin rules) or 'E_' (external rules)"),
            ));
        }

        // Detect duplicate rules
        if rules.contains_key(rule_name) {
            return Err(ParserError::new(
                ParserLoc::new(l, trimmed),
                rule_name_length,
                ParserErrorContent::DuplicateRuleName,
                None,
            ));
        }

        // Get the column the rule's content starts at
        let mut start_column = rule_name_length + rule_op_spaces_sep + 1;

        // Left-trim the content
        let (line, trimmed_2) = trim_start_and_count(&line[start_column..]);
        start_column += trimmed + trimmed_2;

        // Parse and save the new rule
        rules.insert(
            rule_name,
            Rule {
                name: rule_name,
                decl_loc: ParserLoc::new(l, trimmed),
                pattern: parse_rule_pattern(line, ParserLoc::new(l, start_column))?,
            },
        );
    }

    // Ensure all multi-line comments have been closed
    if let Some((line, col)) = multi_line_comment_opened {
        return Err(ParserError::new(
            ParserLoc::new(input.lines().count(), 0),
            0,
            ParserErrorContent::UnterminatedMultiLineComment {
                started_at: ParserLoc::new(line, col),
            },
            Some("you can add '###' on a single line to close the comment"),
        ));
    }

    // Ensure a main rule is declared
    if !rules.contains_key(GRAMMAR_ENTRYPOINT_RULE) {
        return Err(ParserError::new(
            ParserLoc::new(input.lines().count(), 0),
            0,
            ParserErrorContent::MissingMainRule,
            Some(
                "you must declare a rule named 'main' which will be the entrypoint of your syntax",
            ),
        ));
    }

    // Success!
    Ok(PegSyntaxTree { rules })
}

/// Parse a rule's content (e.g. `<content>` in `rule = <content>`)
pub fn parse_rule_pattern(input: &str, base_loc: ParserLoc) -> Result<Pattern, ParserError> {
    // This function is not supposed to be called with an empty content, so we can directly parse the first pattern of the rule
    let (first_pattern, pattern_len, stopped_because_of) = parse_sub_pattern(input, base_loc)?;

    // Remove the first pattern's content from the remaining input
    let input = &input[pattern_len..];

    // Make a global pattern
    // This will contain everything the rule's content is made of
    // The data is built depending on the reason why the pattern parser stopped previously
    match stopped_because_of {
        // If the parser stopped because it got to the end of the line, return the pattern as it is
        PatternParserStoppedBecauseOf::End => Ok(first_pattern),

        // If the parser stopped because of a continuation separator (whitespace) or an union separator (|),
        // all items of the follow/union should be collected at once
        PatternParserStoppedBecauseOf::ContinuationSep
        | PatternParserStoppedBecauseOf::UnionSep => parse_pattern_suite_or_union(
            input,
            base_loc,
            first_pattern,
            pattern_len,
            stopped_because_of,
        ),
    }
}

/// Parse a pattern's suite or union
pub fn parse_pattern_suite_or_union<'a>(
    input: &'a str,
    base_loc: ParserLoc,
    first_pattern: Pattern<'a>,
    first_pattern_consumed: usize,
    stopped_at: PatternParserStoppedBecauseOf,
) -> Result<Pattern<'a>, ParserError> {
    // The `patterns` variable contains the parsed patterns
    // The `unions` variable contains each member of the pending union. If the whole rule's content is not an union, this will remain empty.
    // When an union separator is detected, the content of `patterns` is moved to `unions` in order to separate each member of the union.
    let (mut patterns, mut unions) = if stopped_at == PatternParserStoppedBecauseOf::UnionSep {
        (vec![], vec![create_union_child(vec![first_pattern])])
    } else {
        (vec![first_pattern], vec![])
    };

    // Local modifyable input
    let mut input = input;

    // Local modifyable location
    let mut pattern_loc = base_loc.with_add_cols(first_pattern_consumed);

    loop {
        // Parse the next pattern
        let (next_pattern, next_pattern_len, next_stopped_because_of) =
            parse_sub_pattern(input, pattern_loc)?;

        // Push it to the list of pending patterns
        patterns.push(next_pattern);

        // Remove it from the remaining input
        input = &input[next_pattern_len..];

        // Trim the remaining input
        let trimmed = count_start_whitespaces(input);
        input = &input[trimmed..];

        // Update the column number
        pattern_loc.add_cols(next_pattern_len + trimmed);

        // Check the reason why the parser stopped here
        match next_stopped_because_of {
            // If it stopped because it was the end of the input, it's time to return the whole collected data
            PatternParserStoppedBecauseOf::End => {
                break Ok(Pattern {
                    loc: base_loc,
                    decl_length: pattern_loc.col - base_loc.col + 1,
                    repetition: None,
                    mode: None,
                    // If the parser stopped on the first pattern because it encountered an union separator, the remaining content
                    // should be put inside an union.
                    // Otherwise, and if no union separator was found during the parsing on the whole rule's content,
                    // a simple suite can be made from the patterns.
                    value: if stopped_at != PatternParserStoppedBecauseOf::UnionSep
                        && unions.is_empty()
                    {
                        // Avoid making a whole suite wrapper for a single pattern
                        if patterns.len() == 1 {
                            break Ok(patterns.into_iter().next().unwrap());
                        } else {
                            RulePatternValue::Suite(patterns)
                        }
                    } else {
                        // Otherwise, terminate the union
                        if !patterns.is_empty() {
                            unions.push(create_union_child(patterns));
                        }

                        assert!(unions.len() >= 2);

                        RulePatternValue::Union(unions)
                    },
                });
            }

            // If a continuation separator (whitespace) was encountered, just go to the next pattern (= do nothing for now)
            PatternParserStoppedBecauseOf::ContinuationSep => {}

            // If an union separator (|) was encountered...
            PatternParserStoppedBecauseOf::UnionSep => {
                // The whole pattern is now considered as an union, given that unions have precedence over everything else

                // Put the current patterns in the union's members list
                unions.push(create_union_child(patterns));

                // Prepare for the next union's member (if any)
                patterns = vec![];
            }
        }
    }
}

/// Parse a sub-pattern
/// The success return value is made of the parsed pattern, the consumed input length, and the reason why the parser stopped at this specific symbol
pub fn parse_sub_pattern(
    input: &str,
    mut base_loc: ParserLoc,
) -> Result<(Pattern, usize, PatternParserStoppedBecauseOf), ParserError> {
    // Left-trim the input
    let (input, trimmed) = trim_start_and_count(input);
    base_loc.add_cols(trimmed);

    // Parse the first piece (note that the entire pattern may be made of a single one)
    let (first_pattern, first_pattern_len) = parse_pattern_piece(input, base_loc)?;

    // Remove it from the remaining input
    let input = &input[first_pattern_len..];

    // If the remaining input is empty, the first piece was the whole pattern's content
    if is_finished_line(input) {
        return Ok((
            first_pattern,
            first_pattern_len + trimmed,
            PatternParserStoppedBecauseOf::End,
        ));
    }

    // Get the first character's following the first piece
    let next_char = input.chars().next().unwrap();

    let (remaining, add_trimmed) = trim_start_and_count(input);
    let is_union_sep = remaining.starts_with('|');

    // If we find an union separator or a whitespace, we can stop here
    // The parent will be in charge of continuing the processing
    if is_union_sep || next_char.is_whitespace() {
        Ok((
            first_pattern,
            trimmed + first_pattern_len + add_trimmed + if is_union_sep { 1 } else { 0 },
            if is_union_sep {
                PatternParserStoppedBecauseOf::UnionSep
            } else {
                PatternParserStoppedBecauseOf::ContinuationSep
            },
        ))
    }
    // Otherwise (if we find an unexpected character)...
    else {
        // That's an error, as the content should not end right now.
        Err(ParserError::new(
            base_loc.with_add_cols(first_pattern_len),
            1,
            ParserErrorContent::ExpectedPatternSeparatorOrEndOfLine(next_char),
            Some("adding another pattern to the suite requires a whitespace, or a vertical bar (|) for an union")
        ))
    }
}

/// Parse a rule's piece, which means a single value
///
/// This function's success return value is the parsed piece and the consumed input length
fn parse_pattern_piece(
    input: &str,
    mut base_loc: ParserLoc,
) -> Result<(Pattern, usize), ParserError> {
    let (input, trimmed, mode) = if let Some(input) = input.strip_prefix("°") {
        (input, 2, Some(PatternMode::Silent))
    } else if let Some(input) = input.strip_prefix("@") {
        (input, 1, Some(PatternMode::Atomic))
    } else if let Some(input) = input.strip_prefix("!") {
        (input, 1, Some(PatternMode::Negative))
    } else if let Some(input) = input.strip_prefix("~") {
        (input, 1, Some(PatternMode::Peek))
    } else {
        (input, 0, None)
    };

    // Update the base location
    base_loc.add_cols(trimmed);

    let (value, len) =
    // Check if the value is a constant string
    if let Some((string, len)) = singles::cst_string(input, base_loc)? {
        (RulePatternValue::CstString(string), len)
    }
    // Check if the value is a rule's name
    else if let Some((name, len)) = singles::rule_name(input, base_loc)? {
        (RulePatternValue::Rule(name), len)
    }
    // Check if the value is a group (`(...)`)
    else if let Some((group, len)) = singles::group(input, base_loc)? {
        (RulePatternValue::Group(group), len)
    }
    // If it's none of the above, it is syntax error
    else {
        return Err(ParserError::new(
            base_loc,
            0,
            ParserErrorContent::ExpectedPattern,
            Some(match input.chars().next() {
                Some('\'') => "strings require double quotes",
                Some(_) => "You may either open a group with '(', a string with '\"', or specify a rule's name",
                None => "you need to provide a rule pattern, such as a group, a string or another rule's name"
            })
        ));
    };

    // Get the piece's repetition model (* + ?) following it
    let repetition = input[len..]
        .chars()
        .next()
        .and_then(PatternRepetition::parse);

    // Compute the consumed size
    let decl_length = len + if repetition.is_some() { 1 } else { 0 };

    // Success!
    Ok((
        Pattern {
            loc: base_loc,
            decl_length,
            value,
            mode,
            repetition,
        },
        trimmed + decl_length,
    ))
}

// Create an union child (see usage)
fn create_union_child(patterns: Vec<Pattern>) -> Pattern {
    assert_ne!(patterns.len(), 0);

    if patterns.len() == 1 {
        // Avoid making a `RulePatternValue::Suite` wrapper for a single pattern
        patterns.into_iter().next().unwrap()
    } else {
        Pattern {
            loc: patterns[0].loc(),
            decl_length: patterns.iter().fold(0, |acc, pat| acc + pat.decl_length()),
            repetition: None,
            mode: None,
            value: RulePatternValue::Suite(patterns),
        }
    }
}

/// Reason by the [pattern parser](`parse_rule_pattern`) stopped at a specific moment
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternParserStoppedBecauseOf {
    End,
    ContinuationSep,
    UnionSep,
}
