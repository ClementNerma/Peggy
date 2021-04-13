use super::errors::{ParserError, ParserErrorContent};
use super::parser::{Pattern, PatternRepetition, PegSyntaxTree, RulePatternValue};
use super::{utils::*, GRAMMAR_ENTRYPOINT_RULE};
use std::collections::HashSet;

/// Validate a Peggy expression parsed with [`super::parse_peg_nocheck`]
///
/// Expressions parsed with [`super::parse_peg`] don't require this check, as it is already performed automatically.
pub fn validate_parsed_peg(pst: &PegSyntaxTree) -> Result<(), ParserError> {
    // List used patterns
    let mut used = HashSet::new();

    // Validate each rule one by one
    for rule in pst.rules().values() {
        validate_pattern_recursive(pst, rule.pattern(), &mut used)?;
        check_potentially_empty_union_members(pst, rule.pattern())?;
    }

    for (name, rule) in pst.rules() {
        if *name != GRAMMAR_ENTRYPOINT_RULE && !used.contains(*name) {
            return Err(ParserError::new(rule.decl_loc(), name.len(), ParserErrorContent::UnusedRule, Some("if you are doing some testing, you can comment out the rule by starting it with the '#' symbol")));
        }
    }

    Ok(())
}

/// Validate a [`RulePattern`] recursively
fn validate_pattern_recursive<'a>(
    pst: &'a PegSyntaxTree,
    pattern: &'a Pattern,
    used: &mut HashSet<&'a str>,
) -> Result<(), ParserError> {
    match pattern.value() {
        // Constant strings don't need any validation
        RulePatternValue::CstString(_) => Ok(()),

        // For rules, ensure the specified one exists
        RulePatternValue::Rule(name) => {
            // Uppercase-only rules cannot be declared normally, and can be used to refer to an external rule
            // Else, ensure the provided rule has been declared
            if pst.rules().contains_key(name) {
                used.insert(name);
                Ok(())
            } else if is_external_rule_name(name) || is_valid_builtin_rule_name(name) {
                Ok(())
            } else {
                Err(ParserError::new(
                    pattern.loc(),
                    name.len(),
                    if is_builtin_rule_name(name) {
                        ParserErrorContent::UnknownBuiltinRule
                    } else {
                        ParserErrorContent::UnknownRule
                    },
                    None,
                ))
            }
        }

        // Develop groups
        RulePatternValue::Group(pattern) => validate_pattern_recursive(pst, pattern, used),

        // Develop suites and unions
        RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                validate_pattern_recursive(pst, pattern, used)?;
            }

            Ok(())
        }
    }
}

/// Check for potentially-empty union members, which could cause infinite loops
fn check_potentially_empty_union_members<'a>(
    pst: &'a PegSyntaxTree,
    pattern: &'a Pattern,
) -> Result<bool, ParserError> {
    match pattern.repetition() {
        Some(PatternRepetition::Any) => return Ok(true),
        Some(PatternRepetition::OneOrMore) => {}
        Some(PatternRepetition::Optional) => return Ok(true),
        None => {}
    }

    match pattern.value() {
        RulePatternValue::CstString(_) => Ok(false),

        RulePatternValue::Rule(_) => Ok(false),

        // Develop groups
        RulePatternValue::Group(pattern) => check_potentially_empty_union_members(pst, pattern),

        // Develop suites and unions
        RulePatternValue::Suite(patterns) => {
            for pattern in patterns {
                let is_potentially_empty = check_potentially_empty_union_members(pst, pattern)?;

                if !is_potentially_empty {
                    return Ok(false);
                }
            }

            Ok(true)
        }

        RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                let is_potentially_empty = check_potentially_empty_union_members(pst, pattern)?;

                if is_potentially_empty {
                    return Err(ParserError::new(
                        pattern.loc(),
                        pattern.decl_length(),
                        ParserErrorContent::PotentiallyEmptyUnionMember,
                        Some("empty union members lead to infinite loops, please ensure no member uses a '?' or '*' repetition"),
                    ));
                }
            }

            Ok(false)
        }
    }
}
