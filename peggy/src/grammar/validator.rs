use super::data::*;
use super::errors::{ParserError, ParserErrorContent};
use super::utils::*;
use std::collections::HashSet;

/// Validate a Peggy expression parsed with [`super::parse_peg_nocheck`]
///
/// Expressions parsed with [`super::parse_peg`] don't require this check, as it is already performed automatically.
pub fn validate_parsed_peg(pst: &PegSyntaxTree) -> Result<(), ParserError> {
    let mut ordered_rules: Vec<_> = pst.rules().iter().collect();
    ordered_rules.sort_by_key(|(_, rule)| rule.decl_loc.line());

    // Validate each rule one by one
    for (_, rule) in &ordered_rules {
        validate_pattern_recursive(pst.rules(), rule.pattern())?;
        check_potentially_empty_union_members(pst.rules(), rule.pattern())?;
    }

    // Detect unused rules
    let used = list_used_rules(pst.rules());

    for (name, rule) in &ordered_rules {
        if **name != GRAMMAR_ENTRYPOINT_RULE && !used.contains(**name) {
            return Err(ParserError::new(
                rule.decl_loc(),
                name.len(),
                ParserErrorContent::UnusedRule,
                Some("if you are doing some testing, you can comment out the rule by starting it with the '#' symbol")
            ));
        }
    }

    Ok(())
}

/// Validate a [`RulePattern`] recursively
fn validate_pattern_recursive<'a>(
    rules: &'a Rules,
    pattern: &'a Pattern,
) -> Result<(), ParserError> {
    match pattern.value() {
        // Constant strings don't need any validation
        RulePatternValue::CstString(_) => Ok(()),

        // For rules, ensure the specified one exists
        RulePatternValue::Rule(name) => {
            // Uppercase-only rules cannot be declared normally, and can be used to refer to an external rule
            // Else, ensure the provided rule has been declared
            if rules.contains_key(name)
                || is_external_rule_name(name)
                || is_valid_builtin_rule_name(name)
            {
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
        RulePatternValue::Group(pattern) => validate_pattern_recursive(rules, pattern),

        // Develop suites and unions
        RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                validate_pattern_recursive(rules, pattern)?;
            }

            Ok(())
        }
    }
}

/// List all used rules (directly or inderectly, starting from the entrypoint rule)
pub fn list_used_rules<'a>(rules: &Rules<'a>) -> HashSet<&'a str> {
    let mut used_rules = HashSet::new();
    list_pattern_used_rules(
        rules,
        rules[GRAMMAR_ENTRYPOINT_RULE].pattern(),
        &mut used_rules,
    );
    used_rules
}

/// List all used rules inside a pattern
fn list_pattern_used_rules<'a>(
    rules: &Rules<'a>,
    pattern: &Pattern<'a>,
    used_rules: &mut HashSet<&'a str>,
) {
    match pattern.value() {
        RulePatternValue::CstString(_) => {}
        RulePatternValue::Rule(name) => {
            if !is_valid_builtin_rule_name(name)
                && !is_external_rule_name(name)
                && used_rules.insert(name)
            {
                list_pattern_used_rules(rules, rules[name].pattern(), used_rules)
            }
        }
        RulePatternValue::Group(pattern) => list_pattern_used_rules(rules, pattern, used_rules),
        RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                list_pattern_used_rules(rules, pattern, used_rules);
            }
        }
    }
}

/// Check for potentially-empty union members, which could cause infinite loops
fn check_potentially_empty_union_members<'a>(
    rules: &Rules<'a>,
    pattern: &Pattern<'a>,
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
        RulePatternValue::Group(pattern) => check_potentially_empty_union_members(rules, pattern),

        // Develop suites and unions
        RulePatternValue::Suite(patterns) => {
            for pattern in patterns {
                let is_potentially_empty = check_potentially_empty_union_members(rules, pattern)?;

                if !is_potentially_empty {
                    return Ok(false);
                }
            }

            Ok(true)
        }

        RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                let is_potentially_empty = check_potentially_empty_union_members(rules, pattern)?;

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
