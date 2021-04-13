use super::errors::{ParserError, ParserErrorContent};
use super::parser::{Pattern, PegSyntaxTree, RulePatternValue};
use super::ParserLoc;
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
        validate_pattern_recursive(
            pst,
            rule.pattern().relative_loc(),
            rule.pattern(),
            &mut used,
        )?;
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
    parsed: &'a PegSyntaxTree,
    loc: ParserLoc,
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
            if parsed.rules().contains_key(name) {
                used.insert(name);
                Ok(())
            } else if is_external_rule_name(name) || is_valid_builtin_rule_name(name) {
                Ok(())
            } else {
                Err(ParserError::new(
                    loc,
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
        RulePatternValue::Group(pattern) => validate_pattern_recursive(
            parsed,
            add_parser_loc(loc.line(), loc.col(), pattern.relative_loc()),
            pattern,
            used,
        ),

        // Develop suites and unions
        RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                validate_pattern_recursive(
                    parsed,
                    add_parser_loc(loc.line(), loc.col(), pattern.relative_loc()),
                    pattern,
                    used,
                )?;
            }

            Ok(())
        }
    }
}
