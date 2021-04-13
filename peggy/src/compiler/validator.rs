use super::errors::{ParserError, ParserErrorContent};
use super::parser::{PegSyntaxTree, Pattern, RulePatternValue};
use super::utils::{
    add_parser_loc, is_builtin_rule_name, is_external_rule_name, is_valid_builtin_rule_name,
};
use super::ParserLoc;

/// Validate a Peggy expression parsed with [`super::parse_peg_nocheck`]
///
/// Expressions parsed with [`super::parse_peg`] don't require this check, as it is already performed automatically.
pub fn validate_parsed_peg(parsed: &PegSyntaxTree) -> Result<(), ParserError> {
    // Validate each rule one by one
    for rule in parsed.rules().values() {
        validate_pattern_recursive(parsed, rule.pattern().relative_loc(), rule.pattern())?;
    }

    Ok(())
}

/// Validate a [`RulePattern`] recursively
fn validate_pattern_recursive(
    parsed: &PegSyntaxTree,
    loc: ParserLoc,
    pattern: &Pattern,
) -> Result<(), ParserError> {
    match pattern.value() {
        // Constant strings don't need any validation
        RulePatternValue::CstString(_) => Ok(()),

        // For rules, ensure the specified one exists
        RulePatternValue::Rule(name) => {
            // Uppercase-only rules cannot be declared normally, and can be used to refer to an external rule
            // Else, ensure the provided rule has been declared
            if parsed.rules().contains_key(name)
                || is_external_rule_name(name)
                || is_valid_builtin_rule_name(name)
            {
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
        ),

        // Develop suites and unions
        RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                validate_pattern_recursive(
                    parsed,
                    add_parser_loc(loc.line(), loc.col(), pattern.relative_loc()),
                    pattern,
                )?;
            }

            Ok(())
        }
    }
}
