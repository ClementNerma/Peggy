use crate::grammar::{Pattern, PatternMode, PegSyntaxTree, RulePatternValue};

/// Generate a Peggy grammar from its syntax tree
///
/// Useful to get back to the source code after parsing.
///
/// Note that blank lines and comments, as well as additional whitespaces, won't be restored.
pub fn gen_peggy(pst: &PegSyntaxTree) -> String {
    pst.rules()
        .iter()
        .map(|(name, rule)| format!("{} = {}", name, gen_peggy_pattern(rule.pattern())))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate a Peggy code for a single [`RulePattern`]
pub fn gen_peggy_pattern(pattern: &Pattern) -> String {
    let mode = match pattern.mode() {
        Some(PatternMode::Silent) => "°",
        Some(PatternMode::Peek) => "~",
        Some(PatternMode::Negative) => "!",
        Some(PatternMode::Atomic) => "@",
        None => "",
    };

    let mut pattern_value = format!("{}{}", mode, gen_peggy_pattern_value(pattern.value()));

    if let Some(rep) = pattern.repetition() {
        pattern_value.push(rep.symbol());
    }

    pattern_value
}

/// Generate a Peggy code for a single [`RulePatternValue`]
pub fn gen_peggy_pattern_value(value: &RulePatternValue) -> String {
    match value {
        RulePatternValue::CstString(string) => format!("{:?}", string),
        RulePatternValue::Rule(name) => name.to_string(),
        RulePatternValue::Group(inner) => format!("({})", gen_peggy_pattern(inner.as_ref())),
        RulePatternValue::Suite(patterns) => patterns
            .iter()
            .map(gen_peggy_pattern)
            .collect::<Vec<_>>()
            .join(" "),
        RulePatternValue::Union(patterns) => patterns
            .iter()
            .map(gen_peggy_pattern)
            .collect::<Vec<_>>()
            .join(" | "),
    }
}
