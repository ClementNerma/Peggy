use crate::compiler::data::*;
use crate::compiler::utils::is_builtin_rule_name;
use std::collections::HashMap;

pub fn list_rules<'a>(pst: &'a PegSyntaxTree) -> HashMap<&'a str, PatternMode> {
    let mut dataless_rules = HashMap::new();

    for name in pst.rules().keys() {
        check_rule(pst, &mut dataless_rules, name);
    }

    dataless_rules
}

pub fn check_rule<'a>(
    pst: &'a PegSyntaxTree,
    dataless_rules: &mut HashMap<&'a str, PatternMode>,
    name: &'a str,
) -> Option<PatternMode> {
    if let Some(typ) = dataless_rules.get(&name) {
        return Some(*typ);
    }

    if is_builtin_rule_name(name) {
        None
    } else if let Some(typ) =
        is_non_capturing_pattern(pst, dataless_rules, pst.rules()[name].pattern())
    {
        dataless_rules.insert(name, typ);
        Some(typ)
    } else {
        None
    }
}

pub fn is_non_capturing_pattern<'a>(
    pst: &'a PegSyntaxTree,
    dataless_rules: &mut HashMap<&'a str, PatternMode>,
    pattern: &'a Pattern,
) -> Option<PatternMode> {
    if let Some(mode) = pattern.mode() {
        Some(mode)
    } else {
        match pattern.value() {
            RulePatternValue::CstString(_) => None,
            RulePatternValue::Rule(name) => check_rule(pst, dataless_rules, name),
            RulePatternValue::Group(group) => is_non_capturing_pattern(pst, dataless_rules, group),
            RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
                let prev = is_non_capturing_pattern(pst, dataless_rules, patterns.get(0).unwrap());
                prev.filter(|prev| {
                    patterns.iter().skip(1).all(|pat| {
                        is_non_capturing_pattern(pst, dataless_rules, pat) == Some(*prev)
                    })
                })
            }
        }
    }
}
