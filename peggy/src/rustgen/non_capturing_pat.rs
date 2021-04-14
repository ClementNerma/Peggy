use crate::grammar::data::*;
use crate::grammar::utils::is_builtin_rule_name;
use std::collections::{HashMap, HashSet};

pub fn list_rules<'a>(pst: &'a PegSyntaxTree) -> HashMap<&'a str, PatternMode> {
    let mut dataless_rules = HashMap::new();
    let mut visiting = HashSet::new();

    for name in pst.rules().keys() {
        check_rule(pst, &mut dataless_rules, &mut visiting, name);
    }

    dataless_rules
}

pub fn check_rule<'a>(
    pst: &'a PegSyntaxTree,
    dataless_rules: &mut HashMap<&'a str, PatternMode>,
    visiting: &mut HashSet<&'a str>,
    name: &'a str,
) -> Option<PatternMode> {
    if let Some(typ) = dataless_rules.get(&name) {
        return Some(*typ);
    }

    if !visiting.insert(name) {
        return None;
    }

    let ret = if is_builtin_rule_name(name) {
        None
    } else if let Some(typ) =
        is_non_capturing_pattern(pst, dataless_rules, visiting, pst.rules()[name].pattern())
    {
        dataless_rules.insert(name, typ);
        Some(typ)
    } else {
        None
    };

    visiting.remove(name);

    ret
}

pub fn is_non_capturing_pattern<'a>(
    pst: &'a PegSyntaxTree,
    dataless_rules: &mut HashMap<&'a str, PatternMode>,
    visiting: &mut HashSet<&'a str>,
    pattern: &'a Pattern,
) -> Option<PatternMode> {
    if let Some(mode) = pattern.mode() {
        Some(mode)
    } else {
        match pattern.value() {
            RulePatternValue::CstString(_) => None,
            RulePatternValue::Rule(name) => check_rule(pst, dataless_rules, visiting, name),
            RulePatternValue::Group(group) => {
                is_non_capturing_pattern(pst, dataless_rules, visiting, group)
            }
            RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
                let prev = is_non_capturing_pattern(
                    pst,
                    dataless_rules,
                    visiting,
                    patterns.get(0).unwrap(),
                );
                prev.filter(|prev| {
                    patterns.iter().skip(1).all(|pat| {
                        is_non_capturing_pattern(pst, dataless_rules, visiting, pat) == Some(*prev)
                    })
                })
            }
        }
    }
}
