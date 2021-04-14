use crate::grammar::data::*;
use crate::grammar::utils::*;
use std::collections::{HashMap, HashSet};

pub fn find<'a>(pst: &'a PegSyntaxTree) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut rec = HashMap::new();
    find_in_rule(pst, &mut vec![], &mut rec, GRAMMAR_ENTRYPOINT_RULE);
    rec
}

pub fn find_in_rule<'a>(
    pst: &'a PegSyntaxTree,
    path: &mut Vec<&'a str>,
    treated_recursives: &mut HashMap<&'a str, HashSet<&'a str>>,
    rule_name: &'a str,
) {
    if is_valid_builtin_rule_name(rule_name) || is_external_rule_name(rule_name) {
        return;
    }

    path.push(rule_name);

    let rule = pst.rules().get(rule_name).unwrap();
    build_rules_list(pst, path, treated_recursives, rule.pattern().value());

    path.pop();
}

pub fn build_rules_list<'a>(
    pst: &'a PegSyntaxTree,
    path: &mut Vec<&'a str>,
    treated_recursives: &mut HashMap<&'a str, HashSet<&'a str>>,
    pattern_value: &'a RulePatternValue,
) {
    match pattern_value {
        RulePatternValue::CstString(_) => {}
        RulePatternValue::Rule(name) => {
            if path.contains(name) {
                let parent_name = path[path.len() - 1];

                if let Some(list) = treated_recursives.get_mut(parent_name) {
                    list.insert(name);
                } else {
                    let mut list = HashSet::new();
                    list.insert(*name);
                    treated_recursives.insert(parent_name, list);
                }
            } else {
                find_in_rule(pst, path, treated_recursives, name);
            }
        }
        RulePatternValue::Group(pattern) => {
            build_rules_list(pst, path, treated_recursives, pattern.value())
        }
        RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                build_rules_list(pst, path, treated_recursives, pattern.value());
            }
        }
    }
}
