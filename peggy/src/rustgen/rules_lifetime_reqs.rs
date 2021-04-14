use crate::grammar::data::*;
use crate::grammar::utils::is_builtin_rule_name;
use std::collections::{HashMap, HashSet};

pub fn build_lifetime_reqs<'a>(pst: &'a PegSyntaxTree) -> HashSet<&'a str> {
    let mut lifetime_reqs = HashMap::new();
    let mut visiting = HashSet::new();

    for name in pst.rules().keys() {
        check_rule(pst, &mut lifetime_reqs, &mut visiting, name);
    }

    lifetime_reqs
        .iter()
        .filter_map(|(rule, req_lifetime)| if *req_lifetime { Some(*rule) } else { None })
        .collect::<HashSet<_>>()
}

pub fn check_rule<'a>(
    pst: &'a PegSyntaxTree,
    lifetime_reqs: &mut HashMap<&'a str, bool>,
    visiting: &mut HashSet<&'a str>,
    name: &'a str,
) -> bool {
    if let Some(req) = lifetime_reqs.get(&name) {
        return *req;
    }

    if !visiting.insert(name) {
        return false;
    }

    if is_builtin_rule_name(name) {
        visiting.remove(name);
        false
    } else {
        let ret = check_lifetime_req(pst, lifetime_reqs, visiting, pst.rules()[name].pattern());
        visiting.remove(name);
        lifetime_reqs.insert(name, ret);
        ret
    }
}

pub fn check_lifetime_req<'a>(
    pst: &'a PegSyntaxTree,
    lifetime_reqs: &mut HashMap<&'a str, bool>,
    visiting: &mut HashSet<&'a str>,
    pattern: &'a Pattern,
) -> bool {
    match pattern.mode() {
        Some(mode) => match mode {
            PatternMode::Silent => false,
            PatternMode::Peek => false,
            PatternMode::Negative => false,
            PatternMode::Atomic => true,
        },
        None => match pattern.value() {
            RulePatternValue::CstString(_) => false,
            RulePatternValue::Rule(name) => check_rule(pst, lifetime_reqs, visiting, name),
            RulePatternValue::Group(group) => {
                check_lifetime_req(pst, lifetime_reqs, visiting, group)
            }
            RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => patterns
                .iter()
                .any(|pattern| check_lifetime_req(pst, lifetime_reqs, visiting, pattern)),
        },
    }
}
