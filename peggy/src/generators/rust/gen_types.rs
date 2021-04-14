use super::{make_safe_ident, InternalState};
use crate::compiler::data::*;
use crate::compiler::utils::*;
use quote::__private::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashMap;

pub fn gen_rule_type<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    rule: &'a Rule,
) -> Option<TokenStream> {
    gen_pattern_type(state, visiting, rule.pattern())
}

pub fn gen_pattern_type<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    pattern: &'a Pattern,
) -> Option<TokenStream> {
    if pattern.is_dataless() {
        return None;
    }

    let pattern_type = if pattern.is_atomic() {
        quote! { &'a str }
    } else {
        gen_pattern_value_type(state, visiting, pattern.value())?
    };

    match pattern.repetition() {
        None => Some(pattern_type),
        Some(rep) => match rep {
            PatternRepetition::Any | PatternRepetition::OneOrMore => {
                Some(quote! { Vec<#pattern_type> })
            }
            PatternRepetition::Optional => Some(quote! { Option<#pattern_type> }),
        },
    }
}

pub fn gen_pattern_value_type<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    value: &'a RulePatternValue,
) -> Option<TokenStream> {
    match value {
        RulePatternValue::CstString(string) => {
            Some(if let Some(ident) = state.cst_string_types.get(string) {
                quote! { super::strings::#ident }
            } else {
                let ident = format_str_type(&mut state.cst_string_counters, string);
                state.cst_string_types.insert(string, ident.clone());
                quote! { super::strings::#ident }
            })
        }
        RulePatternValue::Rule(name) => {
            let ident = make_safe_ident(name);

            if is_builtin_rule_name(name) {
                Some(quote! { super::matched::#ident })
            } else if let Some(PatternMode::Silent) = state.non_capturing_rules.get(name) {
                None
            } else {
                let lifetime_req = if state.rules_with_lifetime.contains(name) {
                    quote! { <'a> }
                } else {
                    quote! {}
                };

                if state.recursive_paths[visiting].contains(name) {
                    Some(quote! { std::rc::Rc<super::matched::#ident #lifetime_req> })
                } else {
                    Some(quote! { super::matched::#ident #lifetime_req })
                }
            }
        }
        RulePatternValue::Group(inner) => gen_pattern_type(state, visiting, inner.as_ref()),
        RulePatternValue::Suite(patterns) => {
            let types: Vec<_> = patterns
                .iter()
                .map(|pattern| gen_pattern_type(state, visiting, pattern))
                .filter_map(|pattern| pattern)
                .collect();

            if types.is_empty() {
                None
            } else if types.len() == 1 {
                Some(quote! { #(#types)* })
            } else {
                Some(quote! { (#(#types),*) })
            }
        }
        RulePatternValue::Union(patterns) => {
            let types: Vec<_> = patterns
                .iter()
                .map(|pattern| {
                    gen_pattern_type(state, visiting, pattern).unwrap_or_else(|| quote! { () })
                })
                .collect();

            let variants_len = patterns.len();

            let union_type = format_ident!("Sw{}", variants_len);

            if patterns.is_empty() {
                None
            } else {
                Some(quote! { super::unions::#union_type<#(#types),*> })
            }
        }
    }
}

pub fn format_str_type<'a>(
    cst_string_counters: &mut HashMap<&'a str, usize>,
    cst_string: &'a str,
) -> TokenStream {
    let mut typename = String::new();
    let mut got_space = false;

    for c in cst_string.chars() {
        if c.is_whitespace() {
            if got_space {
                typename.push('_');
            } else {
                got_space = true;
            }
            continue;
        }

        if c.is_alphanumeric() {
            if got_space {
                got_space = false;
                typename.push_str(&c.to_uppercase().collect::<String>());
            } else {
                typename.push(c);
            }
            continue;
        }

        got_space = false;

        if c == '_' {
            typename.push('_');
            continue;
        }

        typename.push_str("__");

        if c == '+' {
            typename.push_str("Plus");
        } else if c == '-' {
            typename.push_str("Less");
        } else if c == '*' {
            typename.push_str("Multiply");
        } else if c == '/' {
            typename.push_str("Divide");
        } else if c == '(' {
            typename.push_str("OpeningParenthesis");
        } else if c == ')' {
            typename.push_str("ClosingParenthesis");
        } else if c == '[' {
            typename.push_str("OpeningBracket");
        } else if c == ']' {
            typename.push_str("ClosingBracket");
        } else if c == '{' {
            typename.push_str("OpeningBrace");
        } else if c == '}' {
            typename.push_str("ClosingBrace");
        } else if c == '\\' {
            typename.push_str("Backslash");
        } else if c == '@' {
            typename.push_str("At");
        } else if c == '=' {
            typename.push_str("Equal");
        } else if c == '!' {
            typename.push_str("Bang");
        } else if c == '^' {
            typename.push_str("CircumflexAccent");
        } else if c == ',' {
            typename.push_str("Comma");
        } else if c == '.' {
            typename.push_str("Dot");
        } else if c == ';' {
            typename.push_str("SemiColon");
        } else {
            typename.push_str(&format!("Char{}", c as u8));
        }

        typename.push_str("__");
    }

    let counter = *cst_string_counters.get(cst_string).unwrap_or(&0);
    cst_string_counters.insert(cst_string, counter + 1);

    let ident = format_ident!(
        "Str{}_{}",
        if counter > 0 {
            counter.to_string()
        } else {
            String::new()
        },
        typename
    );
    quote! { #ident }
}
