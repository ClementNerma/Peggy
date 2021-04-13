use quote::{quote, format_ident};
use quote::__private::TokenStream;
use super::{InternalState, make_safe_ident, get_enum_variant};
use crate::compiler::data::*;
use crate::compiler::utils::*;

pub fn gen_rule_matcher<'a>(
    state: &mut InternalState<'a>,
    name: &'a str,
    rule: &'a Rule,
) -> TokenStream {
    let ident = make_safe_ident(name);

    let pattern_matcher = gen_pattern_matcher(state, name, rule.pattern());

    let body = if let Some(PatternMode::Silent) = state.non_capturing_rules.get(name) {
        quote! { #pattern_matcher }
    } else if name != GRAMMAR_ENTRYPOINT_RULE {
        quote! { #pattern_matcher.and_then(|(matched, consumed)| Ok((super::matched::#ident { matched, at: offset }, consumed))) }
    } else {
        quote! { #pattern_matcher.and_then(|(matched, consumed)| {
            if input.len() > consumed {
                Err(super::PegErrorContent::ExpectedEndOfInput.at(consumed))
            } else {
                Ok((super::matched::#ident { matched, at: offset }, consumed))
            }
        }) }
    };

    let ret_type = if let Some(PatternMode::Silent) = state.non_capturing_rules.get(name) {
        quote! { () }
    } else {
        quote! { super::matched::#ident }
    };

    let body_with_eventual_debugger = if let Some(mod_name) = &state.debugger {
        quote! {
            super::super::#mod_name::entering_rule(#name, input, offset);

            let base_input_for_str = input;
            let base_offset_for_str = offset;
            let result = #body;

            super::super::#mod_name::leaving_rule(#name, input, offset, result.clone().err());

            result
        }
    } else {
        quote! {
            let base_input_for_str = input;
            let base_offset_for_str = offset;
            #body
        }
    };

    quote! {
        pub fn #ident (input: &str, offset: usize) -> Result<(#ret_type, usize), super::PegError> {
            #body_with_eventual_debugger
        }
    }
}

pub fn gen_pattern_matcher<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    pattern: &'a Pattern,
) -> TokenStream {
    let matcher = gen_pattern_value_matcher(state, visiting, pattern.value());

    let matcher = match pattern.mode() {
            Some(PatternMode::Silent) => quote! { #matcher.map(|(_, consumed)| ((), consumed)) },
            Some(PatternMode::Peek) => quote! { #matcher.map(|(_, _)| ((), 0)) },
            Some(PatternMode::Negative) => quote! {{
                let result = #matcher;
                match result {
                    Ok(_) => Err(super::PegErrorContent::MatchedInNegativePattern.at(offset)),
                    Err(_) => Ok(((), 0))
                }
            }},
            Some(PatternMode::Atomic) => quote! { #matcher.map(|(_, consumed)| {
                (base_input_for_str[(offset - base_offset_for_str)..(offset - base_offset_for_str + consumed)].to_string(), consumed)
            }) },
            None => quote! { #matcher }
    };

    match pattern.repetition() {
        None => quote! { #matcher },
        Some(rep) => match rep {
            PatternRepetition::Any | PatternRepetition::OneOrMore => {
                let (storage, push_strategy, ret_val) = if pattern.is_dataless() {
                    (None, None, quote! { () })
                } else {
                    (Some(quote! { let mut out = vec![]; }), Some(quote! { out.push(piece_data); }), quote! { out })
                };

                let (init_var, init_set, err_handling) = if rep == PatternRepetition::Any {
                    (None, None, Some(quote! { Err(_) => break Ok((#ret_val, consumed)) }))
                } else {
                    (
                        Some(quote! { let mut one_success = false; }),
                        Some(quote! { one_success = true; }),
                        Some(quote! { Err(err) => break if one_success { Ok((#ret_val, consumed)) } else { Err(err) } })
                    )
                };

                quote! {
                    {
                        #storage
                        let mut input = input;
                        let mut consumed = 0;
                        let mut offset = offset;
                        #init_var

                        loop {
                            let result = #matcher;

                            match result {
                                Ok((piece_data, piece_consumed)) => {
                                    #init_set
                                    #push_strategy
                                    consumed += piece_consumed;
                                    offset += piece_consumed;
                                    input = &input[piece_consumed..];
                                },

                                #err_handling
                            }
                        }
                    }
                }
            },
            PatternRepetition::Optional => quote! {
                {
                    let result = #matcher;
                    match result {
                        Ok((data, consumed)) => Ok((Some(data), consumed)),
                        Err(_) => Ok((None, 0))
                    }
                }
            }
        }
    }
}

pub fn gen_pattern_value_matcher<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    value: &'a RulePatternValue,
) -> TokenStream {
    match value {
        RulePatternValue::CstString(string) => {
            let str_type = match state.cst_string_types.get(string) {
                Some(str_type) => quote! { super::strings::#str_type },

                // Happens when the parent pattern is silent
                None => quote! { () }
            };

            let str_len = string.len();

            quote! {
                if input.starts_with(#string) {
                    Ok((#str_type, #str_len))
                } else {
                    Err(super::PegErrorContent::ExpectedCstString(#string).at(offset))
                }
            }
        }
        RulePatternValue::Rule(name) => {
            if is_builtin_rule_name(name) {
                state.used_builtin_rules.insert(name);
                gen_builtin_matcher(name)
            } else {
                let ident = make_safe_ident(name);
                let ret_data = quote! { #ident (input, offset) };

                if state.recursive_paths[visiting].contains(name) {
                    quote! { #ret_data.map(|(data, consumed)| (std::rc::Rc::new(data), consumed)) }
                } else {
                    ret_data
                }
            }
        }
        RulePatternValue::Group(pattern) => {
            gen_pattern_matcher(state, visiting, pattern.as_ref())
        }
        RulePatternValue::Suite(patterns) => {
            let mut used = vec![];

            let create_storage: Vec<_> = patterns
                .iter()
                .enumerate()
                .map(|(i, pattern)| {
                    let matcher = gen_pattern_matcher(state, visiting, pattern);
                    
                    let mut is_dataless = pattern.is_dataless();

                    if !is_dataless {
                        if let RulePatternValue::Rule(name) = pattern.value() {
                            if let Some(PatternMode::Silent) = state.non_capturing_rules.get(name) {
                                is_dataless = true;
                            }
                        }
                    }

                    let storage = if is_dataless {
                        format_ident!("_")
                    } else {
                        used.push(format_ident!("p{}", i));
                        format_ident!("p{}", i)
                    };

                    quote! {
                        let result = #matcher;

                        let (#storage, piece_consumed) = match result {
                            Ok(result) => result,
                            Err(err) => break Err(err)
                        };
                        
                        offset += piece_consumed;
                        consumed += piece_consumed;
                        input = &input[piece_consumed..];
                    }
                })
                .collect();

            let ret_success_value = if used.len() == 1 {
                quote! { #(#used)* }
            } else {
                quote! { (#(#used,)*) }
            };

            quote! {
                // TODO: Find a less "hacky" way to achieve this
                loop {
                    let mut input = input;
                    let mut offset = offset;

                    let mut consumed = 0;

                    #(#create_storage)*

                    break Ok((#ret_success_value, consumed));
                }
            }
        }
        RulePatternValue::Union(patterns) => {
            let union_ident = format_ident!("Sw{}", patterns.len());

            let tries: Vec<_> = patterns
                .iter()
                .enumerate()
                .map(|(i, pattern)| {
                    let matcher = gen_pattern_matcher(state, visiting, pattern);
                    
                    let union_variant = format_ident!("{}", get_enum_variant(i));

                    quote! {
                        {
                            let result = #matcher;
                            
                            match result {
                                Ok((data, consumed)) => match candidate {
                                    Some((_, candidate_consumed)) => if consumed > candidate_consumed {
                                        candidate = Some((super::unions::#union_ident::#union_variant(data), consumed));
                                    },
                                    None => candidate = Some((super::unions::#union_ident::#union_variant(data), consumed))
                                },

                                Err(err) => errors.push(std::rc::Rc::new(err))
                            }
                        }
                    }
                })
                .collect();

            if tries.len() > state.highest_union_used {
                state.highest_union_used = tries.len();
            }

            quote! {
                {
                    let mut candidate = None;
                    let mut errors = vec![];
                    #(#tries)*

                    match candidate {
                        None => Err(super::PegErrorContent::NoMatchInUnion(errors).at(offset)),
                        Some((data, consumed)) => Ok((data, consumed))
                    }
                }
            }
        }
    }
}

pub fn gen_builtin_matcher(name: &str) -> TokenStream {
    let cond = match name {
        "B_ANY" => quote! { true },

        "B_NEWLINE_CR" => quote! { *nc == '\r' },
        "B_NEWLINE_LF" => quote! { *nc == '\n' },

        "B_DOUBLE_QUOTE" => quote! { *nc == '"' },

        "B_ASCII" => quote! { nc.is_ascii() },
        "B_ASCII_ALPHABETIC" => quote! { nc.is_ascii_alphabetic() },
        "B_ASCII_ALPHANUMERIC" => quote! { nc.is_ascii_alphanumeric() },
        "B_ASCII_CONTROL" => quote! { nc.is_ascii_control() },
        "B_ASCII_DIGIT" => quote! { nc.is_ascii_digit() },
        "B_ASCII_GRAPHIC" => quote! { nc.is_ascii_graphic() },
        "B_ASCII_HEXDIGIT" => quote! { nc.is_ascii_hexdigit() },
        "B_ASCII_LOWERCASE" => quote! { nc.is_ascii_lowercase() },
        "B_ASCII_PUNCTUATION" => quote! { nc.is_ascii_punctuation() },
        "B_ASCII_UPPERCASE" => quote! { nc.is_ascii_uppercase() },
        "B_ASCII_WHITESPACE" => quote! { nc.is_ascii_whitespace() },

        "B_ALPHABETIC" => quote! { nc.is_alphabetic() },
        "B_ALPHANUMERIC" => quote! { nc.is_alphanumeric() },
        "B_CONTROL" => quote! { nc.is_control() },
        "B_LOWERCASE" => quote! { nc.is_lowercase() },
        "B_NUMERIC" => quote! { nc.is_numeric() },
        "B_UPPERCASE" => quote! { nc.is_uppercase() },
        "B_WHITESPACE" => quote! { nc.is_whitespace() },
        
        _ => unreachable!()
    };

    let name_ident = format_ident!("{}", name);

    quote! {
        match input.chars().next().filter(|nc| #cond) {
            None => Err(super::PegErrorContent::FailedToMatchBuiltinRule(#name).at(offset)),
            Some(c) => Ok((super::matched::#name_ident { matched: c, at: offset }, 1))
        }
    }
}
