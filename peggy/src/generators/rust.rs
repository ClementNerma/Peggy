use crate::compiler::*;
use crate::compiler::utils::{is_builtin_rule_name, is_external_rule_name, is_valid_builtin_rule_name};
use quote::__private::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};

pub fn gen_rust_str(pst: &PegSyntaxTree) -> String {
    gen_rust_token_stream(pst).to_string()
}

pub fn gen_rust_token_stream(pst: &PegSyntaxTree) -> TokenStream {
    let mut state = InternalState {
        recursive_paths: find_recursive_rules(pst),
        cst_string_types: HashMap::new(),
        cst_string_counters: HashMap::new(),
        used_builtin_rules: HashSet::new(),
        rule_types: HashMap::new(),
        silent_rules: list_silent_rules(pst),
        highest_union_used: 0,
    };

    for name in pst.rules().keys() {
        if !state.recursive_paths.contains_key(name) {
            state.recursive_paths.insert(name, HashSet::new());
        }
    }

    let mut rule_types: Vec<_> = pst
        .rules()
        .iter()
        .filter_map(|(name, content)| {
            let ident = make_safe_ident(name);

            let rule_type = gen_rust_rule_type(&mut state, name, content);

            state.rule_types.insert(*name, rule_type.clone());

            let rule_type = rule_type?;

            Some(quote! {
                #[derive(Debug, Clone)]
                pub struct #ident {
                    pub matched: #rule_type,
                    pub at: usize
                }
            })
        })
        .collect();

    rule_types.sort_by_key(|t| t.to_string());

    let mut rule_types_enum_variants: Vec<_> = pst
        .rules()
        .iter()
        .filter(|(name, _)| !state.silent_rules.contains(*name))
        .map(|(name, _)| {
            let variant = make_safe_ident(name);
            quote! { #variant(super::matched::#variant) }
        })
        .collect();

    rule_types_enum_variants.sort_by_key(|t| t.to_string());

    let mut cst_string_types_expanded: Vec<_> = state.cst_string_types
        .iter()
        .map(|(string, typename)| quote! {
            #[doc = #string]
            #[derive(Debug, Clone)]
            // Original string: #string
            pub struct #typename;
        })
        .collect();

    cst_string_types_expanded.sort_by_key(|t| t.to_string());

    let mut rules: Vec<_> = pst
        .rules()
        .iter()
        .map(|(name, content)| gen_rust_rule_matcher(&mut state, name, content))
        .collect();

    rules.sort_by_key(|t| t.to_string());    

    let mut builtin_rules: Vec<_> = state.used_builtin_rules
        .iter()
        .map(|name| {
            let ident = format_ident!("{}", name);
            quote! {
                #[derive(Debug, Clone)]
                pub struct #ident {
                    pub matched: char,
                    pub at: usize
                }
            }
        })
        .collect();

    builtin_rules.sort_by_key(|t| t.to_string());

    let unions = (2..=state.highest_union_used).map(|i| {
        let generics: Vec<_> = (0..i).map(|i| format_ident!("{}", get_enum_variant(i))).collect();
        let variants: Vec<_> = (0..i).map(|i| format_ident!("{}", get_enum_variant(i))).collect();
        let ident = format_ident!("Sw{}", i);

        quote! {
            #[derive(Debug, Clone)]
            pub enum #ident<#(#generics),*> {
                #(#variants (#generics),)*
            }
        }
    });

    let no_linting = quote! {
        #[allow(clippy::all)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
    };

    let main_rule = format_ident!("{}", GRAMMAR_ENTRYPOINT_RULE);

    quote! {
        pub fn exec(input: &str) -> Result<SuccessData, PegError> {
            rules::#main_rule(input, 0).map(|(data, _)| data)
        }

        pub type SuccessData = matched::#main_rule;

        #[derive(Debug, Clone)]
        pub struct PegError<'a> {
            pub offset: usize,
            pub content: PegErrorContent<'a>
        }

        #[derive(Debug, Clone)]
        pub enum PegErrorContent<'a> {
            ExpectedCstString(&'a str),
            FailedToMatchBuiltinRule(&'static str),
            NoMatchInUnion(Vec<std::rc::Rc<PegError<'a>>>),
            ExpectedEndOfInput
        }

        impl<'a> PegErrorContent<'a> {
            fn at(self, offset: usize) -> PegError<'a> {
                PegError { offset, content: self }
            }
        }

        #no_linting
        pub mod matched {
            #[derive(Debug, Clone)]
            pub enum MatchedRule {
                #(#rule_types_enum_variants),*
            }

            #(#rule_types)*
            #(#builtin_rules)*
        }

        #no_linting
        pub mod rules {
            #(#rules)*
        }

        #no_linting
        pub mod strings {
            #(#cst_string_types_expanded)*
        }

        pub mod unions {
            #(#unions)*
        }
    }
}

pub fn find_recursive_rules<'a>(pst: &'a PegSyntaxTree) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut rec = HashMap::new();
    find_recursive_rules_in(pst, &mut vec![], &mut rec, GRAMMAR_ENTRYPOINT_RULE);
    rec
}

pub fn find_recursive_rules_in<'a>(pst: &'a PegSyntaxTree, path: &mut Vec<&'a str>, treated_recursives: &mut HashMap<&'a str, HashSet<&'a str>>, rule_name: &'a str) {
    if is_valid_builtin_rule_name(rule_name) || is_external_rule_name(rule_name) {
        return
    }

    path.push(rule_name);

    let rule = pst.rules().get(rule_name).unwrap();
    build_rules_list(pst, path, treated_recursives, rule.pattern().value());

    path.pop();
}

pub fn build_rules_list<'a>(pst: &'a PegSyntaxTree, path: &mut Vec<&'a str>, treated_recursives: &mut HashMap<&'a str, HashSet<&'a str>>, pattern_value: &'a RulePatternValue) {
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
                find_recursive_rules_in(pst, path, treated_recursives, name);
            }
        }
        RulePatternValue::Group(pattern) => build_rules_list(pst, path, treated_recursives, pattern.value()),
        RulePatternValue::Suite(patterns) | RulePatternValue::Union(patterns) => {
            for pattern in patterns {
                build_rules_list(pst, path, treated_recursives, pattern.value());
            }
        }
    }
}

pub fn list_silent_rules<'a>(pst: &'a PegSyntaxTree) -> HashSet<&'a str> {
    let mut silent_rules = HashSet::new();
    
    for name in pst.rules().keys() {
        check_rule_silence(pst, &mut silent_rules, name);
    }

    silent_rules
}

pub fn check_rule_silence<'a>(pst: &'a PegSyntaxTree, silent_rules: &mut HashSet<&'a str>, name: &'a str) -> bool {
    if silent_rules.contains(&name) {
        return true;
    }
    
    if is_builtin_rule_name(name) {
        false
    } else if is_silent_pattern(pst, silent_rules, pst.rules()[name].pattern()) {
        silent_rules.insert(name);
        true
    } else {
        false
    }
}

pub fn is_silent_pattern<'a>(pst: &'a PegSyntaxTree, silent_rules: &mut HashSet<&'a str>, pattern: &'a Pattern) -> bool {
    if pattern.is_silent() {
        return true;
    }

    match pattern.value() {
        RulePatternValue::CstString(_) => false,
        RulePatternValue::Rule(name) => check_rule_silence(pst, silent_rules, name),
        RulePatternValue::Group(group) => is_silent_pattern(pst, silent_rules, group),
        RulePatternValue::Suite(patterns) => patterns.iter().all(|pattern| is_silent_pattern(pst, silent_rules, pattern)),
        RulePatternValue::Union(patterns) => patterns.iter().all(|pattern| is_silent_pattern(pst, silent_rules, pattern))
    }
}

pub fn gen_rust_rule_type<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    rule: &'a RuleContent,
) -> Option<TokenStream> {
    gen_rust_rule_pattern_type(state, visiting, rule.pattern())
}

pub fn gen_rust_rule_pattern_type<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    pattern: &'a Pattern,
) -> Option<TokenStream> {
    if pattern.is_silent() {
        return None;
    }

    let pattern_type =
        gen_rust_rule_pattern_value_type(state,  visiting, pattern.value())?;

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

pub fn gen_rust_rule_pattern_value_type<'a>(
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
            } else if state.silent_rules.contains(name) {
                None
            } else if state.recursive_paths[visiting].contains(name) {
                Some(quote! { std::rc::Rc<super::matched::#ident> })
            } else {
                Some(quote! { super::matched::#ident })
            }
        }
        RulePatternValue::Group(inner) => {
            gen_rust_rule_pattern_type(state, visiting, inner.as_ref())
        }
        RulePatternValue::Suite(patterns) => {
            let types: Vec<_> = patterns
                .iter()
                .map(|pattern| gen_rust_rule_pattern_type(state, visiting, pattern))
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
                    gen_rust_rule_pattern_type(state, visiting,pattern)
                })
                .filter_map(|pattern| pattern)
                .collect();

            let variants_len = patterns.len();

            let union_type = format_ident!("Sw{}", variants_len);

            if patterns.is_empty() {
                None
            } else {
                if patterns.len() > state.highest_union_used {
                    state.highest_union_used = patterns.len();
                }

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

pub fn make_safe_ident(ident: &str) -> Ident {
    if RUST_RESERVED_KEYWORDS.contains(&ident) {
        format_ident!("r#{}", ident)
    } else {
        format_ident!("{}", ident)
    }
}

pub static RUST_RESERVED_KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try", "union", "static", "dyn",
];

pub fn gen_rust_rule_matcher<'a>(
    state: &mut InternalState<'a>,
    name: &'a str,
    rule: &'a RuleContent,
) -> TokenStream {
    let ident = make_safe_ident(name);

    let pattern_matcher = gen_rust_rule_pattern_matcher(state, name, rule.pattern());

    let body = if state.silent_rules.contains(name) /*|| state.rule_types[name].is_none()*/ {
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

    let ret_type = if state.silent_rules.contains(name) {
        quote! { () }
    } else {
        quote! { super::matched::#ident }
    };

    quote! {
        pub fn #ident (input: &str, offset: usize) -> Result<(#ret_type, usize), super::PegError> {
            #body
        }
    }
}

pub fn gen_rust_rule_pattern_matcher<'a>(
    state: &mut InternalState<'a>,
    visiting: &'a str,
    pattern: &'a Pattern,
) -> TokenStream {
    let matcher = gen_rust_rule_pattern_value_matcher(state, visiting, pattern.value());

    let matcher = if pattern.is_silent() {
        quote! { #matcher.map(|(_, consumed)| ((), consumed)) }
    } else {
        quote! { #matcher }
    };

    match pattern.repetition() {
        None => quote! { #matcher },
        Some(rep) => match rep {
            PatternRepetition::Any => {
                let push_strategy = if pattern.is_silent() {
                    quote! { }
                } else {
                    quote! { out.push(piece_data); }
                };

                quote! {
                    {
                        let mut out = vec![];
                        let mut consumed = 0;
                        let mut input = input;

                        loop {
                            let result = #matcher;

                            match result {
                                Ok((piece_data, piece_consumed)) => {
                                    #push_strategy
                                    consumed += piece_consumed;
                                    input = &input[piece_consumed..];
                                },

                                Err(_) => break Ok((out, consumed))
                            }
                        }
                    }
                }
            },

            PatternRepetition::OneOrMore => {
                let push_strategy = if pattern.is_silent() {
                    quote! { }
                } else {
                    quote! { out.push(piece_data); }
                };

                quote! {
                    {
                        let mut out = vec![];
                        let mut consumed = 0;
                        let mut input = input;
                        let mut one_success = false;

                        loop {
                            let result = #matcher;

                            match result {
                                Ok((piece_data, piece_consumed)) => {
                                    #push_strategy
                                    input = &input[piece_consumed..];
                                    consumed += piece_consumed;
                                    one_success = true;
                                },

                                Err(err) => {
                                    break if one_success {
                                        Ok((out, consumed))
                                    } else {
                                        Err(err)
                                    }
                                }
                            }
                        }
                    }
                }
            }
            PatternRepetition::Optional => quote! {
                {
                    let result = #matcher;
                    match result {
                        Ok(data) => Ok(Some(data)),
                        Err(_) => Ok(None)
                    }
                }
            }
        }
    }
}

pub fn gen_rust_rule_pattern_value_matcher<'a>(
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
            gen_rust_rule_pattern_matcher(state, visiting, pattern.as_ref())
        }
        RulePatternValue::Suite(patterns) => {
            let mut used = vec![];

            let create_storage: Vec<_> = patterns
                .iter()
                .enumerate()
                .map(|(i, pattern)| {
                    let matcher = gen_rust_rule_pattern_matcher(state, visiting, pattern);
                    
                    let is_silent = pattern.is_silent() || matches!(pattern.value(), RulePatternValue::Rule(name) if state.silent_rules.contains(name));

                    let mut storage = format_ident!("p{}", i);

                    if is_silent {
                        storage = format_ident!("_");
                    } else {
                        used.push(storage.clone());
                    }

                    quote! {
                        let (#storage, piece_consumed) = match #matcher {
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

                    // TODO: here too
                    offset -= consumed;

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
                    let matcher = gen_rust_rule_pattern_matcher(state, visiting, pattern);
                    
                    let union_variant = format_ident!("{}", get_enum_variant(i));

                    quote! {
                        match #matcher {
                            Ok((data, consumed)) => match candidate {
                                Some((_, candidate_consumed)) => if consumed > candidate_consumed {
                                    candidate = Some((super::unions::#union_ident::#union_variant(data), consumed));
                                },
                                None => candidate = Some((super::unions::#union_ident::#union_variant(data), consumed))
                            },

                            Err(err) => errors.push(std::rc::Rc::new(err))
                        }
                    }
                })
                .collect();

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
        "B_ANY" => quote! { nc.is_some() },

        "B_NEWLINE_CR" => quote! { nc == '\r' },
        "B_NEWLINE_LF" => quote! { nc == '\n' },

        "B_DOUBLE_QUOTE" => quote! { nc == '"' },

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

pub fn get_enum_variant(mut i: usize) -> String {
    if i == 0 {
        return "A".to_string();
    }

    const ALPHABET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut out = String::new();

    while i > 0 {
        let rem = i % 26;
        out.push(ALPHABET.chars().nth(rem).unwrap());
        i = (i - rem) / 26;
    }

    out.chars().rev().collect()
}

pub struct InternalState<'a> {
    recursive_paths: HashMap<&'a str, HashSet<&'a str>>,
    cst_string_types: HashMap<&'a str, TokenStream>,
    cst_string_counters: HashMap<&'a str, usize>,
    used_builtin_rules: HashSet<&'a str>,
    rule_types: HashMap<&'a str, Option<TokenStream>>,
    silent_rules: HashSet<&'a str>,
    highest_union_used: usize,
}
