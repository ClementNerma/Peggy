mod matchers;
mod non_capturing_pat;
mod recursive_rules;
mod rules_lifetime_reqs;
mod types;

use crate::grammar::*;
use quote::__private::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};

pub static RUST_RESERVED_KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try", "union", "static", "dyn",
];

pub struct InternalState<'a> {
    recursive_paths: HashMap<&'a str, HashSet<&'a str>>,
    cst_string_types: HashMap<&'a str, TokenStream>,
    cst_string_counters: HashMap<&'a str, usize>,
    used_builtin_rules: HashSet<&'a str>,
    rule_types: HashMap<&'a str, Option<TokenStream>>,
    non_capturing_rules: HashMap<&'a str, PatternMode>,
    rules_with_lifetime: HashSet<&'a str>,
    highest_union_used: usize,
    debugger: Option<Ident>,
}

pub fn gen_rust_str(pst: &PegSyntaxTree, debugger: Option<&str>) -> String {
    gen_rust_token_stream(pst, debugger).to_string()
}

pub fn gen_rust_token_stream(pst: &PegSyntaxTree, debugger: Option<&str>) -> TokenStream {
    let mut state = InternalState {
        recursive_paths: recursive_rules::find(pst),
        cst_string_types: HashMap::new(),
        cst_string_counters: HashMap::new(),
        used_builtin_rules: HashSet::new(),
        rule_types: HashMap::new(),
        non_capturing_rules: non_capturing_pat::list_rules(pst),
        rules_with_lifetime: rules_lifetime_reqs::build_lifetime_reqs(pst),
        highest_union_used: 0,
        debugger: debugger.map(|mod_name| format_ident!("{}", mod_name)),
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

            let rule_type = types::gen_rule_type(&mut state, name, content);

            state.rule_types.insert(*name, rule_type.clone());

            let rule_type = rule_type?;

            let lifetime_req = if state.rules_with_lifetime.contains(name) {
                quote! { <'a> }
            } else {
                quote! {}
            };

            Some(quote! {
                #[derive(Debug, Clone, PartialEq, Eq)]
                pub struct #ident #lifetime_req {
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
        .filter(|(name, _)| state.non_capturing_rules.get(*name) != Some(&PatternMode::Silent))
        .map(|(name, _)| {
            let variant = make_safe_ident(name);

            let lifetime_req = if state.rules_with_lifetime.contains(name) {
                quote! { <'a> }
            } else {
                quote! {}
            };

            quote! { #variant(super::matched::#variant #lifetime_req) }
        })
        .collect();

    rule_types_enum_variants.sort_by_key(|t| t.to_string());

    let mut cst_string_types_expanded: Vec<_> = state
        .cst_string_types
        .iter()
        .map(|(string, typename)| {
            quote! {
                #[doc = #string]
                #[derive(Debug, Clone, PartialEq, Eq)]
                // Original string: #string
                pub struct #typename;
            }
        })
        .collect();

    cst_string_types_expanded.sort_by_key(|t| t.to_string());

    let mut rules: Vec<_> = pst
        .rules()
        .iter()
        .map(|(name, content)| matchers::gen_rule_matcher(&mut state, name, content))
        .collect();

    rules.sort_by_key(|t| t.to_string());

    let mut builtin_rules: Vec<_> = state
        .used_builtin_rules
        .iter()
        .map(|name| {
            let ident = format_ident!("{}", name);
            quote! {
                #[derive(Debug, Clone, PartialEq, Eq)]
                pub struct #ident {
                    pub matched: char,
                    pub at: usize
                }
            }
        })
        .collect();

    builtin_rules.sort_by_key(|t| t.to_string());

    let unions = (2..=state.highest_union_used).map(|i| {
        let variants: Vec<_> = (0..i)
            .map(|i| format_ident!("{}", get_enum_variant(i)))
            .collect();

        let mappers: Vec<_> = (0..i)
            .map(|i| format_ident!("mapper_{}", get_enum_variant(i)))
            .collect();

        let ident = format_ident!("Sw{}", i);

        quote! {
            #[derive(Debug, Clone, PartialEq, Eq)]
            pub enum #ident<#(#variants),*> {
                #(#variants (#variants),)*
            }

            impl<#(#variants,)*> #ident<#(#variants,)*> {
                pub fn variants<Mapped>(self, #(#mappers: impl FnOnce(#variants) -> Mapped),*) -> Mapped {
                    match self {
                        #(Self::#variants(v) => #mappers(v)),*
                    }
                }

                pub fn variants_ref<Mapped>(&self, #(#mappers: impl FnOnce(&#variants) -> Mapped),*) -> Mapped {
                    match &self {
                        #(Self::#variants(v) => #mappers(v)),*
                    }
                }
            }
        }
    });

    let no_linting = quote! {
        #[allow(clippy::all)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
    };

    let main_rule = format_ident!("{}", GRAMMAR_ENTRYPOINT_RULE);

    let err_formatter_impl_ts = err_formatter_impl();

    let (global_lifetime_req, global_lifetime_name) = if state.rules_with_lifetime.is_empty() {
        (quote! {}, quote! {})
    } else {
        (quote! { <'a> }, quote! { 'a })
    };

    quote! {
        pub fn exec #global_lifetime_req (input: & #global_lifetime_name str) -> Result<SuccessData #global_lifetime_req, PegError> {
            rules::#main_rule(input, input, 0)
                .and_then(|(typed_matched, consumed, end_err)| {
                    if input.len() > consumed {
                        Err(end_err.unwrap_or_else(|| PegErrorContent::ExpectedEndOfInput.at(input, consumed, #GRAMMAR_ENTRYPOINT_RULE)))
                    } else {
                        Ok(typed_matched)
                    }
                })
        }

        pub type SuccessData #global_lifetime_req = matched::#main_rule #global_lifetime_req;

        #[derive(Debug, Clone)]
        pub struct PegError<'a> {
            pub source: &'a str,
            pub offset: usize,
            pub content: PegErrorContent<'a>,
            pub rule: &'static str,
        }

        impl<'a> PegError<'a> {
            fn in_rule(mut self, rule: &'static str) -> Self {
                self.rule = rule;
                self
            }

            pub fn deepest(&self) -> &PegError<'a> {
                match &self.content {
                    PegErrorContent::ExpectedCstString(_)
                        | PegErrorContent::FailedToMatchBuiltinRule(_, _)
                        | PegErrorContent::MatchedInNegativePattern(_)
                        | PegErrorContent::ExpectedEndOfInput => self,

                    PegErrorContent::NoMatchInUnion(errors) => {
                        let mut deepest = errors[0].deepest();

                        for err in errors.iter().skip(1) {
                            let err = err.deepest();

                            if err.offset > deepest.offset {
                                deepest = err;
                            }
                        }

                        deepest
                    }
                }
            }
        }

        #[derive(Debug, Clone)]
        pub enum PegErrorContent<'a> {
            ExpectedCstString(&'a str),
            FailedToMatchBuiltinRule(&'static str, Option<char>),
            NoMatchInUnion(Vec<std::rc::Rc<PegError<'a>>>),
            MatchedInNegativePattern(&'a str),
            ExpectedEndOfInput
        }

        impl<'a> PegErrorContent<'a> {
            fn at(self, source: &'a str, offset: usize, rule: &'static str) -> PegError<'a> {
                PegError { source, offset, rule, content: self }
            }
        }

        #err_formatter_impl_ts

        #no_linting
        pub mod matched {
            #[derive(Debug, Clone, PartialEq, Eq)]
            pub enum MatchedRule #global_lifetime_req {
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

fn err_formatter_impl() -> TokenStream {
    quote! {
        impl<'a> std::fmt::Display for PegError<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                // Get all lines above the one the error is located on
                let head_lines_count = self.source[..self.offset].lines().count();

                // Deduce from it from the error line's number
                let line_index = if head_lines_count > 0 {
                    head_lines_count - 1
                } else {
                    0
                };

                // Get the length of the lines above the one the error is located on
                let head_lines_len = self
                    .source
                    .lines()
                    .take(line_index)
                    .fold(0, |acc, value| acc + value.len() + 1 /* TODO: Proper line jump counting */);

                let line = if line_index < self.source.lines().count() {
                    self.source.lines().nth(line_index).unwrap()
                } else {
                    ""
                };

                // Deduce from it the column of the error
                let column = self.offset - head_lines_len;

                // Produce a padding
                let padding =
                    " ".repeat(line[..column].chars().count() + (line_index + 1).to_string().len() + 3);

                // Do the formatting
                write!(
                    f,
                    "ERROR: While matching rule [{}] at line {}, column {}: \n\n{} | {}\n{}^{}",
                    self.rule,
                    line_index + 1,
                    column + 1,
                    line_index + 1,
                    self.source.lines().nth(line_index).unwrap(),
                    padding,
                    format!("{}", self.content)
                        .lines()
                        .map(|l| format!("\n{}{}", padding, l))
                        .collect::<String>()
                )
            }
        }

        impl<'a> std::fmt::Display for PegErrorContent<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match &self {
                    PegErrorContent::ExpectedCstString(string) => {
                        write!(f, "Expected constant string: {}", string)
                    }
                    PegErrorContent::FailedToMatchBuiltinRule(rule, Some(c)) => {
                        write!(f, "Failed to match builtin rule [{}]: found character [{}]", rule, c)
                    }
                    PegErrorContent::FailedToMatchBuiltinRule(rule, None) => {
                        write!(f, "Failed to match biultin rule [{}]: reached end of input", rule)
                    }
                    PegErrorContent::NoMatchInUnion(matches) => write!(
                        f,
                        "Failed to match in union: {}",
                        matches
                            .iter()
                            .enumerate()
                            .map(|(i, err)| {
                                let prefix = format!("  Variant {}: ", i + 1);
                                let padding = " ".repeat(prefix.len());
                                format!(
                                    "\n\n{}{}",
                                    prefix,
                                    format!("{}", err)
                                        .lines()
                                        .enumerate()
                                        .map(|(i, l)| {
                                            if i == 0 {
                                                l.to_string()
                                            } else {
                                                format!("\n{}{}", padding, l)
                                            }
                                        })
                                        .collect::<String>()
                                )
                            })
                            .collect::<String>()
                    ),
                    PegErrorContent::MatchedInNegativePattern(neg) => write!(
                        f,
                        "Matched content in negative pattern: {}",
                        neg.lines().next().unwrap_or("")
                    ),
                    PegErrorContent::ExpectedEndOfInput => write!(f, "Expected end of input"),
                }
            }
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

pub fn make_safe_ident(ident: &str) -> Ident {
    if RUST_RESERVED_KEYWORDS.contains(&ident) {
        format_ident!("r#{}", ident)
    } else {
        format_ident!("{}", ident)
    }
}
