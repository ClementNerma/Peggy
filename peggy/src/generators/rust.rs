mod gen_matchers;
mod gen_types;
mod non_capturing;
mod recursive_rules;

use crate::compiler::*;
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
        non_capturing_rules: non_capturing::list_rules(pst),
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

            let rule_type = gen_types::gen_rule_type(&mut state, name, content);

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
        .filter(|(name, _)| state.non_capturing_rules.get(*name) != Some(&PatternMode::Silent))
        .map(|(name, _)| {
            let variant = make_safe_ident(name);
            quote! { #variant(super::matched::#variant) }
        })
        .collect();

    rule_types_enum_variants.sort_by_key(|t| t.to_string());

    let mut cst_string_types_expanded: Vec<_> = state
        .cst_string_types
        .iter()
        .map(|(string, typename)| {
            quote! {
                #[doc = #string]
                #[derive(Debug, Clone)]
                // Original string: #string
                pub struct #typename;
            }
        })
        .collect();

    cst_string_types_expanded.sort_by_key(|t| t.to_string());

    let mut rules: Vec<_> = pst
        .rules()
        .iter()
        .map(|(name, content)| gen_matchers::gen_rule_matcher(&mut state, name, content))
        .collect();

    rules.sort_by_key(|t| t.to_string());

    let mut builtin_rules: Vec<_> = state
        .used_builtin_rules
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
        let generics: Vec<_> = (0..i)
            .map(|i| format_ident!("{}", get_enum_variant(i)))
            .collect();
        let variants: Vec<_> = (0..i)
            .map(|i| format_ident!("{}", get_enum_variant(i)))
            .collect();
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
            MatchedInNegativePattern,
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
