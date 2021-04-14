#![forbid(unsafe_code)]
#![forbid(unused_must_use)]

use lazy_static::lazy_static;
use peggy::grammar::{parse_peg, pretty_format_parser_err};
use peggy::rustgen::gen_rust_token_stream;
use proc_macro::TokenStream;
use quote::quote;
use regex::Regex;
use std::env;
use std::fs;
use std::path::PathBuf;
use syn::{Ident, ItemMod, Visibility};

lazy_static! {
    static ref ATTR_CONTENT: Regex = Regex::new(
        "^filename\\s*=\\s*\"(?P<filename>[^\"]+)\"(?:,\\s*(debugger\\s*=\\s*\"(?P<debugger>[^\"]+)\"))?$"
    ).unwrap();
}

/// Options decoded from the attribute
/// Input file (grammar)
struct Options {
    grammar_file: PathBuf,

    /// Debugger function path
    debugger: Option<String>,
}

#[proc_macro_attribute]
pub fn peggy_gen(attr: TokenStream, item: TokenStream) -> TokenStream {
    let (mod_ident, mod_vis) = parse_input_mod(item);

    let options = parse_options_attr(attr);

    if !options.grammar_file.exists() {
        panic!("Grammar file was not found at path (tip: path starts from your crate's \"src\" directory)");
    }

    let generated_rust = grammar_to_rust(&options);

    let expanded = quote! {
        #mod_vis mod #mod_ident {
            #generated_rust
        }
    };

    TokenStream::from(expanded)
}

fn parse_input_mod(item: TokenStream) -> (Ident, Visibility) {
    let item = syn::parse::<ItemMod>(item).unwrap_or_else(|_| {
        panic!("This macro must be used on a module which will be filled with the grammar's data")
    });

    let mod_ident = item.ident;

    let mod_content = item.content.unwrap_or_else(|| {
        panic!(
            "This macro must be used on an inline, empty module (e.g. 'mod {} {{}}')",
            mod_ident
        )
    });

    if !mod_content.1.is_empty() {
        panic!(
            "This macro must be used on an inline, empty module (e.g. 'mod {} {{}}')",
            mod_ident
        )
    }

    (mod_ident, item.vis)
}

fn parse_options_attr(attr: TokenStream) -> Options {
    let attr = attr.to_string();

    let captured = ATTR_CONTENT.captures(&attr).expect(
        "Please provide a grammar file path under the form: #[peggy_grammar(filename = \"<path>\")] ('debugger' may be added with the same syntax)",
    );
    let filename = captured.name("filename").unwrap();

    let mut grammar_file = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    grammar_file.push("src");
    grammar_file.push(filename.as_str());

    Options {
        grammar_file,
        debugger: captured.name("debugger").map(|m| m.as_str().to_string()),
    }
}

fn grammar_to_rust(options: &Options) -> quote::__private::TokenStream {
    let grammar_src =
        fs::read_to_string(&options.grammar_file).expect("Provided file could not be read");

    let grammar = parse_peg(&grammar_src).unwrap_or_else(|err| {
        panic!(
            "Failed to parse grammar: {}",
            pretty_format_parser_err(&grammar_src, err)
        )
    });

    gen_rust_token_stream(&grammar, options.debugger.as_deref())
}
