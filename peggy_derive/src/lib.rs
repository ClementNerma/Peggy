#![forbid(unsafe_code)]
#![forbid(unused_must_use)]

use lazy_static::lazy_static;
use peggy::compiler::{parse_peg, pretty_format_parser_err};
use peggy::generators::rust::gen_rust_token_stream;
use proc_macro::TokenStream;
use quote::quote;
use regex::Regex;
use std::env;
use std::fs;
use std::path::PathBuf;
use syn::{Ident, ItemMod, Visibility};

lazy_static! {
    static ref FILENAME_ATTR: Regex = Regex::new("^filename\\s*=\\s*\"(.*)\"$").unwrap();
}

#[proc_macro_attribute]
pub fn peggy_grammar(attr: TokenStream, item: TokenStream) -> TokenStream {
    let (mod_ident, mod_vis) = parse_input_mod(item);

    let grammar_file = parse_filename_attr(attr);

    if !grammar_file.exists() {
        panic!("Grammar file was not found at path (tip: path starts from your crate's \"src\" directory)");
    }

    let grammar_src = fs::read_to_string(&grammar_file).expect("Provided file could not be read");

    let generated_rust = grammar_to_rust(&grammar_src);

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

fn parse_filename_attr(attr: TokenStream) -> PathBuf {
    let attr = attr.to_string();

    let captured = FILENAME_ATTR.captures(&attr).expect(
        "Please provide a grammar file path under the form: #[peggy_grammar(filename = \"<path>\")]",
    );
    let filename = captured.get(1).unwrap().as_str();

    let mut base_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    base_path.push("src");
    base_path.push(filename);

    base_path
}

fn grammar_to_rust(grammar_src: &str) -> quote::__private::TokenStream {
    let grammar = parse_peg(&grammar_src).unwrap_or_else(|err| {
        panic!(
            "Failed to parse grammar: {}",
            pretty_format_parser_err(&grammar_src, err)
        )
    });

    gen_rust_token_stream(&grammar)
}
