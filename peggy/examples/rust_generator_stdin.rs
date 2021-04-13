use io::Read;
use peggy::compiler::{parse_peg, pretty_format_parser_err};
use peggy::generators::rust::gen_rust_str;
use std::io;

fn main() {
    // Read the input grammar
    let mut buffer = String::new();
    io::stdin()
        .read_to_string(&mut buffer)
        .expect("Failed to read from STDIN");

    // Parse the grammar
    let pst = parse_peg(&buffer).unwrap_or_else(|err| {
        eprintln!("{}", pretty_format_parser_err(&buffer, err));
        panic!("Failed to parse PRN grammar");
    });

    println!("{}", gen_rust_str(&pst, None));
}
