use peggy::compiler::{parse_peg, pretty_format_parser_err};
use peggy::generators::rust::gen_rust_str;

static PRN_GRAMMAR: &str = r#"

S = _:B_WHITESPACE                          # Whitespace
DEC_SEP = _:("." | ",")                     # Decimal separator

int = B_ASCII_DIGIT+                        # Integer
float = int DEC_SEP int                     # Floating-point number
number = int | float                        # Number

operator = "+" | "-" | "*" | "/"            # Operator
operand = number | paren_expr               # Operand
operation = operand S+ operand S* operator  # Complete operation

paren_expr = _:"(" S* expr S* _:")"         # Expression wrapped between parenthesis
expr = number | operation | paren_expr      # Complete expression

main = expr                                 # Grammar's entrypoint

"#;

fn main() {
    // Parse the grammar
    let pst = parse_peg(PRN_GRAMMAR).unwrap_or_else(|err| {
        eprintln!("{}", pretty_format_parser_err(PRN_GRAMMAR, err));
        panic!("Failed to parse PRN grammar");
    });

    println!("{}", gen_rust_str(&pst));
}
