use peggy_macro::peggy_gen;

#[peggy_gen(filename = "../examples/float.peggy")]
mod float_grammar {}

static TEST_INPUT: &str = "12345.6789";

fn main() {
    // Evaluate the expression
    let success = float_grammar::exec(TEST_INPUT).unwrap_or_else(|err| {
        panic!(
            "Failed to match PRN grammar against a PRN expression: {:#?}",
            err
        );
    });

    // Extract the values
    let (int_part, dec_part) = success.matched.matched;

    println!("Input: {}", TEST_INPUT);
    println!("Integer part: {:?}", int_part.map(|num| num.matched));
    println!("Decimal part: {:?}", dec_part.map(|num| num.matched));
}
