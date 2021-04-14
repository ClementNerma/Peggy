use peggy_macro::peggy_gen;

#[peggy_gen(filename = "../examples/json.peggy")]
pub mod json_grammar {}

static TEST_INPUT: &str = r#"{

    "key": "value"

}"#;

fn main() {
    println!("Expression: {}", TEST_INPUT);

    // Evaluate the expression
    let success = json_grammar::exec(TEST_INPUT).unwrap_or_else(|err| {
        panic!("Failed to match input against RPN grammar:\n{}", err);
    });

    println!("{:#?}", success);
}
