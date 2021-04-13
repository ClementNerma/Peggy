use peggy::compiler::parse_peg;
use peggy::runtime::{execute, RuntimeContext, RuntimeOptions};

static GRAMMAR: &str = r#"main = "["+ E_LETTER_A* "]"+"#;

fn main() {
    let grammar = parse_peg(GRAMMAR).unwrap();

    let exec = |subject: &'static str| {
        execute(&RuntimeContext {
            grammar: &grammar,
            external_rules: Some(Box::new(|rule, input| match rule {
                "E_LETTER_A" => Some(match input.chars().next() {
                    Some('a') | Some('A') => Ok(1),
                    Some(_) => Err("Next character is not the 'A' letter".to_string()),
                    None => Err("Remaining input is empty".to_string()),
                }),
                _ => None,
            })),
            subject,
            options: RuntimeOptions::new(),
        })
    };

    assert!(exec("[]").is_ok());
    assert!(exec("[[a]]").is_ok());
    assert!(exec("[[A]]").is_ok());
    assert!(exec("[[aA]]").is_ok());
    assert!(exec("[[Aa]]").is_ok());
    assert!(exec("[[aAa]]").is_ok());
    assert!(exec("[[AaA]]").is_ok());

    assert!(exec("[[b]]").is_err());
}
