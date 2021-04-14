//! This file demonstrates how to make a simple Reverse Polish Notation (RPN) parser and evaluator using Peggy

use peggy::compiler::{parse_peg, pretty_format_parser_err};
use peggy::runtime::{execute, MatchedData, MatchedRule, RuntimeContext, RuntimeOptions};
use std::time::Instant;

static RPN_GRAMMAR: &str = r#"

S = °B_WHITESPACE                           # Whitespace
DEC_SEP = °("." | ",")                      # Decimal separator

int = @(B_ASCII_DIGIT+)                     # Integer
float = int DEC_SEP int                     # Floating-point number
number = int | float                        # Number

operator = "+" | "-" | "*" | "/"            # Operator
operand = number | paren_expr               # Operand
operation = operand S+ operand S* operator  # Complete operation

paren_expr = °"(" S* expr S* °")"           # Expression wrapped between parenthesis
expr = number | operation | paren_expr      # Complete expression

main = expr                                 # Grammar's entrypoint

"#;

static TEST_SUBJECT: &str = r"(3 (9.3 3 /) +) (5 (2 3 /) /) /";

fn main() {
    // Measure performances
    let now = Instant::now();

    // Parse the grammar
    let pst = parse_peg(RPN_GRAMMAR).unwrap_or_else(|err| {
        eprintln!("{}", pretty_format_parser_err(RPN_GRAMMAR, err));
        panic!("Failed to parse RPN grammar");
    });

    // Get elapsed time
    let elapsed = now.elapsed();

    // Display performance
    println!(
        "Grammar in: {}.{:#003} ms{}",
        elapsed.as_millis(),
        elapsed.subsec_micros(),
        if cfg!(debug_assertions) {
            " [WARNING: debug mode heavily impacts performances]"
        } else {
            ""
        }
    );

    // Measure performances
    let now = Instant::now();

    // Parse the input
    let parsed = execute(&RuntimeContext {
        grammar: &pst,
        external_rules: None,
        subject: TEST_SUBJECT,
        options: RuntimeOptions::new(),
    })
    .unwrap_or_else(|err| {
        eprintln!("{}", err);
        panic!("Failed to match RPN grammar against a RPN expression");
    });

    // Get elapsed time
    let elapsed = now.elapsed();

    println!("Expression: {}", TEST_SUBJECT);

    // Display performance
    println!(
        "Parse time: {}.{:#003} ms{}",
        elapsed.as_millis(),
        elapsed.subsec_micros(),
        if cfg!(debug_assertions) {
            " [WARNING: debug mode heavily impacts performances]"
        } else {
            ""
        }
    );

    // Evaluate the expression
    let result = eval_expr(&parsed);
    println!("Result    : {}", result);

    // Ensure the result is correct
    assert!((result - 0.8133333333333332).abs() < f64::EPSILON);
}

fn eval_expr(matching: &MatchedRule) -> f64 {
    let data = matching.data();

    match matching.name() {
        "main" => match data {
            MatchedData::Rule(expr) => eval_expr(expr.as_ref()),
            _ => unreachable!("'main' rule doesn't contain an 'expr' rule"),
        },

        "expr" => match data {
            MatchedData::Rule(inner) => eval_expr(inner),
            _ => unreachable!("'expr' rule is not made as a rule"),
        },

        "paren_expr" => match data {
            MatchedData::SuiteOf(suite) => match suite.as_slice() {
                [_, MatchedData::Rule(expr), _] => eval_expr(expr),
                _ => unreachable!("'paren_expr' rule's suite isn't made as epxected"),
            },
            _ => unreachable!("'paren_expr' rule isn't made of a suite of patterns"),
        },

        "operation" => match data {
            MatchedData::SuiteOf(suite) => match suite.as_slice() {
                [MatchedData::Rule(expr1), _, MatchedData::Rule(expr2), _, MatchedData::Rule(operator)] =>
                {
                    let expr1 = eval_expr(expr1);
                    let expr2 = eval_expr(expr2);

                    match operator.name() {
                        "operator" => match operator.data() {
                            MatchedData::CstString(operator) => match *operator {
                                "+" => expr1 + expr2,
                                "-" => expr1 - expr2,
                                "*" => expr1 * expr2,
                                "/" => expr1 / expr2,
                                _ => unreachable!("unknown operator: {}", operator),
                            },
                            _ => unreachable!("'operator' rule isn't made of a constant string"),
                        },
                        _ => unreachable!(
                            "'operation' rule's suite doesn't start with an 'operator' rule"
                        ),
                    }
                }
                _ => unreachable!("'operation' rule's suite isn't made as expected"),
            },
            _ => unreachable!("'operation' rule doesn't contain the expected suite"),
        },

        "operand" => match data {
            MatchedData::Rule(number_of_paren_expr) => eval_expr(number_of_paren_expr),
            _ => unreachable!("'operand' rule isn't made of a rule"),
        },

        "number" => match data {
            MatchedData::Rule(int_or_float) => eval_expr(int_or_float),
            _ => unreachable!("'number' rule isn't made of a rule"),
        },

        "int" => match data {
            MatchedData::AtomicPattern(digits) => digits.parse::<f64>().unwrap(),
            _ => unreachable!("'int' rule isn't made of an atomic pattern"),
        },

        "float" => match data {
            MatchedData::SuiteOf(patterns) => match patterns.as_slice() {
                [MatchedData::Rule(int1), _, MatchedData::Rule(int2)] => {
                    match (int1.name(), int2.name()) {
                        ("int", "int") => {
                            let int2 = eval_expr(int2);
                            eval_expr(int1) + (int2 / 10f64.powi(int2.to_string().len() as i32))
                        }
                        _ => unreachable!(
                        "'float' rule's suite isn't made of two integers and a decimal separator"
                    ),
                    }
                }
                _ => unreachable!("'float' rule's suite isn't made as expected"),
            },
            _ => unreachable!("'float' rule isn't made of a suite"),
        },

        name => unreachable!("unknown rule: {}", name),
    }
}
