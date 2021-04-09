//! This file demonstrates how to make a simple Reverse Polish Notation (RPN) parser and evaluator using Peggy

use peggy::compiler::{parse_peg, pretty_format_parser_err};
use peggy::runtime::{execute, MatchedData, MatchingPattern, RuntimeContext, RuntimeOptions};
use std::time::Instant;

static PRN_GRAMMAR: &str = r#"

S = B_WHITESPACE                            # Whitespace
DEC_SEP = "." | ","                         # Decimal separator

int = B_ASCII_DIGIT+                        # Integer
float = int DEC_SEP int                     # Floating-point number
number = int | float                        # Number

operator = "+" | "-" | "*" | "/"            # Operator
operand = number | paren_expr               # Operand
operation = operand S+ operand S* operator  # Complete operation

paren_expr = ("(" S* expr S* ")")           # Expression wrapped between parenthesis
expr = number | operation | paren_expr      # Complete expression

main = expr                                 # Grammar's entrypoint

"#;

static TEST_SUBJECT: &str = r"(3 (9.3 3 /) +) (5 (2 3 /) /) /";

fn main() {
    // Measure performances
    let now = Instant::now();

    // Parse the grammar
    let pst = parse_peg(PRN_GRAMMAR).unwrap_or_else(|err| {
        eprintln!("{}", pretty_format_parser_err(PRN_GRAMMAR, err));
        panic!("Failed to parse PRN grammar");
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
        external_patterns: None,
        subject: TEST_SUBJECT,
        options: RuntimeOptions::new(),
    })
    .unwrap_or_else(|err| {
        eprintln!("{}", err);
        panic!("Failed to match PRN grammar against a PRN expression");
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

fn eval_expr(matching: &MatchingPattern) -> f64 {
    let data = matching.data();

    match matching.name() {
        "main" => match data {
            MatchedData::Pattern(expr) => eval_expr(expr.as_ref()),
            _ => unreachable!("'main' pattern doesn't contain an 'expr' pattern"),
        },

        "expr" => match data {
            MatchedData::Pattern(inner) => eval_expr(inner),
            _ => unreachable!("'expr' pattern is not made as a pattern"),
        },

        "paren_expr" => match data {
            MatchedData::SuiteOf(suite) => match suite.as_slice() {
                [_, _, MatchedData::Pattern(expr), _, _] => eval_expr(expr),
                _ => unreachable!("'paren_expr' pattern's suite isn't made as epxected")
            },
            _ => unreachable!("'paren_expr' pattern isn't made of a suite of pieces")
        }

        "operation" => match data {
                MatchedData::SuiteOf(suite) => match suite.as_slice() {
                    [MatchedData::Pattern(expr1), _, MatchedData::Pattern(expr2), _, MatchedData::Pattern(operator)] =>
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
                                _ => unreachable!("'operator' pattern isn't made of a constant string"),
                            },
                            _ => unreachable!("'operation' pattern's suite doesn't start with an 'operator' pattern"),
                        }
                    }
                    _ => unreachable!("'operation' pattern's suite isn't made as expected"),
                },
                _ => unreachable!("'operation' pattern doesn't contain the expected suite"),
            },

        "operand" => match data {
            MatchedData::Pattern(number_of_paren_expr) => eval_expr(number_of_paren_expr),
            _ => unreachable!("'operand' pattern isn't made of a pattern")
        },

        "number" => match data {
            MatchedData::Pattern(int_or_float) => eval_expr(int_or_float),
            _ => unreachable!("'number' pattern isn't made of a pattern"),
        },

        "int" => match data {
            MatchedData::RepeatedPiece(digits) => {
                let mut num: f64 = 0.0;

                for digit in digits {
                    match digit {
                        MatchedData::BuiltinPattern {
                            name: "B_ASCII_DIGIT",
                            symbol,
                        } => {
                            num *= 10.0;
                            num += symbol.unwrap().to_digit(10).unwrap() as f64;
                        }
                        _ => unreachable!(
                            "'int' pattern's repeated pieces aren't 'B_ASCII_DIGIT' patterns"
                        ),
                    }
                }

                num
            }
            _ => unreachable!("'int' pattern isn't made of repeated pieces"),
        },

        "float" => match data {
            MatchedData::SuiteOf(pieces) => match pieces.as_slice() {
                [MatchedData::Pattern(int1), _, MatchedData::Pattern(int2)] => match (int1.name(), int2.name()) {
                    ("int", "int") => {
                        let int2 = eval_expr(int2);
                        eval_expr(int1) + (int2 / 10f64.powi(int2.to_string().len() as i32))
                    },
                    _ => unreachable!("'float' pattern's suite isn't made of two integers and a decimal separator")
                },
                _ => unreachable!("'float' pattern's suite isn't made as expected")
            },
            _ => unreachable!("'float' pattern isn't made of a suite")
        },

        name => unreachable!("unknown pattern: {}", name),
    }
}
