use std::time::Instant;

#[macro_use]
extern crate peggy_derive;

#[peggy_grammar(filename = "../examples/prn.peggy")]
pub mod prn_grammar {}

use prn_grammar::matched;
use prn_grammar::strings;
use prn_grammar::unions::*;

static TEST_INPUT: &str = "(3 (9.3 3 /) +) (5 (2 3 /) /) /";

fn main() {
    println!("Expression: {}", TEST_INPUT);

    // Measure performance
    let now = Instant::now();

    // Evaluate the expression
    let success = prn_grammar::exec(TEST_INPUT).unwrap_or_else(|err| {
        panic!(
            "Failed to match PRN grammar against a PRN expression: {:#?}",
            err
        );
    });

    // Get elapsed time
    let elapsed = now.elapsed();

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

    // Display the result
    let result = eval_expr(&success.matched);
    println!("Result: {}", result);

    // Ensure the result is correct
    assert!((result - 0.8133333333333332).abs() < f64::EPSILON);
}

fn eval_expr(expr: &matched::expr) -> f64 {
    match &expr.matched {
        Sw3::A(num) => eval_num(num),
        Sw3::B(op) => eval_operation(op),
        Sw3::C(paren_expr) => eval_paren_expr(paren_expr),
    }
}

fn eval_num(num: &matched::number) -> f64 {
    match &num.matched {
        Sw2::A(int) => eval_int(int),
        Sw2::B(float) => eval_float(float),
    }
}

fn eval_int(int: &matched::int) -> f64 {
    int.matched.parse::<f64>().unwrap()
}

fn eval_float(float: &matched::float) -> f64 {
    let int = eval_int(&float.matched.0);
    let dec = eval_int(&float.matched.1);
    int + (dec / 10f64.powi(dec.to_string().len() as i32))
}

fn eval_operation(op: &matched::operation) -> f64 {
    let op1 = eval_operand(&op.matched.0);
    let op2 = eval_operand(&op.matched.1);

    match &op.matched.2.matched {
        Sw4::A(strings::Str___Plus__) => op1 + op2,
        Sw4::B(strings::Str___Less__) => op1 - op2,
        Sw4::C(strings::Str___Multiply__) => op1 * op2,
        Sw4::D(strings::Str___Divide__) => op1 / op2,
    }
}

fn eval_operand(op: &matched::operand) -> f64 {
    match &op.matched {
        Sw2::A(num) => eval_num(num),
        Sw2::B(paren_expr) => eval_paren_expr(paren_expr),
    }
}

fn eval_paren_expr(paren_expr: &matched::paren_expr) -> f64 {
    eval_expr(paren_expr.matched.as_ref())
}
