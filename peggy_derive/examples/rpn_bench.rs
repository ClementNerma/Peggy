use std::time::Instant;

#[macro_use]
extern crate peggy_derive;

#[peggy_grammar(filename = "../examples/rpn.peggy")]
pub mod rpn_grammar {}

use rpn_grammar::matched;
use rpn_grammar::strings;
use rpn_grammar::unions::*;

static TEST_INPUT: &str = "(3 (9.3 3 /) +) (5 (2 3 /) /) /";
static ITERATIONS: usize = 100_000;

fn main() {
    println!("Expression: {}", TEST_INPUT);
    println!("Iterations: {}", ITERATIONS);

    // Measure performance
    let mut iteration = 0;
    let now = Instant::now();

    // Evaluate the expression
    let success = loop {
        iteration += 1;

        let result = rpn_grammar::exec(TEST_INPUT).unwrap_or_else(|err| {
            panic!("Failed to match input against RPN grammar:\n{}", err);
        });

        if iteration == ITERATIONS {
            break result;
        }
    };

    // Get elapsed time
    let elapsed = now.elapsed();

    // Get average execution time
    let average_time_ms = elapsed.as_micros() as f64 / ITERATIONS as f64;

    // Display performance
    println!(
        "Parsing   : {:.1} microseconds (average) {}",
        average_time_ms,
        if cfg!(debug_assertions) {
            " [WARNING: debug mode heavily impacts performances]"
        } else {
            ""
        }
    );

    // Display the result
    let result = eval_expr(&success.matched);
    println!("Result    : {}", result);

    // Ensure the result is correct
    assert!((result - 0.8133333333333332).abs() < f64::EPSILON);
}

fn eval_expr(expr: &matched::expr) -> f64 {
    expr.matched
        .variants_ref(eval_num, eval_operation, eval_paren_expr)
}

fn eval_num(num: &matched::number) -> f64 {
    num.matched.variants_ref(eval_int, eval_float)
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
    op.matched.variants_ref(eval_num, eval_paren_expr)
}

fn eval_paren_expr(paren_expr: &matched::paren_expr) -> f64 {
    eval_expr(paren_expr.matched.as_ref())
}
