use std::time::Instant;

#[macro_use]
extern crate peggy_derive;

#[peggy_grammar(filename = "../examples/debugger.peggy", debugger = "debugger")]
pub mod debugger_grammar {}

pub mod debugger {
    use lazy_static::lazy_static;
    use std::sync::Mutex;

    lazy_static! {
        // Rules we are currently visiting
        static ref VISITING: Mutex<Vec<&'static str>> = Mutex::new(vec![]);
    }

    // Called when entering a rule
    pub fn entering_rule(rule_name: &'static str, input: &str, offset: usize) {
        let mut rules = VISITING.lock().unwrap();
        rules.push(rule_name);

        let padding = format!(
            "{}{}",
            "--".repeat(rules.len() * 2),
            if rules.is_empty() { "" } else { "-> " }
        );

        println!(
            "{}[ {} ] (depth: {})\n{}At offset {}, next 20 chars: >{}<\n{}",
            padding,
            rules.join(" => "),
            rules.len(),
            padding,
            offset,
            input
                .chars()
                .take(20)
                .map(|c| if c == '\r' || c == '\n' { '.' } else { c })
                .collect::<String>(),
            padding
        );
    }

    // Called when a rule has completed its task (succeeding or failing)
    pub fn leaving_rule(
        _rule_name: &'static str,
        _input: &str,
        _offset: usize,
        _err: Option<super::debugger_grammar::PegError>,
    ) {
        VISITING.lock().unwrap().pop();
    }
}

// Define a test input
static TEST_INPUT: &str = "Peggy! Peggy! Peggy!";

fn main() {
    // Measure performance
    let now = Instant::now();

    // Evaluate the expression
    let success = debugger_grammar::exec(TEST_INPUT).unwrap_or_else(|err| {
        panic!(
            "Failed to match PRN grammar against a PRN expression: {:#?}",
            err
        )
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

    dbg!(success);
}
