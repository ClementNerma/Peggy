use super::errors::ParserError;

/// Format in a human-readable way a compilation error
pub fn pretty_format_parser_err(input: &str, err: ParserError) -> String {
    let line = if err.line() < input.lines().count() {
        input.lines().nth(err.line()).unwrap()
    } else {
        ""
    };
    let padding =
        " ".repeat(line[..err.col()].chars().count() + (err.line() + 1).to_string().len() + 3);
    let tip = err.tip().map(|tip| format!("\n{}Tip: {}", padding, tip));

    format!(
        "ERROR: At line {}, column {}:\n\n{} | {}\n{}{}{}{}",
        err.line() + 1,
        err.col() + 1,
        err.line() + 1,
        line,
        padding,
        "^".repeat(if err.length() == 0 { 1 } else { err.length() }),
        format!("{}", err.content())
            .lines()
            .map(|line| format!("\n{}{}", padding, line))
            .collect::<String>(),
        tip.unwrap_or_else(String::new)
    )
}
