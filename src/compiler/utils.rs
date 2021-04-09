use super::parser::ParserLoc;

/// Left-trim an input and get the number of removed characters
pub fn trim_start_and_count(input: &str) -> (&str, usize) {
    let trimmed = count_start_whitespaces(input);
    (&input[trimmed..], trimmed)
}

/// Count start whitespaces
pub fn count_start_whitespaces(input: &str) -> usize {
    input.chars().take_while(|c| c.is_whitespace()).count()
}

/// Check if a line is finished, which requires one the following conditions:
/// * The line is empty
/// * The line is only made of spaces
/// * The line is only made of spaces and a comment (`# ...`)
pub fn is_finished_line(line: &str) -> bool {
    let trimmed = line.trim();
    trimmed.is_empty() || trimmed.starts_with('#')
}

/// Add a base location to a parser location
pub fn add_parser_loc(base_line: usize, base_col: usize, target_loc: ParserLoc) -> ParserLoc {
    ParserLoc::new(
        base_line + target_loc.line(),
        if target_loc.line() == 0 {
            base_col + target_loc.col()
        } else {
            // Don't add the base column after the first line
            target_loc.col()
        },
    )
}

// Substract a base location from a parser location
pub fn sub_parser_loc(base_line: usize, base_col: usize, target_loc: ParserLoc) -> ParserLoc {
    ParserLoc::new(
        target_loc.line() - base_line,
        if target_loc.line() == 0 {
            target_loc.col() - base_col
        } else {
            // Don't substract the base column after the first line
            target_loc.col()
        },
    )
}

/// Check if a pattern's name refers to a builtin one
pub fn is_builtin_pattern_name(name: &str) -> bool {
    name.starts_with("B_")
}

/// Check if a pattern's name refers to an existing builtin pattern
pub fn is_valid_builtin_pattern(name: &str) -> bool {
    BUILTIN_PATTERNS.contains(&name)
}

/// Check if a pattern's name refers to an external one
pub fn is_external_pattern_name(name: &str) -> bool {
    name.starts_with("E_")
}

/// Check if a pattern name is reserved
pub fn is_reserved_pattern_name(name: &str) -> bool {
    is_builtin_pattern_name(name) || is_external_pattern_name(name)
}

/// Exhaustive list of all builtin patterns
pub static BUILTIN_PATTERNS: &[&str] = &[
    "B_EOI",
    "B_TRUE",
    "B_FALSE",
    "B_ANY",
    "B_NEWLINE_CR",
    "B_NEWLINE_LF",
    "B_DOUBLE_QUOTE",
    "B_ASCII",
    "B_ASCII_ALPHABETIC",
    "B_ASCII_ALPHANUMERIC",
    "B_ASCII_CONTROL",
    "B_ASCII_DIGIT",
    "B_ASCII_GRAPHIC",
    "B_ASCII_HEXDIGIT",
    "B_ASCII_LOWERCASE",
    "B_ASCII_PUNCTUATION",
    "B_ASCII_UPPERCASE",
    "B_ASCII_WHITESPACE",
    "B_CONTROL",
    "B_LOWERCASE",
    "B_NUMERIC",
    "B_UPPERCASE",
    "B_WHITESPACE",
    "B_BIN_DIGIT",
    "B_OCTAL_DIGIT",
    "B_DEC_DIGIT",
    "B_HEX_DIGIT",
];
