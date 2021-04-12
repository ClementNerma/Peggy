/// Match using a one-character builtin pattern
///
/// If the pattern does not exist, the `None` value will be returned.
/// If it exists but does not match, this function will return `false`.
/// Otherwise, it will return `true`, indicating a match. A single character should be consumed from the input in that case.
pub fn match_builtin_pattern(pattern_name: &str, next_char: Option<char>) -> Option<bool> {
    Some(match pattern_name {
        "B_ANY" => next_char.is_some(),

        "B_NEWLINE_CR" => next_char.map(|c| c == '\r').unwrap_or(false),
        "B_NEWLINE_LF" => next_char.map(|c| c == '\n').unwrap_or(false),

        "B_DOUBLE_QUOTE" => next_char.map(|c| c == '"').unwrap_or(false),

        "B_ASCII" => next_char.map(|c| c.is_ascii()).unwrap_or(false),
        "B_ASCII_ALPHABETIC" => next_char.map(|c| c.is_ascii_alphabetic()).unwrap_or(false),
        "B_ASCII_ALPHANUMERIC" => next_char
            .map(|c| c.is_ascii_alphanumeric())
            .unwrap_or(false),
        "B_ASCII_CONTROL" => next_char.map(|c| c.is_ascii_control()).unwrap_or(false),
        "B_ASCII_DIGIT" => next_char.map(|c| c.is_ascii_digit()).unwrap_or(false),
        "B_ASCII_GRAPHIC" => next_char.map(|c| c.is_ascii_graphic()).unwrap_or(false),
        "B_ASCII_HEXDIGIT" => next_char.map(|c| c.is_ascii_hexdigit()).unwrap_or(false),
        "B_ASCII_LOWERCASE" => next_char.map(|c| c.is_ascii_lowercase()).unwrap_or(false),
        "B_ASCII_PUNCTUATION" => next_char.map(|c| c.is_ascii_punctuation()).unwrap_or(false),
        "B_ASCII_UPPERCASE" => next_char.map(|c| c.is_ascii_uppercase()).unwrap_or(false),
        "B_ASCII_WHITESPACE" => next_char.map(|c| c.is_ascii_whitespace()).unwrap_or(false),

        "B_CONTROL" => next_char.map(|c| c.is_control()).unwrap_or(false),
        "B_LOWERCASE" => next_char.map(|c| c.is_lowercase()).unwrap_or(false),
        "B_NUMERIC" => next_char.map(|c| c.is_numeric()).unwrap_or(false),
        "B_UPPERCASE" => next_char.map(|c| c.is_uppercase()).unwrap_or(false),
        "B_WHITESPACE" => next_char.map(|c| c.is_whitespace()).unwrap_or(false),

        _ => return None,
        // NOTE: When adding a new item to this list, the `BUILTIN_PATTERNS` static also needs to be updated
        // NOTE: Also needs to be updated the Rust generator for these patterns
    })
}
