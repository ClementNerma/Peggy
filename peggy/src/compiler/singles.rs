use super::errors::{ParserError, ParserErrorContent};
use super::parser::{parse_pattern_content, ParserLoc, PatternPiece, PatternRepetition};
use std::rc::Rc;

/// Try to match a constant string piece
pub fn cst_string(input: &str) -> Result<Option<(&str, usize)>, ParserError> {
    let mut chars = input.chars();

    match chars.next() {
        Some('"') => {}
        Some(_) | None => return Ok(None),
    }

    let mut col = 0;

    loop {
        col += 1;

        let c = chars.next().ok_or_else(|| {
            ParserError::new(
                ParserLoc::new(0, col),
                0,
                ParserErrorContent::UnterminatedCstString {
                    started_at: ParserLoc::new(0, 0),
                },
                Some("you may need to add a closing quote '\"'"),
            )
        })?;

        if c == '"' {
            break;
        }
    }

    Ok(Some((&input[1..col], col + 1)))
}

/// Try to match a pattern's name
pub fn pattern_name(input: &str) -> Result<Option<(&str, usize)>, ParserError> {
    let mut chars = input.chars();

    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        Some(_) | None => return Ok(None),
    }

    let mut name_len = 1;

    for c in chars {
        if c.is_whitespace() || PatternRepetition::parse(c).is_some() || c == '|' || c == '#' {
            break;
        }

        if !c.is_alphanumeric() && c != '_' {
            return Err(ParserError::new(
                ParserLoc::new(0, name_len),
                1,
                ParserErrorContent::IllegalSymbol(c),
                Some("check if you have spelled the pattern's name correctly"),
            ));
        }

        name_len += 1;
    }

    Ok(Some((&input[0..=name_len - 1], name_len)))
}

/// Try to match a group
pub fn group(input: &str) -> Result<Option<(Rc<PatternPiece>, usize)>, ParserError> {
    let mut chars = input.chars();

    let mut opened_string = false;
    let mut stacked_groups: u32 = 0;
    let mut group_length = 1;

    match chars.next() {
        Some('(') => {}
        Some(_) | None => return Ok(None),
    }

    loop {
        group_length += 1;

        let next_c = chars.next().ok_or_else(|| {
            ParserError::new(
                ParserLoc::new(0, group_length),
                0,
                ParserErrorContent::UnclosedGroup {
                    started_at: ParserLoc::new(0, 0),
                },
                Some("you may need to add a closing parenthesis ')'"),
            )
        })?;

        if opened_string {
            if next_c == '"' {
                opened_string = false;
            }

            continue;
        }

        if next_c == '(' {
            stacked_groups += 1;
        } else if next_c == ')' {
            if stacked_groups == 0 {
                break;
            }

            stacked_groups -= 1;
        }
    }

    Ok(Some((
        Rc::new(parse_pattern_content(&input[1..group_length - 1], ParserLoc::new(0, 1))?.0),
        group_length,
    )))
}
