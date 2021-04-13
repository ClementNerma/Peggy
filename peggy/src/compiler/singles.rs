use super::data::{ParserLoc, Pattern, PatternRepetition};
use super::errors::{ParserError, ParserErrorContent};
use super::parser::parse_rule_pattern;
use std::rc::Rc;

/// Try to match a constant string pattern
pub fn cst_string(input: &str, base_loc: ParserLoc) -> Result<Option<(&str, usize)>, ParserError> {
    let mut chars = input.chars();

    match chars.next() {
        Some('"') => {}
        Some(_) | None => return Ok(None),
    }

    let mut col = 1;

    loop {
        let c = chars.next().ok_or_else(|| {
            ParserError::new(
                base_loc.with_add_cols(col),
                0,
                ParserErrorContent::UnterminatedCstString {
                    started_at: base_loc,
                },
                Some("you may need to add a closing quote '\"'"),
            )
        })?;

        if c == '"' {
            break;
        }

        col += c.len_utf8();
    }

    let cst_str = &input[1..col];

    if cst_str.is_empty() {
        Err(ParserError::new(
            base_loc,
            2,
            ParserErrorContent::EmptyConstantString,
            None,
        ))
    } else {
        Ok(Some((cst_str, cst_str.len() + 2)))
    }
}

/// Try to match a rule's name
pub fn rule_name(input: &str, base_loc: ParserLoc) -> Result<Option<(&str, usize)>, ParserError> {
    let mut chars = input.chars();

    match chars.next() {
        Some(c) if c.is_alphabetic() => {}
        Some(_) | None => return Ok(None),
    }

    let mut name_len = 1;

    for c in chars {
        if c.is_whitespace() || PatternRepetition::parse(c).is_some() || c == '|' || c == '#' {
            break;
        }

        if !c.is_alphanumeric() && c != '_' {
            return Err(ParserError::new(
                base_loc.with_add_cols(name_len - 1),
                1,
                ParserErrorContent::IllegalSymbol(c),
                Some("check if you have spelled the rule's name correctly"),
            ));
        }

        name_len += c.len_utf8();
    }

    Ok(Some((&input[0..=name_len - 1], name_len)))
}

/// Try to match a group
pub fn group(
    input: &str,
    base_loc: ParserLoc,
) -> Result<Option<(Rc<Pattern>, usize)>, ParserError> {
    let mut chars = input.chars();

    let mut opened_string = false;
    let mut inner_groups: u32 = 0;
    let mut group_length = 1;

    match chars.next() {
        Some('(') => {}
        Some(_) | None => return Ok(None),
    }

    loop {
        let next_c = chars.next().ok_or_else(|| {
            ParserError::new(
                base_loc.with_add_cols(group_length),
                0,
                ParserErrorContent::UnclosedGroup {
                    started_at: base_loc,
                },
                Some("you may need to add a closing parenthesis ')'"),
            )
        })?;

        group_length += next_c.len_utf8();

        if opened_string {
            if next_c == '"' {
                opened_string = false;
            }

            continue;
        }

        if next_c == '(' {
            inner_groups += 1;
        } else if next_c == ')' {
            if inner_groups == 0 {
                break;
            }

            inner_groups -= 1;
        }
    }

    Ok(Some((
        Rc::new(parse_rule_pattern(
            &input[1..group_length - 1],
            base_loc.with_add_cols(1),
        )?),
        group_length,
    )))
}
