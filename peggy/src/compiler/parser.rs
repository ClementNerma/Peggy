use super::errors::{add_base_err_loc, ParserError, ParserErrorContent};
use super::singles;
use super::utils::*;
use super::validator::validate_parsed_peg;
use std::collections::HashMap;
use std::rc::Rc;

/// A grammar's entrypoint pattern
pub const GRAMMAR_ENTRYPOINT_PATTERN: &str = "main";

/// Compile a Peggy grammar to a [syntax tree](`PegSyntaxTree`)
pub fn parse_peg(grammar: &str) -> Result<PegSyntaxTree, ParserError> {
    let parsed = parse_peg_nocheck(grammar)?;

    // Ensure the syntax tree is valid
    validate_parsed_peg(&parsed)?;

    Ok(parsed)
}

/// Compile a Peggy grammar but don't check for validity (e.g. inexistant pattern names, etc.)
///
/// A bit faster than [`parse_peg`] but less safe due to the lack of check.
///
/// Near-instant checks are still performed, like ensuring the presence of a `main` pattern.
pub fn parse_peg_nocheck(input: &str) -> Result<PegSyntaxTree, ParserError> {
    // Collected patterns
    let mut patterns = HashMap::new();

    // Is a multi-line comment opened?
    let mut multi_line_comment_opened = None;

    // Iterate over each line, as there should be one pattern per non-empty line
    for (l, line) in input.lines().enumerate() {
        // Left trim
        let (line, trimmed) = trim_start_and_count(line);

        if line.trim_end() == "###" {
            multi_line_comment_opened = if multi_line_comment_opened.is_none() {
                Some((l, trimmed))
            } else {
                None
            };
            continue;
        }

        if multi_line_comment_opened.is_some() {
            continue;
        }

        // Ignore empty lines
        if is_finished_line(line) {
            continue;
        }

        let mut chars = line.chars();

        // Get the first character of the line...
        let c = chars.next().unwrap();

        // ...which must be a pattern's name (syntax: `pattern = <content>`)
        if !c.is_alphabetic() && c != '_' {
            return Err(ParserError::new(
                ParserLoc::new(l, trimmed),
                1,
                ParserErrorContent::ExpectedPatternDeclaration,
                Some(match c {
                    '0'..='9' => "digits are not allowed to begin a pattern's name",
                    _ => "only alphabetic and underscores characters are allowed to begin a pattern's name"
                })
            ));
        }

        // Length of the pattern's name (1 as we already checked the first character)
        let mut pattern_name_length = 1;

        // Indicate if the name is finished, but the loop is still active.
        // Used to count spaces separating the name and the assignment operator (=)
        let mut pattern_name_ended = false;

        // Number of spaces separating the name and the assignment operator (=)
        let mut pattern_op_spaces_sep = 0;

        // Iterate over characters
        loop {
            match chars.next() {
                // Identifier-compliant characters
                Some(c) if !pattern_name_ended && (c.is_alphanumeric() || c == '_') => {
                    pattern_name_length += 1;
                }

                // Assignment operator (indicates the beginning of the pattern's content)
                Some('=') => break,

                // Whitespaces (optional, can be used to separate the name and the assignment operator)
                Some(c) if c.is_whitespace() => {
                    pattern_name_ended = true;
                    pattern_op_spaces_sep += 1;
                }

                // Other characters (= non-compliant)
                Some(c) => {
                    return Err(ParserError::new(
                        ParserLoc::new(l, trimmed + pattern_name_length),
                        1,
                        ParserErrorContent::IllegalSymbol(c),
                        Some("only alphanumeric and underscore characters are allowed in a pattern's name")
                    ));
                }

                // End of line without an assignment operator
                None => {
                    return Err(ParserError::new(
                        ParserLoc::new(l, trimmed + pattern_name_length),
                        1,
                        ParserErrorContent::ExpectedPatternAssignmentOp,
                        Some("you may have forgot to add the pattern assignment operator '='"),
                    ));
                }
            }
        }

        // Collect the pattern's name
        let pattern_name = &line[..pattern_name_length];

        // Detect reserved pattern names
        if is_reserved_pattern_name(pattern_name) {
            return Err(ParserError::new(
                ParserLoc::new(l, trimmed),
                pattern_name_length,
                ParserErrorContent::ReservedUppercasePatternName,
                Some("try to use a name that doesn't start by 'B_' (builtin patterns) or 'E_' (external patterns)"),
            ));
        }

        // Detect duplicate patterns
        if patterns.contains_key(pattern_name) {
            return Err(ParserError::new(
                ParserLoc::new(l, trimmed),
                pattern_name_length,
                ParserErrorContent::DuplicatePatternName,
                None,
            ));
        }

        // Get the column the pattern's content starts at
        let mut start_column = pattern_name_length + pattern_op_spaces_sep + 1;

        // Left-trim the content
        let (line, trimmed_2) = trim_start_and_count(&line[start_column..]);
        start_column += trimmed + trimmed_2;

        // Parse the pattern's content
        let pattern = parse_pattern_content(
            line,
            ParserLoc {
                line: l,
                col: start_column,
            },
        )?;

        // Save the new pattern
        patterns.insert(pattern_name, pattern);
    }

    // Ensure all multi-line comments have been closed
    if let Some((line, col)) = multi_line_comment_opened {
        return Err(ParserError::new(
            ParserLoc::new(input.lines().count(), 0),
            0,
            ParserErrorContent::UnterminatedMultiLineComment {
                started_at: ParserLoc::new(line, col),
            },
            Some("you can add '###' on a single line to close the comment"),
        ));
    }

    // Ensure a main pattern is declared
    if !patterns.contains_key(GRAMMAR_ENTRYPOINT_PATTERN) {
        return Err(ParserError::new(
            ParserLoc::new(input.lines().count(), 0),
            0,
            ParserErrorContent::MissingMainPattern,
            Some("you must declare a pattern named 'main' which will be the entrypoint of your syntax")
        ));
    }

    // Success!
    Ok(PegSyntaxTree { patterns })
}

/// Parse a pattern's content (e.g. `<content>` in `pattern = <content>`)
pub fn parse_pattern_content(
    input: &str,
    base_loc: ParserLoc,
) -> Result<PatternContent, ParserError> {
    // This function is not supposed to be called with an empty content, so we can directly parse the first piece of the pattern
    let (first_piece, piece_len, stopped_at) = parse_pattern_piece(input)
        .map_err(|err| add_base_err_loc(base_loc.line, base_loc.col, err))?;

    // Remove the first piece's content from the remaining input
    let input = &input[piece_len..];
    let mut column = piece_len;

    // Make a global piece
    // This will contain everything the pattern's content is made of
    // The data is built depending on the reason why the piece parser stopped previously
    let mut global_piece = match stopped_at {
        // If the parser stopped because it got to the end of the line, return the piece as it is
        PieceParserStoppedAt::End => first_piece,

        // If the parser stopped because of a continuation separator (whitespace) or an union separator (|),
        // all items of the follow/union should be collected at once
        PieceParserStoppedAt::ContinuationSep | PieceParserStoppedAt::UnionSep => {
            // Create an union child (see below)
            fn create_union_child(mut pieces: Vec<PatternPiece>) -> PatternPiece {
                assert_ne!(pieces.len(), 0);

                if pieces.len() == 1 {
                    // Avoid making a `PatternPieceValue::Suite` wrapper for a single piece
                    pieces.into_iter().next().unwrap()
                } else {
                    let ParserLoc { line, col } = pieces[0].relative_loc;

                    for piece in pieces.iter_mut() {
                        piece.relative_loc = sub_parser_loc(line, col, piece.relative_loc);
                    }

                    PatternPiece {
                        relative_loc: pieces[0].relative_loc,
                        repetition: None,
                        is_silent: false,
                        value: PatternPieceValue::Suite(pieces),
                    }
                }
            }

            // Get the first pieces
            // The `pieces` variable contains the parsed pieces
            // The `unions` variable contains each member of the pending union. If the whole pattern's content is not an union, this will remain empty.
            // When an union separator is detected, the content of `pieces` is moved to `unions` in order to separate each member of the union.
            let (mut pieces, mut unions) = if stopped_at == PieceParserStoppedAt::UnionSep {
                (vec![], vec![create_union_child(vec![first_piece])])
            } else {
                (vec![first_piece], vec![])
            };

            // Local modifyable input
            let mut input = input;

            loop {
                // Parse the next piece
                let (next_piece, next_piece_len, next_stopped_at) = parse_pattern_piece(input)
                    .map_err(|err| add_base_err_loc(base_loc.line, base_loc.col + column, err))?;

                // Push it to the list of pending pieces
                pieces.push(PatternPiece {
                    relative_loc: add_parser_loc(0, column, next_piece.relative_loc),
                    ..next_piece
                });

                // Remove it from the remaining input
                input = &input[next_piece_len..];

                // Trim the remaining input
                let trimmed = count_start_whitespaces(input);
                input = &input[trimmed..];

                // Update the column number
                column += next_piece_len + trimmed;

                // Check the reason why the parser stopped here
                match next_stopped_at {
                    // If it stopped because it was the end of the input, it's time to return the whole collected data
                    PieceParserStoppedAt::End => {
                        break PatternPiece {
                            relative_loc: ParserLoc { line: 0, col: 0 },
                            repetition: None,
                            is_silent: false,
                            // If the parser stopped on the first piece because it encountered an union separator, the remaining content
                            // should be put inside an union.
                            // Otherwise, and if no union separator was found during the parsing on the whole pattern's content,
                            // a simple suite can be made from the pieces.
                            value: if stopped_at != PieceParserStoppedAt::UnionSep
                                && unions.is_empty()
                            {
                                // Avoid making a whole suite wrapper for a single piece
                                if pieces.len() == 1 {
                                    break pieces.into_iter().next().unwrap();
                                } else {
                                    PatternPieceValue::Suite(pieces)
                                }
                            } else {
                                // Otherwise, terminate the union
                                if !pieces.is_empty() {
                                    unions.push(create_union_child(pieces));
                                }

                                assert!(unions.len() >= 2);

                                PatternPieceValue::Union(unions)
                            },
                        };
                    }

                    // If a continuation separator (whitespace) was encountered, just go to the next piece (= do nothing for now)
                    PieceParserStoppedAt::ContinuationSep => {}

                    // If an union separator (|) was encountered...
                    PieceParserStoppedAt::UnionSep => {
                        // The whole piece is now considered as an union, given that unions have precedence over everything else

                        // Put the current pieces in the union's members list
                        unions.push(create_union_child(pieces));

                        // Prepare for the next union's member (if any)
                        pieces = vec![];
                    }
                }
            }
        }
    };

    // Update the base location
    if global_piece.relative_loc.line == 0 {
        global_piece.relative_loc.col += base_loc.col;
    }
    global_piece.relative_loc.line += base_loc.line;

    // Success!
    Ok(PatternContent(global_piece))
}

/// Parse a pattern's piece
/// The success return value is made of the parsed piece, the consumed input length, and the reason why the parser stopped at this specific symbol
pub fn parse_pattern_piece(
    input: &str,
) -> Result<(PatternPiece, usize, PieceParserStoppedAt), ParserError> {
    // Left-trim the input
    let (input, trimmed) = trim_start_and_count(input);

    // Parse the first sub-piece (note that the entire piece may be made of a single one)
    let (first_piece, first_piece_len) =
        parse_pattern_sub_piece(input).map_err(|err| add_base_err_loc(0, trimmed, err))?;

    // Remove it from the remaining input
    let input = &input[first_piece_len..];

    // If the remaining input is empty, the first sub-piece was the whole piece's content
    if is_finished_line(input) {
        return Ok((
            first_piece,
            first_piece_len + trimmed,
            PieceParserStoppedAt::End,
        ));
    }

    let mut chars = input.chars();

    // Get the first character's following the first sub-piece
    let next_char = chars.next().unwrap();

    // If it's a whitespace...
    if next_char.is_whitespace() {
        // It may or may not be followed by an union separator, which can semantically be surrounded by whitespaces

        // The number of characters looked ahead of the end of the first sub-piece
        let mut looked_ahead = 1;

        // Iterate over remaining characters
        for c in chars {
            // Increase the counter
            looked_ahead += 1;

            // Ignore whitespaces
            if c.is_whitespace() {
                continue;
            }
            // If we find an union separator (|)...
            else if c == '|' {
                // Stop right now.
                // The parent function will be in charge of parsing the next members of the union.
                return Ok((
                    first_piece,
                    first_piece_len + trimmed + looked_ahead,
                    PieceParserStoppedAt::UnionSep,
                ));
            }
            // If we encounter any other character, it means the first sub-piece was just followed by a whitespace to indicate
            // it was going to be followed by another sub-piece.
            else {
                // Decrease the counter
                looked_ahead -= 1;
                break;
            }
        }

        // Stop here. The parent function will be in charge of parsing the following items.
        Ok((
            first_piece,
            first_piece_len + trimmed + looked_ahead,
            PieceParserStoppedAt::ContinuationSep,
        ))
    }
    // If we find an union separator (|)...
    else if next_char == '|' {
        // Stop right now.
        // The parent function will be in charge of parsing the next members of the union.
        Ok((
            first_piece,
            first_piece_len + trimmed + 1,
            PieceParserStoppedAt::UnionSep,
        ))
    }
    // If we find any other character...
    else {
        // That's an error, as the content should not end right now.
        Err(ParserError::new(
            ParserLoc::new(0, first_piece_len + trimmed),
            1,
            ParserErrorContent::ExpectedPatternSeparatorOrEndOfLine,
            Some("adding another piece to the suite requires a whitespace, or a vertical bar (|) for an union")
        ))
    }
}

/// Parse a pattern's sub-piece, which means a single value
///
/// This function's success return value is the parsed sub-piece and the consumed input length
fn parse_pattern_sub_piece(input: &str) -> Result<(PatternPiece, usize), ParserError> {
    // Determine if the sub-piece is silent
    let (trimmed, is_silent) = parse_pattern_piece_silence(input);
    let input = &input[trimmed..];

    let (value, len) =
    // Check if the value is a constant string
    if let Some((string, len)) = singles::cst_string(input)? {
        (PatternPieceValue::CstString(string), len)
    }
    // Check if the value is a pattern's name
    else if let Some((name, len)) = singles::pattern_name(input)? {
        (PatternPieceValue::Pattern(name), len)
    }
    // Check if the value is a group (`(...)`)
    else if let Some((group, len)) = singles::group(input)? {
        (PatternPieceValue::Group(group), len)
    }
    // If it's none of the above, it is syntax error
    else {
        return Err(ParserError::new(
            ParserLoc::new(0, 0),
            0,
            ParserErrorContent::ExpectedPatternContent,
            Some(match input.chars().next() {
                Some('\'') => "strings require double quotes",
                Some(_) => "You may either open a group with '(', a string with '\"', or specify a pattern's name",
                None => "you need to provide a pattern piece, such as a group, a string or a pattern's name"
            })
        ));
    };

    // Get the sub-piece's repetition model (* + ?) following it
    let repetition = input.chars().nth(len).and_then(PatternRepetition::parse);

    // Success!
    Ok((
        PatternPiece {
            relative_loc: ParserLoc { line: 0, col: 0 },
            value,
            is_silent,
            repetition,
        },
        trimmed + len + if repetition.is_some() { 1 } else { 0 },
    ))
}

/// Parse a possibly silent piece beginning
///
/// If the piece is indicated to be non-capturing (silent), the consumed size will be returned with the `true` value
pub fn parse_pattern_piece_silence(input: &str) -> (usize, bool) {
    if input.starts_with("_:") {
        (2, true)
    } else {
        (0, false)
    }
}

/// Reason by the [piece parser](`parse_pattern_piece`) stopped at a specific moment
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceParserStoppedAt {
    End,
    ContinuationSep,
    UnionSep,
}

/// Syntax tree generated by [the compiler](`parse_peg`)
#[derive(Debug)]
pub struct PegSyntaxTree<'a> {
    patterns: HashMap<&'a str, PatternContent<'a>>,
}

impl<'a> PegSyntaxTree<'a> {
    /// Get the patterns of the syntax tree (the map's keys are the patterns' name)
    pub fn patterns(&self) -> &HashMap<&'a str, PatternContent<'a>> {
        &self.patterns
    }

    /// Get the pattern's main pattern
    pub fn main_pattern(&self) -> &PatternContent<'a> {
        &self.patterns[GRAMMAR_ENTRYPOINT_PATTERN]
    }
}

/// A pattern's content, parsed by the [`parse_pattern_content`] function
#[derive(Debug)]
pub struct PatternContent<'a>(pub(crate) PatternPiece<'a>);

impl<'a> PatternContent<'a> {
    pub fn inner_piece(&self) -> &PatternPiece<'a> {
        &self.0
    }
}

/// A pattern's piece, parsed by the [`parse_pattern_piece`] function
#[derive(Debug)]
pub struct PatternPiece<'a> {
    /// Piece's beginning, relative to its parent
    relative_loc: ParserLoc,

    /// Repetition model
    repetition: Option<PatternRepetition>,

    /// Is the piece silent?
    is_silent: bool,

    /// The piece's value
    value: PatternPieceValue<'a>,
}

impl<'a> PatternPiece<'a> {
    /// Get the piece's beginning, relatively to its parent
    pub fn relative_loc(&self) -> ParserLoc {
        self.relative_loc
    }

    /// Get the piece's repetition model
    pub fn repetition(&self) -> Option<PatternRepetition> {
        self.repetition
    }

    /// Is the piece silent?
    pub fn is_silent(&self) -> bool {
        self.is_silent
    }

    /// Get the piece's value
    pub fn value(&self) -> &PatternPieceValue<'a> {
        &self.value
    }
}

/// [Pattern piece](`PatternPiece`)'s repetition
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternRepetition {
    // The piece can be provided any number of times
    Any,

    // The piece can be provided one or more times
    OneOrMore,

    // The piece can be provided or not
    Optional,
}

impl PatternRepetition {
    /// Check if a symbol is a valid repetition symbol
    pub fn is_valid_symbol(symbol: char) -> bool {
        symbol == '*' || symbol == '+' || symbol == '?'
    }

    /// Try to parse a repetition symbol
    pub fn parse(symbol: char) -> Option<Self> {
        match symbol {
            '*' => Some(Self::Any),
            '+' => Some(Self::OneOrMore),
            '?' => Some(Self::Optional),
            _ => None,
        }
    }

    /// Get the symbol associated to a pattern's repetition model
    pub fn symbol(self) -> char {
        match self {
            Self::Any => '*',
            Self::OneOrMore => '+',
            Self::Optional => '?',
        }
    }
}

/// A single [`PatternPiece`]'s value, indicating which content it must match
#[derive(Debug)]
pub enum PatternPieceValue<'a> {
    /// Match a constant string
    CstString(&'a str),

    /// Match using another pattern's content
    Pattern(&'a str),

    /// Match using a group (will match on the inner piece)
    Group(Rc<PatternPiece<'a>>),

    /// Match a suite of pieces
    Suite(Vec<PatternPiece<'a>>),

    /// Match one of the provided pieces
    /// Evaluation is performed in order, and the first matching piece will be used
    Union(Vec<PatternPiece<'a>>),
}

/// Location in the input grammar
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParserLoc {
    /// Line number
    line: usize,

    /// Column number
    col: usize,
}

impl ParserLoc {
    /// Create a new location
    pub(crate) fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    /// Get the location's line number
    pub fn line(&self) -> usize {
        self.line
    }

    /// Get the location's column number
    pub fn col(&self) -> usize {
        self.col
    }
}
