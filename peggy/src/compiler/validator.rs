use super::errors::{ParserError, ParserErrorContent};
use super::parser::{PatternPiece, PatternPieceValue, PegSyntaxTree};
use super::utils::{
    add_parser_loc, is_builtin_pattern_name, is_external_pattern_name, is_valid_builtin_pattern,
};
use super::ParserLoc;

/// Validate a Peggy expression parsed with [`super::parse_peg_nocheck`]
///
/// Expressions parsed with [`super::parse_peg`] don't require this check, as it is already performed automatically.
pub fn validate_parsed_peg(parsed: &PegSyntaxTree) -> Result<(), ParserError> {
    // Validate each pattern one by one
    for pattern in parsed.patterns().values() {
        validate_piece_recursive(
            parsed,
            pattern.inner_piece().relative_loc(),
            pattern.inner_piece(),
        )?;
    }

    Ok(())
}

/// Validate a [`PatternPiece`] recursively
fn validate_piece_recursive(
    parsed: &PegSyntaxTree,
    loc: ParserLoc,
    piece: &PatternPiece,
) -> Result<(), ParserError> {
    match piece.value() {
        // Constant strings don't need any validation
        PatternPieceValue::CstString(_) => Ok(()),

        // For patterns, ensure the specified one exists
        PatternPieceValue::Pattern(name) => {
            // Uppercase-only patterns cannot be declared normally, and can be used to refer to an external pattern
            // Else, ensure the provided pattern has been declared
            if parsed.patterns().contains_key(name)
                || is_external_pattern_name(name)
                || is_valid_builtin_pattern(name)
            {
                Ok(())
            } else {
                Err(ParserError::new(
                    loc,
                    name.len(),
                    if is_builtin_pattern_name(name) {
                        ParserErrorContent::UnknownBuiltinPattern
                    } else {
                        ParserErrorContent::UnknownPattern
                    },
                    None,
                ))
            }
        }

        // Develop groups
        PatternPieceValue::Group(piece) => validate_piece_recursive(
            parsed,
            add_parser_loc(loc.line(), loc.col(), piece.relative_loc()),
            piece,
        ),

        // Develop suites and unions
        PatternPieceValue::Suite(pieces) | PatternPieceValue::Union(pieces) => {
            for piece in pieces {
                validate_piece_recursive(
                    parsed,
                    add_parser_loc(loc.line(), loc.col(), piece.relative_loc()),
                    piece,
                )?;
            }

            Ok(())
        }
    }
}
