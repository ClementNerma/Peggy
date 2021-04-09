use crate::compiler::{PatternPiece, PatternPieceValue, PegSyntaxTree};

/// Generate a Peggy grammar from its syntax tree
///
/// Useful to get back to the source code after parsing.
///
/// Note that blank lines and comments, as well as additional whitespaces, won't be restored.
pub fn gen_peggy(pst: &PegSyntaxTree) -> String {
    pst.patterns()
        .iter()
        .map(|(name, pattern)| format!("{} = {}", name, gen_peggy_piece(pattern.inner_piece())))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate a Peggy code for a single [`PatternPiece`]
pub fn gen_peggy_piece(piece: &PatternPiece) -> String {
    let piece_value = gen_peggy_piece_value(piece.value());

    if let Some(rep) = piece.repetition() {
        format!("{}{}", piece_value, rep.symbol())
    } else {
        piece_value
    }
}

/// Generate a Peggy code for a single [`PatternPieceValue`]
pub fn gen_peggy_piece_value(value: &PatternPieceValue) -> String {
    match value {
        PatternPieceValue::CstString(string) => format!("{:?}", string),
        PatternPieceValue::Pattern(name) => name.to_string(),
        PatternPieceValue::Group(inner) => format!("({})", gen_peggy_piece(inner.as_ref())),
        PatternPieceValue::Suite(pieces) => pieces
            .iter()
            .map(gen_peggy_piece)
            .collect::<Vec<_>>()
            .join(" "),
        PatternPieceValue::Union(pieces) => pieces
            .iter()
            .map(gen_peggy_piece)
            .collect::<Vec<_>>()
            .join(" | "),
    }
}
