use crate::piece::Piece;

/// TODO: does this size affect performance?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Change {
    pub removed: Diff,
    pub added: Diff,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Diff {
    pub piece: Piece,
    pub board_index: usize,
    pub mask: u64,
}
