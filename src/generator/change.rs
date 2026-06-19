use crate::piece::Piece;
use crate::bitgrid::mini::*;
use crate::hex_grid::IntoPieces;
use std::collections::HashSet;

pub type Result<T> = std::result::Result<T, String>;

/// TODO: does this size affect performance?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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

pub struct Differ {
}

impl Differ {
    // TODO: ugly function, cleanup or break it up.
    // Additionally, this logic is duplicated for HexGrid in Annotator
    // we can likely dedupe that
    pub fn single_diff(before: &MiniBitGrid, after: &MiniBitGrid) -> Result<Change> {
        let before_pieces = before.pieces();
        let after_pieces = after.pieces();

        let diffs_after = after_pieces.iter()
                                .filter(|stack_location| !before_pieces.contains(stack_location))
                                .map(|(stack, location)| location);

        let diffs_before = before_pieces.iter()
                                .filter(|stack_location| !after_pieces.contains(stack_location))
                                .map(|(stack, location)| location);

        let diffs = diffs_before.chain(diffs_after).collect::<HashSet<_>>();

        if diffs.len() > 2 {
            return Err("More than two location changes detected".to_string());
        }

        if diffs.len() == 0 {
            return Ok(Change::default());
        }
        
        if diffs.len() == 1 {
            let location = diffs.iter().next().unwrap();
            let piece = after_pieces.iter().find(|(_, loc)| loc == *location).map(|(stack, _)| stack.last().unwrap()).unwrap();
            return Ok(Change {
                removed : Diff::default(),
                added: Diff {
                    piece: *piece,
                    board_index: location.board_index,
                    mask : location.mask,
                }
            })
        }

        let diffs = diffs.into_iter().collect::<Vec<_>>();
        let (loc0, loc1) = (diffs[0], diffs[1]);

        let after_stack0 = after_pieces.clone()
                                .into_iter()
                                .find(|(_, location)| location == loc0)
                                .map(|(stack, _)| stack)
                                .unwrap_or(Vec::new());

        let after_stack1 = after_pieces.clone()
                                .into_iter()
                                .find(|(_, location)| location == loc1)
                                .map(|(stack, _)| stack)
                                .unwrap_or(Vec::new());

        let before_stack1 = before_pieces.clone()
                                .into_iter()
                                .find(|(_, location)| location == loc1)
                                .map(|(stack, _)| stack)
                                .unwrap_or(Vec::new());

        let before_stack0 = before_pieces.clone()
                                .into_iter()
                                .find(|(_, location)| location == loc0)
                                .map(|(stack, _)| stack)
                                .unwrap_or(Vec::new());

        let mut change = Change::default();

        if before_stack0.len() > after_stack0.len() {
            change.removed = Diff {
                piece: before_stack0.last().unwrap().clone(),
                board_index: loc0.board_index,
                mask: loc0.mask
            };
        } else if before_stack0.len() < after_stack0.len() {
            change.added = Diff {
                piece: after_stack0.last().unwrap().clone(),
                board_index: loc0.board_index,
                mask: loc0.mask
            };
        }

        if before_stack1.len() > after_stack1.len() {
            change.removed = Diff {
                piece: before_stack1.last().unwrap().clone(),
                board_index: loc1.board_index,
                mask: loc1.mask
            };
        } else if before_stack1.len() < after_stack1.len() {
            change.added = Diff {
                piece: after_stack1.last().unwrap().clone(),
                board_index: loc1.board_index,
                mask: loc1.mask
            };
        }

        Ok(change)
    }
}
