use super::*;
use crate::bitgrid::mini::*;
use crate::hex_grid::{HexGrid, HexLocation};
use crate::piece::IntoPieces;
use crate::uhp::GameType;

/// Generator that generates legal positions
/// for the MiniBitGrid representation of Hive
#[derive(Clone, Debug)]
pub struct MiniGenerator {
    /// Represents the latest version of the board
    grid: MiniBitGrid,
    game_type: GameType,

    /// Represents the location of the piece that cannot be moved, move, nor take an
    /// action this turn.
    immobilized: Option<MiniBitGridLocation>,
}

impl FromHexGrid for MiniGenerator {
    fn from_hex_grid(
        grid: &HexGrid,
        game_type: GameType,
        previous_change: Option<HexLocation>,
    ) -> Self {
        let result = MiniBitGrid::try_from(grid.clone());
        let grid = result.expect("Failed to convert HexGrid to MiniBitGrid");

        let immobilized = previous_change.map(|location| location.into());

        Self {
            grid,
            game_type,
            immobilized,
        }
    }
}
