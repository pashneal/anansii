use crate::bitgrid::mini::*;
use crate::constants::*;
use crate::generator::*;
use crate::hex_grid::{HexGrid, HexLocation};
use crate::uhp::GameType;

/// Generator that generates legal positions
/// for the MiniBitGrid representation of Hive
#[derive(Clone, Debug)]
pub struct MiniGenerator {
    grid: MiniBitGrid,
    game_type: GameType,

    /// Represents the location of the piece that cannot be moved, move, nor take an
    /// action this turn.
    immobilized: Option<MiniBitGridLocation>,

    /// Represents the legal positions generated for a given position.
    ///
    /// It is intended that this buffer avoid allocations by
    /// reusing the same buffer per generation.
    change_buffer: Vec<Change>,
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
            change_buffer: Vec::with_capacity(EXPECTED_MAX_BRANCHING_FACTOR),
        }
    }
}

impl MiniGenerator {
    pub fn apply(&mut self, change: Change) {
        self.grid.apply_change(change);
    }

    pub fn undo(&mut self, change: Change) {
        let added = change.removed;
        let removed = change.added;
        let undo = Change { added, removed };
        self.grid.apply_change(undo);
    }

    pub fn clear_change_buffer(&mut self) {
        self.change_buffer.clear();
    }

    pub fn generate_spider_moves(
        &mut self,
        location: MiniBitGridLocation,
    ) {
        if let Some(immobilized) = self.immobilized {
            if immobilized == location {
                return; 
            }
        }
        if self.grid.is_pinned(location) { return; }
         
    }
}

/// NOTE: everything in this implementation can be slow,
/// because it is simply used for correctness testing.
impl MoveGenerator<MiniBitGrid> for MiniGenerator {
    fn spider_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        self.clear_change_buffer();
        self.generate_spider_moves(location.into());

        let mut boards : Vec<MiniBitGrid> = vec![];
        for change in self.change_buffer.iter() {
            let mut new_board = self.grid.clone();
            new_board.apply_change(*change);
            boards.push(new_board);
        }

        boards
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing_utils::positions::test_suite::*;

    #[test]
    fn test_spider_suite() {
        let result = test_spider_moves::<_, MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_ant_suite() {
        let result = test_ant_moves::<_, MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_grasshopper_suite() {
        let result = test_grasshopper_moves::<_, MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_queen_suite() {
        let result = test_queen_moves::<_, MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_beetle_suite() {
        let result = test_beetle_moves::<_, MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_mosquito_suite() {
        let result = test_mosquito_moves::<_, MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_pillbug_moves_suite() {
        let result = test_pillbug_moves::<_, MiniGenerator>();
        assert!(result.is_ok());
    }
}
