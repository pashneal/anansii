use crate::bitgrid::mini::*;
use crate::constants::EXPECTED_MAX_BRANCHING_FACTOR;
use crate::generator::*;
use crate::hex_grid::{HexGrid, HexLocation};
use crate::uhp::GameType;
pub use crate::piece::*;

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
}


impl MoveGenerator for MiniGenerator {
    type Position = MiniBitGrid;

    fn grasshopper_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.grasshopper_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids
    }
    
    fn pillbug_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.pillbug_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids
    }
    
    fn queen_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.queen_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids
    }

    fn beetle_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.beetle_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids
    }

    fn ant_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.ant_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids
    }

    fn spider_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.spider_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids
    }

    fn ladybug_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.ladybug_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids  
    }

    fn mosquito_moves(&mut self, location: HexLocation) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let pinned = self.grid.is_pinned(location);
        if pinned {
            return Vec::new();
        } 

        let piece = self.grid.top(location)
            .expect("Expected piece at location");

        let changes = MiniBitGrid::decompose(
            self.grid.mosquito_moves(location),
            Some(location),
            piece,
        );

        let grids : Vec<MiniBitGrid> = changes.into_iter().map(|change| {
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect();

        grids
    }
}

impl SwapGenerator for MiniGenerator {
    type Position = MiniBitGrid;

    fn pillbug_swaps(&mut self, location: HexLocation, immobilized: Option<HexLocation>) -> Vec<MiniBitGrid> {
        // TODO make this a little bit more parametric
        let location = location.into();
        let (sources, sinks) = self.grid.pillbug_swaps(location, immobilized.map(|loc| loc.into()));
        //cartesian product
        itertools::iproduct!(sources, sinks).map(|(source, sink)| {
            let piece = self.grid.top(source)
                .expect("Expected piece at source location");
            let change = Change {
                added: Diff{
                    piece,
                    board_index: sink.board_index,
                    mask: sink.mask,
                },
                removed: Diff{
                    piece,
                    board_index: source.board_index,
                    mask: source.mask,
                }
            };
            let mut new_grid = self.grid.clone();
            new_grid.apply_change(change);
            new_grid
        }).collect()
        
    }
}

impl PlacementGenerator<MiniBitGrid> for MiniGenerator {

    fn placements(&self, placing_color: PieceColor) -> Vec<MiniBitGridLocation> {
        let grid = self.current_grid();
        grid.placements(placing_color)
    }

    fn current_grid(&self) -> MiniBitGrid {
        self.grid.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing_utils::positions::test_suite::*;

    #[test]
    fn test_spider_suite() {
        let result = test_spider_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_ant_suite() {
        let result = test_ant_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_grasshopper_suite() {
        let result = test_grasshopper_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_queen_suite() {
        let result = test_queen_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_beetle_suite() {
        let result = test_beetle_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_mosquito_suite() {
        let result = test_mosquito_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_pillbug_moves_suite() {
        let result = test_pillbug_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_ladybug_moves_suite() {
        let result = test_ladybug_moves::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_pillbug_swaps_suite() {
        let result = test_pillbug_swaps::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_mosquito_swaps_suite() {
        let result = test_mosquito_swaps::<MiniGenerator>();
        assert!(result.is_ok());
    }

    #[test]
    fn test_placements_suite() {
        let result = test_placements::<_, MiniGenerator>();
        assert!(result.is_ok());
    }
}
