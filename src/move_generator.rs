use crate::hex_grid::*;
use crate::piece::PIECE_COUNTS;
use crate::uhp::GameType;
use std::collections::HashSet;

/// Represents a HexGrid wrapper that can generate new positions.
/// It will create new boards according to the rules that govern pieces as if the
/// game state could not be swapped by the Pillbug.
///
/// For moves of the pillbug and pillbug adjacent pieces, see pillbug_swaps() and pillbug_moves()
///
/// The move generator is only guaranteed to generate moves correctly
/// for positions that follow the One Hive Rule
pub struct MoveGeneratorDebugger {
    grid: HexGrid,
    pinned: Vec<HexLocation>,
    outside: HashSet<HexLocation>,
    game_type: GameType,
    last_move: Option<HexLocation>,
}

impl MoveGeneratorDebugger {
    pub fn new(game_type: GameType) -> MoveGeneratorDebugger {
        MoveGeneratorDebugger {
            grid: HexGrid::new(),
            pinned: Vec::new(),
            outside: HashSet::new(),
            game_type,
            last_move: None,
        }
    }

    pub fn from_grid(
        grid: &HexGrid,
        game_type: GameType,
        last_move: Option<HexLocation>,
    ) -> MoveGeneratorDebugger {
        MoveGeneratorDebugger {
            grid: grid.clone(),
            pinned: grid.pinned(),
            outside: grid.outside(),
            game_type,
            last_move,
        }
    }

    pub fn from_default_grid(grid: &HexGrid) -> MoveGeneratorDebugger {
        MoveGeneratorDebugger {
            grid: grid.clone(),
            pinned: grid.pinned(),
            outside: grid.outside(),
            game_type: GameType::MLP,
            last_move: None,
        }
    }

    fn spider_dfs(
        &self,
        location: HexLocation,
        mut visited: Vec<HexLocation>,
        depth: usize,
        spider_removed: &HexGrid,
    ) -> Vec<HexLocation> {
        if visited.contains(&location) {
            return vec![];
        }
        visited.push(location);

        if depth == 3 {
            return vec![location];
        }

        let mut result = vec![];

        for slidable_location in spider_removed.slidable_locations_2d(location).iter() {
            let found = self.spider_dfs(
                *slidable_location,
                visited.clone(),
                depth + 1,
                spider_removed,
            );
            result.extend(found);
        }

        result
    }

    /// Returns a list of all possible moves for a spider at a given location
    /// if the spider is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn spider_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        let stack = self.grid.peek(location);
        debug_assert!(stack.len() == 1_usize);
        debug_assert!(
            stack[0].piece_type == PieceType::Spider || stack[0].piece_type == PieceType::Mosquito
        );

        if self.pinned.contains(&location) {
            return vec![];
        }

        let mut spider_removed = self.grid.clone();
        spider_removed.remove(location);

        let new_locations = self.spider_dfs(location, vec![], 0, &spider_removed);
        let deduplicated = new_locations
            .iter()
            .cloned()
            .collect::<HashSet<HexLocation>>();

        let mut result = vec![];

        for new_location in deduplicated.iter() {
            let mut new_grid = self.grid.clone();
            new_grid.remove(location);
            new_grid.add(stack[0], *new_location);
            result.push(new_grid);
        }

        result
    }

    /// Returns a list of all possible moves for a grasshopper at a given location
    /// if the grasshopper is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn grasshopper_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        debug_assert!(self.grid.peek(location).len() == 1);
        debug_assert!(
            self.grid.peek(location)[0].piece_type == PieceType::Grasshopper
                || self.grid.peek(location)[0].piece_type == PieceType::Mosquito
        );

        if self.pinned.contains(&location) {
            return vec![];
        }
        let grasshopper = self.grid.peek(location)[0];

        let mut result = vec![];
        for direction in Direction::all().iter() {
            let mut search_location = location.apply(*direction);

            // No piece to jump over, don't bother searching
            if self.outside.contains(&search_location) {
                continue;
            }
            while !self.outside.contains(&search_location) {
                search_location = search_location.apply(*direction);
            }

            let mut new_grid = self.grid.clone();
            new_grid.remove(location);
            new_grid.add(grasshopper, search_location);
            result.push(new_grid);
        }

        result
    }

    /// Returns a list of all possible moves for a queen at a given location
    /// if the queen is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn queen_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        debug_assert!(self.grid.peek(location).len() == 1);
        debug_assert!(
            self.grid.peek(location)[0].piece_type == PieceType::Queen
                || self.grid.peek(location)[0].piece_type == PieceType::Mosquito
        );

        if self.pinned.contains(&location) {
            return vec![];
        }
        let queen = self.grid.peek(location)[0];
        let mut result = vec![];

        let mut queen_removed = self.grid.clone();
        queen_removed.remove(location);
        let outside = queen_removed.outside();

        for slidable_location in self.grid.slidable_locations_2d(location).iter() {
            if outside.contains(slidable_location) {
                let mut new_grid = self.grid.clone();
                new_grid.remove(location);
                new_grid.add(queen, *slidable_location);
                result.push(new_grid);
            }
        }

        result
    }

    /// Returns a list of all possible moves for an ant at a given location
    /// if the ant is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn ant_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        debug_assert!(self.grid.peek(location).len() == 1);
        debug_assert!(
            self.grid.peek(location)[0].piece_type == PieceType::Ant
                || self.grid.peek(location)[0].piece_type == PieceType::Mosquito
        );

        if self.pinned.contains(&location) {
            return vec![];
        }

        fn dfs(location: HexLocation, visited: &mut HashSet<HexLocation>, grid: &HexGrid) {
            if visited.contains(&location) {
                return;
            }
            visited.insert(location);

            for slidable_location in grid.slidable_locations_2d(location).iter() {
                // In contact with the hive
                if !grid.get_neighbors(*slidable_location).is_empty() {
                    dfs(*slidable_location, visited, grid);
                }
            }
        }

        let mut ant_removed = self.grid.clone();
        let ant = ant_removed.remove(location).unwrap();
        let mut visited = HashSet::new();
        dfs(location, &mut visited, &ant_removed);

        visited.remove(&location);

        let mut result = vec![];
        for location in visited.iter() {
            debug_assert!(self.outside.contains(location));
            let mut new_grid = ant_removed.clone();
            new_grid.add(ant, *location);
            result.push(new_grid);
        }

        result
    }

    /// Returns a list of all possible moves for a beetle at a given location
    /// if the beetle is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn beetle_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        let height = self.grid.peek(location).len();
        debug_assert!(height >= 1);
        debug_assert!(
            self.grid.top(location).unwrap().piece_type == PieceType::Beetle
                || self.grid.top(location).unwrap().piece_type == PieceType::Mosquito
        );

        let hive = self
            .grid
            .pieces()
            .into_iter()
            .map(|(_, loc)| loc)
            .collect::<HashSet<HexLocation>>();

        if self.pinned.contains(&location) && height == 1 {
            return vec![];
        }

        let beetle = self.grid.top(location).unwrap();
        let mut result = vec![];

        let mut beetle_removed = self.grid.clone();
        beetle_removed.remove(location);
        let outside = beetle_removed.outside();

        for slidable_location in self.grid.slidable_locations_3d(location).iter() {
            if outside.contains(slidable_location) || hive.contains(slidable_location) {
                let mut new_grid = self.grid.clone();
                new_grid.remove(location);
                new_grid.add(beetle, *slidable_location);
                result.push(new_grid);
            }
        }

        result
    }

    /// Returns a list of all possible moves for a beetle at a given location
    /// if the beetle is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn ladybug_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        let height = self.grid.peek(location).len();
        debug_assert!(height == 1);

        let piece_type = self.grid.top(location).unwrap().piece_type;
        debug_assert!(piece_type == PieceType::Ladybug || piece_type == PieceType::Mosquito);

        if self.pinned.contains(&location) {
            return vec![];
        }

        // The grid without a "ladybug" on it
        let mut ladybug_removed = self.grid.clone();
        let ladybug = ladybug_removed.remove(location).unwrap();

        let mut outside = ladybug_removed.outside();
        outside.remove(&location);

        let hive = ladybug_removed
            .pieces()
            .into_iter()
            .map(|(_, loc)| loc)
            .collect::<HashSet<HexLocation>>();

        let mut result = vec![];

        // First move unto the hive
        let height = 1;
        let slidable_locs = ladybug_removed.slidable_locations_3d_height(location, height);
        let neighbors = slidable_locs.iter().filter(|loc| hive.contains(loc));

        // Then climb across the hive
        let climb_atop = neighbors.flat_map(|loc| {
            // The height must account for an imaginary ladybug now being on top of
            // the existing board
            let effective_height = ladybug_removed.peek(*loc).len() + 1;
            ladybug_removed.slidable_locations_3d_height(*loc, effective_height)
        });
        let climb_atop = climb_atop.filter(|loc| hive.contains(loc));

        // Then climb off the hive
        let climb_down = climb_atop.flat_map(|loc| {
            let height = ladybug_removed.peek(loc).len() + 1;
            ladybug_removed.slidable_locations_3d_height(loc, height)
        });

        let climb_down = climb_down.filter(|loc| outside.contains(loc));
        let unique_final_moves = climb_down.collect::<HashSet<HexLocation>>();

        for final_move in unique_final_moves {
            let mut new_grid = ladybug_removed.clone();
            new_grid.add(ladybug, final_move);
            result.push(new_grid);
        }

        result
    }

    /// Returns a list of all possible moves for a pillbug at a given location
    /// if the pillbug is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn pillbug_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        let height = self.grid.peek(location).len();
        debug_assert!(height == 1);
        debug_assert!(
            self.grid.top(location).unwrap().piece_type == PieceType::Pillbug
                || self.grid.top(location).unwrap().piece_type == PieceType::Mosquito
        );

        if self.pinned.contains(&location) {
            return vec![];
        }

        let mut pillbug_removed = self.grid.clone();
        let pillbug = pillbug_removed.remove(location).unwrap();

        let mut result = vec![];
        for slidable_location in pillbug_removed.slidable_locations_2d(location).iter() {
            let mut new_grid = self.grid.clone();
            new_grid.remove(location);
            new_grid.add(pillbug, *slidable_location);
            result.push(new_grid);
        }

        result
    }

    /// Returns a list of all positions with each possible swap applied to adjacent pieces by
    /// the top-facing pillbug at a given *location*.
    ///
    /// Adjacent pieces that are not allowed to be swapped are:
    ///
    /// - a piece at the specified *disallowed* location
    /// - pieces in a stack of height > 1
    /// - pieces whose absence would violate the One Hive Rule
    /// - pieces that must pass through a gate of height > 1 to slide on/off the top of the pillbug
    pub fn pillbug_swaps(
        &self,
        pillbug_location: HexLocation,
        disallowed: Option<HexLocation>,
    ) -> Vec<HexGrid> {
        let height = self.grid.peek(pillbug_location).len();
        debug_assert!(height == 1, "The stack must only contain the pillbug");
        debug_assert!(
            self.grid.top(pillbug_location).unwrap().piece_type == PieceType::Pillbug
                || self.grid.top(pillbug_location).unwrap().piece_type == PieceType::Mosquito
        );

        let mut swappable = Vec::new();
        for &candidate_loc in self.grid.get_neighbors(pillbug_location).iter() {
            if let Some(disallowed) = disallowed {
                if candidate_loc == disallowed {
                    continue;
                }
            }

            let candidate_stack = self.grid.peek(candidate_loc);
            if candidate_stack.len() > 1 {
                continue;
            }

            if self.pinned.contains(&candidate_loc) {
                continue;
            }

            // Pretend the candidate moved to height 2 and attempted to slide to
            // the pillbug
            let slidable = self.grid.slidable_locations_3d_height(candidate_loc, 2);
            if !slidable.contains(&pillbug_location) {
                continue;
            }

            swappable.push(candidate_loc);
        }

        let mut empty_neighbors = Vec::new();
        let slidable = self.grid.slidable_locations_3d_height(pillbug_location, 2);
        for &candidate_loc in slidable.iter() {
            if self.grid.peek(candidate_loc).is_empty() {
                empty_neighbors.push(candidate_loc);
            }
        }

        itertools::iproduct!(empty_neighbors, swappable)
            .map(|(destination, source)| {
                let mut new_grid = self.grid.clone();
                let piece = new_grid.remove(source).unwrap();
                new_grid.add(piece, destination);
                new_grid
            })
            .collect()
    }

    /// Returns locations that follow the typical placement rules for a given
    /// color. These are all locations which are:
    ///  1) adjacent to some piece on the hive
    ///  2) not adjacent to a piece of the opposite color
    ///  3) unoccupied
    ///
    /// If the board has no pieces, placement occurs at the center HexLocation
    /// If the board has one piece, placement only needs follow rule 1
    pub fn placements(&self, placing_color: PieceColor) -> Vec<HexLocation> {
        let mut placements = self.outside.clone();

        if self.grid.num_pieces() == 1 {
            let piece_loc = self.grid.pieces().first().unwrap().1;
            return self.grid.get_empty_neighbors(piece_loc);
        }

        for (_, loc) in self.grid.pieces() {
            let Some(piece) = self.grid.top(loc) else {
                continue;
            };
            if piece.color == placing_color {
                continue;
            }
            for neighbor in self.grid.get_empty_neighbors(loc) {
                placements.remove(&neighbor);
            }
        }

        if self.grid.is_empty() {
            placements.insert(HexLocation::center());
        }

        placements.into_iter().collect()
    }

    /// Returns a list of all possible moves for a mosquito at a given location
    /// if the mosquito is not covered by any other pieces.
    /// (ignores pillbug swaps)
    pub fn mosquito_moves(&self, location: HexLocation) -> Vec<HexGrid> {
        use PieceType::*;
        let height = self.grid.peek(location).len();
        debug_assert!(height >= 1);
        debug_assert!(self.grid.top(location).unwrap().piece_type == PieceType::Mosquito);

        if self.pinned.contains(&location) && height == 1 {
            return vec![];
        }

        if height > 1 {
            return self.beetle_moves(location);
        }

        let mut adjacent_pieces = Vec::new();
        for neighbor in self.grid.get_neighbors(location) {
            let piece = self.grid.top(neighbor).unwrap().piece_type;
            adjacent_pieces.push(piece);
        }

        let mut grids = HashSet::new();
        for piece in adjacent_pieces {
            match piece {
                Mosquito => {}
                Spider => {
                    grids.extend(self.spider_moves(location).into_iter());
                }
                Grasshopper => {
                    grids.extend(self.grasshopper_moves(location).into_iter());
                }
                Queen => {
                    grids.extend(self.queen_moves(location).into_iter());
                }
                Ant => {
                    grids.extend(self.ant_moves(location).into_iter());
                }
                Beetle => {
                    grids.extend(self.beetle_moves(location).into_iter());
                }
                Ladybug => {
                    grids.extend(self.ladybug_moves(location).into_iter());
                }
                Pillbug => {
                    grids.extend(self.pillbug_moves(location).into_iter());
                }
            }
        }

        grids.into_iter().collect()
    }

    fn pieces_in_hand(&self, color: PieceColor) -> Vec<Piece> {
        let all_pieces = self.grid.pieces();
        let friendly_pieces = all_pieces
            .iter()
            .flat_map(|(stack, _)| stack)
            .filter(|piece| piece.color == color)
            .collect::<Vec<_>>();
        let mut result = Vec::new();

        for piece in PieceType::all(self.game_type) {
            let num_placed = friendly_pieces
                .iter()
                .filter(|p| p.piece_type == piece)
                .count();
            let (_, total) = PIECE_COUNTS
                .iter()
                .find(|(piece_type, _)| *piece_type == piece)
                .unwrap();
            if num_placed < *total {
                result.push(Piece::new(piece, color));
            }
        }

        result
    }

    /// Returns unique legal positions reachable from the current board state
    /// given that it is the turn of the specified color.
    pub fn all_moves_for(&self, color: PieceColor) -> HashSet<HexGrid> {
        let mut positions = HashSet::new();
        let queen = self.grid.find(Piece::new(PieceType::Queen, color));
        let all_pieces = self.grid.pieces();
        let friendly_pieces = all_pieces
            .iter()
            .flat_map(|(stack, _)| stack)
            .filter(|piece| piece.color == color)
            .collect::<Vec<_>>();
        let num_friendly_pieces = friendly_pieces.len();

        // Queen not placed
        if queen.is_none() {
            // Forced to place a queen by 4th turn
            if num_friendly_pieces == 3 {
                for placement in self.placements(color) {
                    let mut new_grid = self.grid.clone();
                    new_grid.add(Piece::new(PieceType::Queen, color), placement);
                    positions.insert(new_grid);
                }
                return positions;
            }
        }

        // 1. Calculate placements
        itertools::iproduct!(self.pieces_in_hand(color), self.placements(color)).for_each(
            |(piece, placement)| {
                let placement_disallowed =
                    piece.piece_type == PieceType::Queen && num_friendly_pieces == 0;

                if !placement_disallowed {
                    let mut new_grid = self.grid.clone();
                    new_grid.add(piece, placement);
                    positions.insert(new_grid);
                }
            },
        );

        // Then 2. Calculate moves
        for (stack, location) in all_pieces {
            let top = stack.last().unwrap();
            if top.color != color {
                continue;
            }
            let moves = match top.piece_type {
                PieceType::Queen => self.queen_moves(location),
                PieceType::Grasshopper => self.grasshopper_moves(location),
                PieceType::Spider => self.spider_moves(location),
                PieceType::Ant => self.ant_moves(location),
                PieceType::Beetle => self.beetle_moves(location),
                PieceType::Ladybug => self.ladybug_moves(location),
                PieceType::Mosquito => self.mosquito_moves(location),
                PieceType::Pillbug => {
                    let mut moves = self.pillbug_swaps(location, self.last_move);
                    moves.extend(self.pillbug_moves(location));
                    moves
                }
            };

            positions.extend(moves.into_iter());
        }

        // If there are no possible moves, return this board to represent th
        // "pass" move
        if positions.is_empty() {
            positions.insert(self.grid.clone());
        }
        positions
    }
}

#[cfg(test)]
mod tests {
    use super::MoveGeneratorDebugger;
    use super::*;
    use crate::testing_utils::compare_moves;

    #[test]
    pub fn test_spider_gate() {
        // Testing with the "gate" structure that disallows free movement
        // between adjacent locations
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . . a . . .\n",
            " . a S a . . .\n",
            ". . a a . . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let legal_moves: Vec<_> = vec![];

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid
            .find(Piece::new(PieceType::Spider, PieceColor::White))
            .unwrap();
        let spider_moves = generator.spider_moves(spider);

        assert!(spider_moves.is_empty());
        assert_eq!(spider_moves, legal_moves);

        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . . a a . .\n",
            " . a . . a . .\n",
            ". . a S a . .\n",
            " . . a a . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let legal_moves = vec![];

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid
            .find(Piece::new(PieceType::Spider, PieceColor::White))
            .unwrap();
        let spider_moves = generator.spider_moves(spider);

        assert!(spider_moves.is_empty());
        assert_eq!(spider_moves, legal_moves);
    }

    #[test]
    pub fn test_spider_pinned() {
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . . a . . .\n",
            " . a S a . . .\n",
            ". . . . . . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
        let spider_moves = generator.spider_moves(spider);
        assert!(spider_moves.is_empty());

        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a . . . .\n",
            " . a S . . . .\n",
            ". . . a a . .\n",
            " . . a . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
        let spider_moves = generator.spider_moves(spider);
        assert!(spider_moves.is_empty());
    }

    #[test]
    pub fn test_spider_door() {
        // Testing with the "door" structure that allows spiders extra mobility than typical
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a S . .\n",
            ". a . . . . .\n",
            " . a a . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));

        let selector = concat!(
            " . . * . . . .\n",
            ". . a a . . .\n",
            " . a * a S . .\n",
            ". a * . . . .\n",
            " . a a * . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
        let spider_moves = generator.spider_moves(spider);
        compare_moves(spider, selector, &grid, &spider_moves);

        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a . . .\n",
            ". a . . S . .\n",
            " . a a . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));

        let selector = concat!(
            " . . . * . . .\n",
            ". . a a . . .\n",
            " . a * a . . .\n",
            ". a * . S . .\n",
            " . a a . . . .\n",
            ". . . * . . .\n\n",
            "start - [0 0]\n\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
        let spider_moves = generator.spider_moves(spider);
        compare_moves(spider, selector, &grid, &spider_moves);

        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a a a . .\n",
            " . a S . a . .\n",
            ". . a . a . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));

        let selector = concat!(
            " . . . . . . .\n",
            ". . a a a . .\n",
            " . a S . a . .\n",
            ". . a . a . .\n",
            " . * * * * . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
        let spider_moves = generator.spider_moves(spider);
        compare_moves(spider, selector, &grid, &spider_moves);
    }

    #[test]
    pub fn test_spider_typical_boards() {
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a . . .\n",
            ". a . . S . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            " . . . * . . .\n",
            ". . a a . . .\n",
            " . a . a . . .\n",
            ". a * . S . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
        let spider_moves = generator.spider_moves(spider);
        compare_moves(spider, selector, &grid, &spider_moves);
    }

    #[test]
    pub fn test_grasshopper() {
        use PieceColor::*;
        use PieceType::*;
        // Tests:
        //  gaps,
        //  multiple directions
        //  0 pieces to jump over
        //  1 piece to jump over
        //  >1 pieces to jump over
        let grid = HexGrid::from_dsl(concat!(
            ". a a a . . .\n",
            " . . . a . . .\n",
            ". . a a . . .\n",
            " . a G a a a .\n",
            ". . . . . . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));

        let selector = concat!(
            ". a a a * . .\n",
            " . * . a . . .\n",
            ". . a a . . .\n",
            " * a G a a a *\n",
            ". . . . . . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (grasshopper, _) = grid.find(Piece::new(Grasshopper, White)).unwrap();
        let grasshopper_moves = generator.grasshopper_moves(grasshopper);
        compare_moves(grasshopper, selector, &grid, &grasshopper_moves);
    }

    #[test]
    pub fn test_grasshopper_pinned() {
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            ". . . a . . .\n",
            " . . a a . . .\n",
            ". . a . . . .\n",
            " . a G a a a .\n",
            ". . . a . . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (grasshopper, _) = grid.find(Piece::new(Grasshopper, White)).unwrap();
        let grasshopper_moves = generator.grasshopper_moves(grasshopper);
        assert!(grasshopper_moves.is_empty());
    }

    #[test]
    pub fn test_queen_pinned() {
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a . . . .\n",
            " . a . a . . .\n",
            ". a . . Q . .\n",
            " . a a a . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (queen, _) = grid.find(Piece::new(Queen, White)).unwrap();
        let queen_moves = generator.queen_moves(queen);
        assert!(queen_moves.is_empty());
    }

    #[test]
    pub fn test_queen_moves() {
        use PieceColor::*;
        use PieceType::*;
        // Test gate structure
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a . . .\n",
            ". a . . Q . .\n",
            " . a a a . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a * . .\n",
            ". a . . Q . .\n",
            " . a a a * . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (queen, _) = grid.find(Piece::new(Queen, White)).unwrap();
        let queen_moves = generator.queen_moves(queen);
        compare_moves(queen, selector, &grid, &queen_moves);

        // Testing typical # of moves
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a . . .\n",
            ". a . . Q . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a * . .\n",
            ". a . * Q . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (queen, _) = grid.find(Piece::new(Queen, White)).unwrap();
        let queen_moves = generator.queen_moves(queen);
        compare_moves(queen, selector, &grid, &queen_moves);

        // Testing "door" structure
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a . a . . .\n",
            ". a . Q . . .\n",
            " . a a . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            " . . . . . . .\n",
            ". . a a . . .\n",
            " . a * a . . .\n",
            ". a * Q * . .\n",
            " . a a * . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (queen, _) = grid.find(Piece::new(Queen, White)).unwrap();
        let queen_moves = generator.queen_moves(queen);
        compare_moves(queen, selector, &grid, &queen_moves);
    }

    #[test]
    pub(crate) fn test_queen_slide() {
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " a . . a a . .\n",
            ". a . Q . a .\n",
            " a . . a a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " a . * a a . .\n",
            ". a . Q . a .\n",
            " a . * a a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (queen, _) = grid.find(Piece::new(Queen, White)).unwrap();
        let queen_moves = generator.queen_moves(queen);
        compare_moves(queen, selector, &grid, &queen_moves);
    }

    #[test]
    pub fn test_ant_moves() {
        use PieceColor::*;
        use PieceType::*;
        // Test with doors, gates, and typical moves
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . . . .\n",
            ". . . g g g . . .\n",
            " . . g . . g g . .\n",
            ". . . . . g . g .\n",
            " . . g g g . g . .\n",
            ". . . . A . . . .\n",
            " . . . . . . . . .\n",
            ". . . . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            " . . * * * * . . .\n",
            ". . * g g g * * .\n",
            " . * g . . g g * .\n",
            ". . * . . g . g *\n",
            " . * g g g * g * .\n",
            ". . * * A * * * .\n",
            " . . . . . . . . .\n",
            ". . . . . . . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (ant, _) = grid.find(Piece::new(Ant, White)).unwrap();
        let ant_moves = generator.ant_moves(ant);
        compare_moves(ant, selector, &grid, &ant_moves);
    }

    #[test]
    pub(crate) fn test_ant_pinned() {
        use PieceColor::*;
        use PieceType::*;

        let grid = HexGrid::from_dsl(concat!(
            " . . . . . . . . .\n",
            ". . . g g g . . .\n",
            " . . g . . g g . .\n",
            ". . . . . g . g .\n",
            " . . g A g . g . .\n",
            ". . . . . . . . .\n",
            " . . . . . . . . .\n",
            ". . . . . . . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (ant, _) = grid.find(Piece::new(Ant, White)).unwrap();
        let ant_moves = generator.ant_moves(ant);
        assert!(ant_moves.is_empty());
    }

    #[test]
    pub(crate) fn test_beetle_gate_lower_level() {
        use PieceColor::*;
        use PieceType::*;
        // Tests slide, climb up (always unblocked if only lower level gates exist), down
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " a . . a a . .\n",
            ". a . B . a .\n",
            " a . . a a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " a . * * a . .\n",
            ". a . B . a .\n",
            " a . * * a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        compare_moves(beetle, selector, &grid, &beetle_moves);

        // Can ignore lower level gate when climbing up
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " a . . a a . .\n",
            ". a . B a a .\n",
            " a . . a a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " a . * * a . .\n",
            ". a . B * a .\n",
            " a . * * a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        compare_moves(beetle, selector, &grid, &beetle_moves);

        // Can ignore lower level gate when climbing down
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " a . . a . . .\n",
            ". a . . 2 . .\n",
            " a . . a a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a B]\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " a . . * * . .\n",
            ". a . * 2 * .\n",
            " a . . * * . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a B]\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        compare_moves(beetle, selector, &grid, &beetle_moves);
    }

    #[test]
    pub(crate) fn test_beetle_gate_upper_level() {
        // Test when the beetle is on top of the hive with these situations:
        // slide (blocked/unblocked), up (blocked/unblocked), down(blocked/unblocked)
        use PieceColor::*;
        use PieceType::*;

        // slide unblocked
        // slide blocked
        // climb up unblocked
        // climb down unblocked
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . 3 a . .\n",
            ". . . a 2 a .\n",
            " . . . 3 . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "3 - [a b b]\n",
            "2 - [a B]\n",
            "3 - [a b b]\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " . . . * * . .\n",
            ". . . a 2 * .\n",
            " . . . * * . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        compare_moves(beetle, selector, &grid, &beetle_moves);

        // climb up blocked
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . 3 a . .\n",
            ". . . 2 B a .\n",
            " . . . 4 . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "3 - [a b b]\n",
            "2 - [a b]\n",
            "4 - [a b b b]\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " . . . * * . .\n",
            ". . . 2 B * .\n",
            " . . . * . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        compare_moves(beetle, selector, &grid, &beetle_moves);

        // slide blocked
        // climb down blocked
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . 2 . . .\n",
            ". . . a 2 2 .\n",
            " . . . 4 a . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a b]\n",
            "2 - [a B]\n",
            "2 - [a b]\n",
            "4 - [a b b b]\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " . . . * . . .\n",
            ". . . a 2 * .\n",
            " . . . * a . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        compare_moves(beetle, selector, &grid, &beetle_moves);
    }

    #[test]
    pub(crate) fn test_beetle_pinned() {
        // Test with a beetle that is pinned
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . a . . .\n",
            ". . . . B . .\n",
            " . . . a . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        assert!(beetle_moves.is_empty());
    }

    #[test]
    pub(crate) fn test_beetle_pinned_top() {
        // Test with a beetle that is on top of a pinned piece,
        // but is not pinned itself
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . a . . .\n",
            ". . . 2 . . .\n",
            " . . . a . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a B]\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " . . * * . . .\n",
            ". . * 2 * . .\n",
            " . . * * . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (beetle, _) = grid.find(Piece::new(Beetle, White)).unwrap();
        let beetle_moves = generator.beetle_moves(beetle);
        compare_moves(beetle, selector, &grid, &beetle_moves);
    }

    #[test]
    pub(crate) fn test_ladybug_moves() {
        //  Test when ladybug moves across the hive with several situations:
        //  blocked climb up, blocked slide on upper level, blocked climb down
        //  unblocked climb up, unblocked slide on upper level, unblocked climb down
        use PieceColor::*;
        use PieceType::*;

        // unblocked slide
        // blocked slide
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . 2 2 . . .\n",
            ". . a a L . .\n",
            " . . 2 a . . .\n",
            ". . . . . . .\n\n",
            " . . . . . a .\n",
            "start - [0 0]\n\n",
            "2 - [a b]\n",
            "2 - [a b]\n",
            "2 - [a b]\n",
        ));
        let selector = concat!(
            ". . * * * . .\n",
            " . * 2 2 * . .\n",
            ". . a a L . .\n",
            " . * 2 a * . .\n",
            ". . * * * . .\n\n",
            " . . . . . a .\n",
            "start - [0 0]\n\n",
            "2 - [a b]\n",
            "2 - [a b]\n",
            "2 - [a b]\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (ladybug, _) = grid.find(Piece::new(Ladybug, White)).unwrap();
        let ladybug_moves = generator.ladybug_moves(ladybug);
        compare_moves(ladybug, selector, &grid, &ladybug_moves);

        // climb up blocked
        // climb up unblocked
        // climb down blocked
        // climb down unblocked
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . a .\n",
            " . . . 2 a . .\n",
            ". . . . L 2 .\n",
            " . . . 2 a . .\n",
            ". . . . a 2 .\n",
            " . . . . . a .\n\n",
            "start - [0 0]\n\n",
            "2 - [a b]\n",
            "2 - [a b]\n",
            "2 - [a b]\n",
            "2 - [a b]\n"
        ));
        let selector = concat!(
            ". . . . * . .\n",
            " . . . . . * .\n",
            ". . . . L . .\n",
            " . . . . . . .\n",
            ". . . * . . .\n",
            " . . . * * . .\n\n",
            "start - [0 0]\n\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (ladybug, _) = grid.find(Piece::new(Ladybug, White)).unwrap();
        let ladybug_moves = generator.ladybug_moves(ladybug);
        compare_moves(ladybug, selector, &grid, &ladybug_moves);
    }

    #[test]
    pub(crate) fn test_ladybug_pinned() {
        use PieceColor::*;
        use PieceType::*;
        // unblocked slide
        // blocked slide
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . 2 . . .\n",
            ". . a . L . .\n",
            " . . 2 a . . .\n",
            ". . . . . . .\n\n",
            " . . . . . a .\n",
            "start - [0 0]\n\n",
            "2 - [a b]\n",
            "2 - [a b]\n",
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (ladybug, _) = grid.find(Piece::new(Ladybug, White)).unwrap();
        let ladybug_moves = generator.ladybug_moves(ladybug);
        assert!(ladybug_moves.is_empty());
    }

    #[test]
    pub(crate) fn test_pillbug_moves() {
        use PieceColor::*;
        use PieceType::*;
        // Testing gates and when adjacent moves would break the One Hive Rule
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " a . . a a . .\n",
            ". a . P . a .\n",
            " a . . a a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " a . * a a . .\n",
            ". a . P . a .\n",
            " a . * a a . .\n",
            ". a a a . . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (pillbug, _) = grid.find(Piece::new(Pillbug, White)).unwrap();
        let pillbug_moves = generator.pillbug_moves(pillbug);
        compare_moves(pillbug, selector, &grid, &pillbug_moves);

        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " a . . a a . .\n",
            ". a . P . a .\n",
            " a . . . a . .\n",
            ". a a a a . .\n\n",
            "start - [0 0]\n\n"
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " a . * a a . .\n",
            ". a . P * a .\n",
            " a . . . a . .\n",
            ". a a a a . .\n\n",
            "start - [0 0]\n\n"
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (pillbug, _) = grid.find(Piece::new(Pillbug, White)).unwrap();
        let pillbug_moves = generator.pillbug_moves(pillbug);
        compare_moves(pillbug, selector, &grid, &pillbug_moves);
    }

    #[test]
    pub(crate) fn test_pillbug_swaps() {
        use PieceColor::*;
        use PieceType::*;

        // Testing strategy
        //
        // for pieces adjacent to the pillbug:
        //   -  [x] Pinned/ [x] Unpinned
        //   -  [x] a location disallowed vs [x] free
        //   -  [x] Blocked entrance by upper level gate vs [x] Blocked exit vs [x] free
        //   -  [x] Under stack vs [x] free
        //   -  [x] 0 free spaces, [x] 1 free space, [x] >1 free spaces
        // for pillbug:
        //   - [x] unpinned/ [x] pinned

        // tests covered:
        //  -  >1 free space
        //  -  pillbug pinned
        //  -  Blocked exit by upper level gate
        //  -  unblocked by upper level gate
        //  -  adjacent piece is not pinned
        //  -  under stack
        //  -  not under stack
        //  -  locations allowed
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . 2 . . .\n",
            ". . 2 P . . .\n",
            " . . l . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [q b]\n",
            "2 - [m b]\n"
        ));

        let expected = vec![
            HexGrid::from_dsl(concat!(
                ". . . . . . .\n",
                " . . . 2 . . .\n",
                ". . 2 P l . .\n",
                " . . . . . . .\n",
                ". . . . . . .\n\n",
                "start - [0 0]\n\n",
                "2 - [q b]\n",
                "2 - [m b]\n"
            )),
            HexGrid::from_dsl(concat!(
                ". . . . . . .\n",
                " . . . 2 . . .\n",
                ". . 2 P . . .\n",
                " . . . l . . .\n",
                ". . . . . . .\n\n",
                "start - [0 0]\n\n",
                "2 - [q b]\n",
                "2 - [m b]\n"
            )),
        ];

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (pillbug, _) = grid.find(Piece::new(Pillbug, White)).unwrap();
        let pillbug_moves = generator.pillbug_swaps(pillbug, None);
        assert_eq!(pillbug_moves.len(), expected.len());
        for grid in expected {
            assert!(
                pillbug_moves.contains(&grid),
                "Expected grid not found in pillbug_moves: \n{}",
                grid.to_dsl()
            );
        }

        // adjacent piece pinned
        // pillbug unpinned
        // location disallowed
        // 1 free space
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . b q . . .\n",
            ". . 2 P a a .\n",
            " . . l . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [m b]\n"
        ));

        let expected = vec![
            HexGrid::from_dsl(concat!(
                ". . . . . . .\n",
                " . . . q . . .\n",
                ". . 2 P a a .\n",
                " . . l b . . .\n",
                ". . . . . . .\n\n",
                "start - [0 0]\n\n",
                "2 - [m b]\n"
            )),
            HexGrid::from_dsl(concat!(
                ". . . . . . .\n",
                " . . b q . . .\n",
                ". . 2 P a a .\n",
                " . . . l . . .\n",
                ". . . . . . .\n\n",
                "start - [0 0]\n\n",
                "2 - [m b]\n"
            )),
        ];

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (pillbug, _) = grid.find(Piece::new(Pillbug, White)).unwrap();
        let (queen, _) = grid.find(Piece::new(Queen, Black)).unwrap();
        let pillbug_moves = generator.pillbug_swaps(pillbug, Some(queen));

        assert_eq!(pillbug_moves.len(), expected.len());
        for grid in expected {
            assert!(
                pillbug_moves.contains(&grid),
                "Expected grid not found in pillbug_moves: \n{}",
                grid.to_dsl()
            );
        }

        // 0 free space
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . b q . . .\n",
            ". . 2 P a a .\n",
            " . . l a . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [m b]\n"
        ));

        let expected = vec![];

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (pillbug, _) = grid.find(Piece::new(Pillbug, White)).unwrap();
        let (queen, _) = grid.find(Piece::new(Queen, Black)).unwrap();
        let pillbug_moves = generator.pillbug_swaps(pillbug, Some(queen));

        assert_eq!(pillbug_moves.len(), expected.len());
        for grid in expected {
            assert!(
                pillbug_moves.contains(&grid),
                "Expected grid not found in pillbug_moves: \n{}",
                grid.to_dsl()
            );
        }

        // Blocked entrance by upper level gate
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . b 3 . . .\n",
            ". . 2 P a . .\n",
            " . . . a . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "3 - [m b b]\n",
            "2 - [m b]\n",
        ));

        let expected = vec![
            HexGrid::from_dsl(concat!(
                ". . . . . . .\n",
                " . . b 3 . . .\n",
                ". . 2 P . . .\n",
                " . . a a . . .\n",
                ". . . . . . .\n\n",
                "start - [0 0]\n\n",
                "3 - [m b b]\n",
                "2 - [m b]\n",
            )),
            HexGrid::from_dsl(concat!(
                ". . . . . . .\n",
                " . . b 3 . . .\n",
                ". . 2 P a . .\n",
                " . . a . . . .\n",
                ". . . . . . .\n\n",
                "start - [0 0]\n\n",
                "3 - [m b b]\n",
                "2 - [m b]\n",
            )),
        ];

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (pillbug, _) = grid.find(Piece::new(Pillbug, White)).unwrap();
        let pillbug_moves = generator.pillbug_swaps(pillbug, None);
        assert_eq!(pillbug_moves.len(), expected.len());
        for grid in expected {
            assert!(
                pillbug_moves.contains(&grid),
                "Expected grid not found in pillbug_moves: \n{}",
                grid.to_dsl()
            );
        }
    }

    #[test]
    pub(crate) fn test_pillbug_pinned_moves() {
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . b . . . .\n",
            ". . 2 P a . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [m b]\n",
        ));

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (pillbug, _) = grid.find(Piece::new(Pillbug, White)).unwrap();
        let pillbug_moves = generator.pillbug_moves(pillbug);
        assert!(pillbug_moves.is_empty());
    }

    #[test]
    pub(crate) fn test_placements() {
        use PieceColor::*;
        // Tests interesting interactions with enemy pieces
        // including stacks effectively change the color
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . A . A . . .\n",
            ". 2 b a A . .\n",
            " . b . 2 . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [A b]\n",
            "2 - [m B]\n",
        ));
        let expected_black_placements = HexGrid::selector(concat!(
            ". . . . . . .\n",
            " . A . A . . .\n",
            "* 2 b a A . .\n",
            " * b . 2 . . .\n",
            ". * * . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [A b]\n",
            "2 - [m B]\n",
        ));
        let expected_white_placements = HexGrid::selector(concat!(
            ". * * * * . .\n",
            " . A . A * . .\n",
            ". 2 b a A * .\n",
            " . b . 2 * . .\n",
            ". . . * * . .\n\n",
            "start - [0 0]\n\n",
            "2 - [A b]\n",
            "2 - [m B]\n",
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let white_placements = generator.placements(White);
        let black_placements = generator.placements(Black);

        assert_eq!(black_placements.len(), expected_black_placements.len());
        assert_eq!(white_placements.len(), expected_white_placements.len());

        for placement in expected_white_placements {
            assert!(
                white_placements.contains(&placement),
                "Expected place not found in white_placements: \n{:?}",
                placement
            );
        }

        for placement in expected_black_placements {
            assert!(
                black_placements.contains(&placement),
                "Expected place not found in black_placements: \n{:?}",
                placement
            );
        }
    }

    #[test]
    pub(crate) fn test_placements_empty() {
        use PieceColor::*;
        // Make sure you place at the center when the board is empty
        let grid = HexGrid::from_dsl(concat!(".\n\n", "start - [0 0]\n\n",));
        let selector = HexGrid::selector(concat!("*\n\n", "start - [0 0]\n\n",));

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let placements = generator.placements(White);
        let expected = selector;
        assert_eq!(placements, expected);
    }

    #[test]
    pub(crate) fn test_placements_single() {
        use PieceColor::*;
        // Regression test: placement allowed in any adjacent
        // hex if the board has a single piece
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . . . . .\n",
            ". . . A . . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let expected = HexGrid::selector(concat!(
            ". . . . . . .\n",
            " . . * * . . .\n",
            ". . * A * . .\n",
            " . . * * . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let placements = generator.placements(Black);
        for placement in expected.iter() {
            assert!(
                placements.contains(placement),
                "Expected place not found in placements: \n{:?}",
                placement
            );
        }
        assert_eq!(placements.len(), expected.len());
    }

    #[test]
    pub(crate) fn test_mosquito_mosquito_no_moves() {
        // Mosquitos beside only other mosquitoes have no moves
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . . . . .\n",
            ". a m M . . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (mosquito, _) = grid.find(Piece::new(Mosquito, White)).unwrap();
        let mosquito_moves = generator.mosquito_moves(mosquito);
        assert!(mosquito_moves.is_empty());
    }

    #[test]
    pub(crate) fn test_lower_level_mosquito_moves() {
        // A mosquito on the lower level adopts the moves of surrounding pieces
        // Mosquitos next to stacks
        use PieceColor::*;
        use PieceType::*;
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . q g . . .\n",
            ". a b M 2 . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a S]\n",
        ));
        let selector = concat!(
            ". . * . * . .\n",
            " . . * * . . .\n",
            "* a * M * * .\n",
            " * . * * . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a S]\n",
        );

        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (mosquito, _) = grid.find(Piece::new(Mosquito, White)).unwrap();
        let mosquito_moves = generator.mosquito_moves(mosquito);
        compare_moves(mosquito, selector, &grid, &mosquito_moves);
    }

    #[test]
    pub(crate) fn test_upper_level_mosquito_moves() {
        use PieceColor::*;
        use PieceType::*;
        // A mosquito on the upper level is functionally a beetle
        // A top level mosquito on top of a pinned piece may still move
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . q . . . .\n",
            ". a b 2 2 . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a M]\n",
            "2 - [a S]\n",
        ));
        let selector = concat!(
            ". . . . . . .\n",
            " . . * * . . .\n",
            ". a * 2 * . .\n",
            " . . * * . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a M]\n",
            "2 - [a S]\n",
        );
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (mosquito, _) = grid.find(Piece::new(Mosquito, White)).unwrap();
        let mosquito_moves = generator.mosquito_moves(mosquito);
        compare_moves(mosquito, selector, &grid, &mosquito_moves);
    }

    #[test]
    pub(crate) fn test_pinned_mosquito() {
        use PieceColor::*;
        use PieceType::*;
        // A lower level mosquito may be pinned and have no moves
        let grid = HexGrid::from_dsl(concat!(
            ". . . . . . .\n",
            " . . . g . . .\n",
            ". a b M 2 . .\n",
            " . . . . . . .\n",
            ". . . . . . .\n\n",
            "start - [0 0]\n\n",
            "2 - [a B]\n",
        ));
        let generator = MoveGeneratorDebugger::from_default_grid(&grid);
        let (mosquito, _) = grid.find(Piece::new(Mosquito, White)).unwrap();
        let mosquito_moves = generator.mosquito_moves(mosquito);
        assert!(mosquito_moves.is_empty());
    }
}
