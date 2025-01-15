use crate::game::Position;
use crate::hex_grid_dsl::Parser;
pub use crate::location::*;
pub use crate::piece::*;
pub use std::collections::HashMap;
use std::collections::HashSet;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum HexGridError {
    #[error("String input cannot be converted to piece")]
    PieceError,
    #[error("Board is illegal, too many pieces on the top of the hive")]
    TooManyPiecesOnHive,
}

pub type Result<T> = std::result::Result<T, HexGridError>;

pub type Height = usize;
pub const HEX_GRID_SIZE: usize = 60;
pub const HEX_GRID_CENTER: (usize, usize) = (HEX_GRID_SIZE / 2, HEX_GRID_SIZE / 2);

/// Represents a hexagonal grid
///
/// The coordinate system is axial as found here:
/// https://www.redblobgames.com/grids/hexagons/
///
/// As pieces can potentially stack, they are filled from the
/// first element of the array to the last
///
/// HexLocation 0,0 is in the center of the grid to make
/// the grid easier to reason about as Hive is a boardless "floating" game
#[derive(Debug, Clone)]
pub struct HexGrid {
    fast_grid: HashMap<(usize, usize), Vec<Piece>>,
}

impl HexGrid {
    /// Translates a DSL string with "*" characters and reports the
    /// locations of each * on the resulting board
    pub fn selector(input: &str) -> Vec<HexLocation> {
        Parser::parse_selector(input).expect("Failed to parse selector")
    }

    /// Translates a typical DSL string into a HexGrid, ignoring the
    /// "*" characters
    pub fn from_dsl(input: &str) -> Self {
        Parser::parse_hex_grid(input).expect("Failed to parse input into HexGrid")
    }

    pub fn new() -> HexGrid {
        HexGrid {
            fast_grid: HashMap::new(),
        }
    }

    /// Depth first search on the stack with a location not allowed to be visited
    /// to determine if the location contains a pinned piece
    fn dfs(
        &self,
        visited: &mut HashSet<HexLocation>,
        disallowed: HexLocation,
        current_location: HexLocation,
    ) {
        if visited.contains(&current_location) {
            return;
        }
        if current_location == disallowed {
            return;
        }
        visited.insert(current_location);
        for neighbor in self.get_neighbors(current_location) {
            if !self.peek(neighbor).is_empty() {
                self.dfs(visited, disallowed, neighbor);
            }
        }
    }

    /// Returns the locations that are neighbors of the current pieces of hive, but
    /// that location contains no pieces
    pub fn outside(&self) -> HashSet<HexLocation> {
        let mut outside = HashSet::new();
        for (_, location) in self.pieces() {
            for direction in Direction::all().iter() {
                let neighbor = location.apply(*direction);
                let stack = self.peek(neighbor);
                if stack.is_empty() {
                    outside.insert(neighbor);
                }
            }
        }

        outside
    }

    /// Returns the locations in the hive that are "pinned",
    /// in other words, removing the pieces in that stack would violate the One Hive rule
    ///
    /// returns in board order, that is, first top-to-bottom then left-to-right
    /// Assumes that the pieces on the board already form "One Hive"
    pub fn pinned(&self) -> Vec<HexLocation> {
        let mut pinned = vec![];
        let hive = self
            .pieces()
            .into_iter()
            .map(|(_, location)| location)
            .collect::<Vec<_>>();

        for &candidate in hive.iter() {
            let mut visited = HashSet::new();
            let neighbors = self.get_neighbors(candidate);
            if !neighbors.is_empty() {
                self.dfs(&mut visited, candidate, neighbors[0])
            }

            visited.insert(candidate);
            if visited.len() != hive.len() {
                pinned.push(candidate);
            }
        }
        pinned
    }

    /// Returns the empty locations surrounding a given location
    pub fn get_empty_neighbors(&self, location: HexLocation) -> Vec<HexLocation> {
        let mut neighbors = vec![];
        for direction in Direction::all().iter() {
            let loc = location.apply(*direction);
            if self.peek(loc).is_empty() {
                neighbors.push(loc);
            }
        }
        neighbors
    }
    /// Returns the non-empty locations surrounding a given location
    pub fn get_neighbors(&self, location: HexLocation) -> Vec<HexLocation> {
        let mut neighbors = vec![];
        for direction in Direction::all().iter() {
            let loc = location.apply(*direction);
            if !self.peek(loc).is_empty() {
                neighbors.push(loc);
            }
        }
        neighbors
    }

    /// Returns locations that are neighbors of an given location but are
    /// "slidable", that is, they do not form gates that are inaccessible for
    /// sliding pieces and maintains contact with at least one of its original neighbors
    ///
    /// Specifies the effective height of the piece, to see if the piece can jump over the gate
    pub fn slidable_locations_3d_height(
        &self,
        location: HexLocation,
        effective_height: usize,
    ) -> Vec<HexLocation> {
        let mut slidable = vec![];
        let original_neighbors = self.get_neighbors(location);

        for direction in Direction::all().iter() {
            let destination = location.apply(*direction);
            let destination_height = self.peek(destination).len();
            let final_height = destination_height + 1;
            let effective_height = final_height.max(effective_height);

            let (left_dir, right_dir) = direction.adjacent();
            let (left, right) = (location.apply(left_dir), location.apply(right_dir));
            let (left_stack, right_stack) = (self.peek(left), self.peek(right));

            // Must be high enough to step over and through gate
            let gate_requirement = left_stack.len().min(right_stack.len());
            if effective_height <= gate_requirement {
                continue;
            }

            let destination_neighbors = self.get_neighbors(destination);
            // maintains contact if the destination has a piece
            // or if the location has a piece under it
            let mut maintains_contact = !self.peek(destination).is_empty();
            maintains_contact = maintains_contact || effective_height > 1;

            for destination_neighbor in destination_neighbors.iter() {
                if original_neighbors.contains(destination_neighbor) {
                    maintains_contact = true;
                    break;
                }
            }

            if maintains_contact {
                slidable.push(destination);
            }
        }
        slidable
    }

    /// Returns locations that are neighbors of an given location but are
    /// "slidable", that is, they do not form gates that are inaccessible for
    /// sliding pieces and maintains contact with at least one of its original neighbors
    ///
    /// "3D" because it allows climbing up the hive
    pub fn slidable_locations_3d(&self, location: HexLocation) -> Vec<HexLocation> {
        let effective_height = self.peek(location).len();
        self.slidable_locations_3d_height(location, effective_height)
    }

    /// Returns locations that are neighbors of an given location but are
    /// "slidable", that is, they do not form gates that are inaccessible for
    /// sliding pieces and maintains contact with at least one of its original neighbors
    ///
    /// "2D" because it ignores the height of the pieces, disallowing climbing up the hive
    pub fn slidable_locations_2d(&self, location: HexLocation) -> Vec<HexLocation> {
        debug_assert!(self.peek(location).len() <= 1); // Cannot climb up the hive
        let all_locations = self.slidable_locations_3d_height(location, 1);
        all_locations
            .into_iter()
            .filter(|&loc| self.peek(loc).is_empty())
            .collect()
    }

    /// Returns the first occurrence of a specified piece in the grid.
    /// The search occurs in board order, that is, from top to bottom, then left to right.
    /// If the piece is not found, None is returned.
    pub fn find(&self, piece: Piece) -> Option<(HexLocation, Height)> {
        for (stack, location) in self.pieces() {
            for (height, &stack_piece) in stack.iter().enumerate() {
                if piece == stack_piece {
                    return Some((location, height));
                }
            }
        }
        None
    }

    fn uncentralize(x: usize, y: usize) -> HexLocation {
        HexLocation::new(
            x as i8 - HEX_GRID_CENTER.0 as i8,
            y as i8 - HEX_GRID_CENTER.1 as i8,
        )
    }

    fn centralize(location: HexLocation) -> (usize, usize) {
        let (x, y) = (location.x, location.y);
        let (x, y) = (x + HEX_GRID_CENTER.0 as i8, y + HEX_GRID_CENTER.1 as i8);
        (x as usize, y as usize)
    }

    /// Adds a piece to the top of the stack at the given location
    pub fn add(&mut self, piece: Piece, location: HexLocation) {
        let (x, y) = HexGrid::centralize(location);
        self.fast_grid.entry((x, y)).or_default().push(piece);
    }

    /// Removes the top-most piece from the stack at the given location
    pub fn remove(&mut self, location: HexLocation) -> Option<Piece> {
        let (x, y) = HexGrid::centralize(location);
        let piece = self
            .fast_grid
            .get_mut(&(x, y))
            .and_then(|stack| stack.pop());
        if self
            .fast_grid
            .get(&(x, y))
            .map(|stack| stack.len())
            .unwrap_or(0)
            == 0
        {
            self.fast_grid.remove(&(x, y));
        }
        piece
    }

    /// Access a copy of the pieces at a given location
    /// stacked from bottom to top
    pub fn peek(&self, location: HexLocation) -> Vec<Piece> {
        let (x, y) = HexGrid::centralize(location);
        self.axial(x, y)
    }

    /// Acces the top most piece at a given location
    pub fn top(&self, location: HexLocation) -> Option<Piece> {
        self.peek(location).last().cloned()
    }

    /// Access the grid using the axial coordinate system,
    /// with only the pieces that are present at the location
    /// https://www.redblobgames.com/grids/hexagons/#coordinates-cube
    fn axial(&self, x: usize, y: usize) -> Vec<Piece> {
        if x >= HEX_GRID_SIZE || y >= HEX_GRID_SIZE {
            return vec![];
        }
        self.fast_grid.get(&(x, y)).unwrap_or(&vec![]).clone()
    }

    pub fn oddr_to_axial(row: usize, col: usize) -> (i8, i8) {
        let q = col as i8 - (row as i8 - ((row as i8) & 1)) / 2;
        let r = row as i8;
        (q, r)
    }

    pub fn evenr_to_axial(row: usize, col: usize) -> (i8, i8) {
        let q = col as i8 - (row as i8 + ((row as i8) & 1)) / 2;
        let r = row as i8;
        (q, r)
    }

    pub fn axial_to_oddr(q: i8, r: i8) -> (usize, usize) {
        let col = q + (r - (r & 1)) / 2;
        let row = r;
        (row as usize, col as usize)
    }

    /// Access the grid using the odd-r coordinate system
    /// https://www.redblobgames.com/grids/hexagons/#coordinates-offset
    fn oddr(&self, row: usize, col: usize) -> Vec<Piece> {
        let (q, r) = HexGrid::oddr_to_axial(row, col);
        if q < 0 {
            return vec![];
        } // out of bounds
        self.axial(q as usize, r as usize)
    }

    /// Outputs the current state of the grid according to the DSL
    /// specified above
    ///
    /// example:
    ///
    ///  . . . . .
    ///   . Q 3 g .
    ///  . . A b .
    ///   . 2 . m .
    ///  . . . . .
    ///
    ///  start - [ 3, -2 ]
    ///
    ///  3 - [G b B]
    ///  2 - [a M]
    pub fn to_dsl(&self) -> String {
        self.board_string() + "\n" + &self.start_string() + "\n\n" + &self.stacks_string()
    }

    /// Returns the coordinate of the top-most and left-most corner of the
    /// grid when returned by board_string() in a form compatible
    /// with the DSL
    ///
    /// Will have the format
    /// start - [ <x> <y> ]
    pub fn start_string(&self) -> String {
        let ((top, left), _) = self.bounds();

        let top_row = top - 1;
        let left_col = left - 1;

        let (left_q, top_r) = HexGrid::oddr_to_axial(top_row, left_col);
        let (left, top) = (
            left_q - HEX_GRID_CENTER.0 as i8,
            top_r - HEX_GRID_CENTER.1 as i8,
        );

        let mut start = "start - [".to_owned();
        start.push_str(&format!(" {} {} ", left, top));
        start.push(']');
        start
    }

    pub fn num_pieces(&self) -> usize {
        self.fast_grid.values().map(|stack| stack.len()).sum()
    }

    /// Outputs the stack part of this current grid according to the DSL
    /// specified above.
    ///
    /// Will have the format:
    /// <number> - [ <piece> <piece> ... ]
    /// <number> - [ <piece> <piece> ... ]
    /// ...
    pub fn stacks_string(&self) -> String {
        let (min, max) = self.bounds();
        let (top, left) = min;
        let (bottom, right) = max;

        let mut stack_string = String::new();

        for row in top..=bottom {
            for col in left..=right {
                let pieces = self.oddr(row, col);
                if pieces.len() > 1 {
                    stack_string.push_str(&format!("{} - [ ", pieces.len()));
                    for piece in pieces {
                        stack_string.push_str(&piece.to_str());
                        stack_string.push(' ');
                    }
                    stack_string.push_str("]\n");
                }
            }
        }

        stack_string
    }

    /// Outputs the board part of this current grid according to the DSL
    /// specified above
    ///
    /// example:
    ///
    ///  . . . . .
    ///   . Q 3 g .
    ///  . . A b .
    ///   . 2 . m .
    ///  . . . . .
    ///
    pub fn board_string(&self) -> String {
        if self.is_empty() {
            return ".".to_owned();
        }

        let (min, max) = self.bounds();
        let (min_row, min_col) = min;
        let (max_row, max_col) = max;

        let left = min_col - 1;
        let right = max_col + 1;
        let bottom = max_row + 1;
        let top = min_row - 1;

        let mut board = String::new();

        for row in top..=bottom {
            if row % 2 == 1 {
                board.push(' ');
            }
            for col in left..=right {
                let pieces = self.oddr(row, col);

                match pieces.len() {
                    0 => board.push('.'),
                    1 => board.push_str(&pieces[0].to_str()),
                    _ => board.push_str(&format!("{}", pieces.len())),
                }
                // Do not add a space after the last column
                if col != right {
                    board.push(' ');
                }
            }
            board.push('\n');
        }

        board
    }

    /// Returns a bounding box around all present pieces
    /// in the grid according the odd_r format as described here:
    /// https://www.redblobgames.com/grids/hexagons/#coordinates-offset
    fn bounds(&self) -> ((usize, usize), (usize, usize)) {
        let mut min_row = HEX_GRID_SIZE;
        let mut min_col = HEX_GRID_SIZE;
        let mut max_row = 0;
        let mut max_col = 0;

        for (_, location) in self.pieces() {
            let (x, y) = HexGrid::centralize(location);
            let (row, col) = HexGrid::axial_to_oddr(x as i8, y as i8);
            min_row = min_row.min(row);
            min_col = min_col.min(col);
            max_row = max_row.max(row);
            max_col = max_col.max(col);
        }

        ((min_row, min_col), (max_row, max_col))
    }

    /// Checks to see if the board contains no pieces
    pub fn is_empty(&self) -> bool {
        self.fast_grid.is_empty()
    }
}

impl PieceIterator for HexGrid {
    fn pieces(&self) -> Vec<(Vec<Piece>, HexLocation)> {
        let mut pieces = vec![];
        for (&(q, r), stack) in self.fast_grid.iter() {
            let location = HexGrid::uncentralize(q, r);
            let (row, col) = HexGrid::axial_to_oddr(q as i8, r as i8);
            pieces.push(((row, col), stack.clone(), location));
        }
        pieces.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));

        pieces
            .into_iter()
            .map(|(_, stack, loc)| (stack, loc))
            .collect::<Vec<_>>()
    }
}

impl std::hash::Hash for HexGrid {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pieces().hash(state);
    }
}

impl PartialEq<HexGrid> for HexGrid {
    fn eq(&self, other: &Self) -> bool {
        other.pieces() == self.pieces()
    }
}

impl Eq for HexGrid {}

impl Position for HexGrid {
    fn new() -> Self {
        HexGrid::new()
    }

    fn from_hex_grid(grid: &HexGrid) -> Self {
        grid.clone()
    }

    fn to_hex_grid(&self) -> HexGrid {
        self.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::HexGrid;
    use super::*;
    use std::collections::HashSet;

    #[test]
    pub(crate) fn test_board_string_empty() {
        let grid = HexGrid::new();
        let board = grid.board_string();
        let expected = ".";
        assert_eq!(board, expected, "Empty board should be a single dot");
    }

    #[test]
    pub(crate) fn test_board_string_single() {
        let mut grid = HexGrid::new();
        let piece = Piece::new(PieceType::Queen, PieceColor::White);
        grid.add(piece, HexLocation::new(0, 1));
        let board = grid.board_string();
        let expected = concat!(". . .\n", " . Q .\n", ". . .\n",);

        assert_eq!(board, expected, "Should represent a single piece");
    }

    #[test]
    pub(crate) fn test_board_string_single_stack() {
        let mut grid = HexGrid::new();
        let piece = Piece::new(PieceType::Queen, PieceColor::White);
        let dummy = Piece::new(PieceType::Pillbug, PieceColor::Black);
        grid.add(piece, HexLocation::new(0, 1));
        grid.add(dummy, HexLocation::new(0, 1));
        let board = grid.board_string();
        let expected = concat!(". . .\n", " . 2 .\n", ". . .\n",);

        assert_eq!(board, expected, "Should represent a single stack");
    }

    #[test]
    pub(crate) fn test_board_string_multiple() {
        let mut grid = HexGrid::new();
        let white_queen = Piece::new(PieceType::Queen, PieceColor::White);
        let white_ant = Piece::new(PieceType::Ant, PieceColor::White);
        let black_beetle = Piece::new(PieceType::Beetle, PieceColor::Black);
        let black_mosquito = Piece::new(PieceType::Mosquito, PieceColor::Black);
        let black_grasshopper = Piece::new(PieceType::Grasshopper, PieceColor::Black);
        let black_spider = Piece::new(PieceType::Spider, PieceColor::Black);
        let white_ladybug = Piece::new(PieceType::Ladybug, PieceColor::White);
        let white_beetle = Piece::new(PieceType::Beetle, PieceColor::White);
        let dummy = Piece::new(PieceType::Pillbug, PieceColor::Black);

        let start = HexLocation::new(10, -3);
        let white_ant_loc = start;
        let white_queen_loc = white_ant_loc.apply(Direction::NW);
        let black_beetle_loc = white_ant_loc.apply(Direction::E);
        let black_mosquito_loc = black_beetle_loc.apply(Direction::SE);
        let black_grasshopper_loc = black_beetle_loc.apply(Direction::NE);
        let stack2_loc = white_ant_loc.apply(Direction::SW);
        let stack3_loc = white_ant_loc.apply(Direction::NE);
        let black_spider_loc = stack2_loc.apply(Direction::SW);
        let white_ladybug_loc = black_spider_loc.apply(Direction::SE);
        let white_beetle_loc = white_ladybug_loc.apply(Direction::SW);

        grid.add(white_queen, white_queen_loc);
        grid.add(white_ant, white_ant_loc);
        grid.add(black_beetle, black_beetle_loc);
        grid.add(black_mosquito, black_mosquito_loc);
        grid.add(black_grasshopper, black_grasshopper_loc);
        grid.add(black_spider, black_spider_loc);
        grid.add(white_ladybug, white_ladybug_loc);
        grid.add(white_beetle, white_beetle_loc);

        grid.add(dummy, stack2_loc);
        grid.add(dummy, stack2_loc);

        grid.add(dummy, stack3_loc);
        grid.add(dummy, stack3_loc);
        grid.add(dummy, stack3_loc);

        let expected = concat!(
            " . . . . . .\n",
            ". . Q 3 g .\n",
            " . . A b . .\n",
            ". . 2 . m .\n",
            " . s . . . .\n",
            ". . L . . .\n",
            " . B . . . .\n",
            ". . . . . .\n",
        );

        let board = grid.board_string();
        println!("{}", board);

        assert_eq!(board, expected);
    }

    #[test]
    pub(crate) fn test_board_multiple_stacks() {
        let mut grid = HexGrid::new();
        let white_queen = Piece::new(PieceType::Queen, PieceColor::White);
        let white_ant = Piece::new(PieceType::Ant, PieceColor::White);
        let black_beetle = Piece::new(PieceType::Beetle, PieceColor::Black);
        let black_mosquito = Piece::new(PieceType::Mosquito, PieceColor::Black);
        let black_grasshopper = Piece::new(PieceType::Grasshopper, PieceColor::Black);
        let black_spider = Piece::new(PieceType::Spider, PieceColor::Black);
        let white_ladybug = Piece::new(PieceType::Ladybug, PieceColor::White);
        let white_beetle = Piece::new(PieceType::Beetle, PieceColor::White);

        let start = HexLocation::new(2, 0);
        let white_ant_loc = start;
        let white_queen_loc = white_ant_loc.apply(Direction::NW);
        let black_beetle_loc = white_ant_loc.apply(Direction::E);
        let black_mosquito_loc = black_beetle_loc.apply(Direction::SE);
        let black_grasshopper_loc = black_beetle_loc.apply(Direction::NE);
        let stack2_loc = white_ant_loc.apply(Direction::SW);
        let stack3_loc = white_ant_loc.apply(Direction::NE);
        let black_spider_loc = stack2_loc.apply(Direction::SW);
        let white_ladybug_loc = black_spider_loc.apply(Direction::SE);
        let white_beetle_loc = white_ladybug_loc.apply(Direction::SW);

        grid.add(white_queen, white_queen_loc);
        grid.add(white_queen, white_queen_loc);
        grid.add(white_queen, white_queen_loc);

        grid.add(white_ant, white_ant_loc);
        grid.add(black_beetle, black_beetle_loc);

        grid.add(black_mosquito, black_mosquito_loc);
        grid.add(black_mosquito, black_mosquito_loc);

        grid.add(black_grasshopper, black_grasshopper_loc);
        grid.add(black_grasshopper, black_grasshopper_loc);

        grid.add(black_spider, black_spider_loc);
        grid.add(white_ladybug, white_ladybug_loc);
        grid.add(white_beetle, white_beetle_loc);

        grid.add(black_beetle, stack2_loc);
        grid.add(black_mosquito, stack2_loc);

        grid.add(white_queen, stack3_loc);
        grid.add(black_grasshopper, stack3_loc);
        grid.add(white_ant, stack3_loc);

        // Modeled after the board string:
        //let expected = concat!(
        //" . . . . . .\n",
        //". . 3 3 2 .\n",
        //" . . A b . .\n",
        //". . 2 . 2 .\n",
        //" . s . . . .\n",
        //". . L . . .\n",
        //" . B . . . .\n",
        //". . . . . .\n",
        //);

        let expected = concat!(
            "3 - [ Q Q Q ]\n",
            "3 - [ Q g A ]\n",
            "2 - [ g g ]\n",
            "2 - [ b m ]\n",
            "2 - [ m m ]\n",
        );
        let board = grid.stacks_string();
        println!("{}", board);

        assert_eq!(board, expected);
    }

    #[test]
    pub(crate) fn test_board_string_padding() {
        let start = HexLocation::new(0, 0);
        let ne = start.apply(Direction::NE);
        let se = start.apply(Direction::SE);

        let expected = concat!(". . .\n", " . Q .\n", ". Q .\n", " . Q .\n", ". . .\n",);

        let mut grid = HexGrid::new();
        let white_queen = Piece::new(PieceType::Queen, PieceColor::White);
        grid.add(white_queen, start);
        grid.add(white_queen, ne);
        grid.add(white_queen, se);

        let board = grid.board_string();
        println!("{}", board);
        assert_eq!(board, expected);

        let start = HexLocation::new(0, 1);
        let nw = start.apply(Direction::NW);
        let sw = start.apply(Direction::SW);

        let expected = concat!(" . . .\n", ". Q .\n", " . Q .\n", ". Q .\n", " . . .\n",);

        let mut grid = HexGrid::new();
        let white_queen = Piece::new(PieceType::Queen, PieceColor::White);
        grid.add(white_queen, start);
        grid.add(white_queen, nw);
        grid.add(white_queen, sw);

        let board = grid.board_string();
        println!("{}", board);
        assert_eq!(board, expected);
    }

    #[test]
    pub(crate) fn test_start_string1() {
        let start = HexLocation::new(0, 0);
        let ne = start.apply(Direction::NE);
        let se = start.apply(Direction::SE);

        // The board looks like this:
        //let expected = concat!(
        //". . .\n",
        //" . Q .\n",
        //". Q .\n",
        //" . Q .\n",
        //". . .\n",
        //);

        let mut grid = HexGrid::new();
        let white_queen = Piece::new(PieceType::Queen, PieceColor::White);
        grid.add(white_queen, start);
        grid.add(white_queen, ne);
        grid.add(white_queen, se);

        let start_string = grid.start_string();
        let expected = "start - [ 0 -2 ]";
        assert_eq!(start_string, expected);
    }

    #[test]
    pub(crate) fn test_start_string2() {
        let start = HexLocation::new(5, -6);
        let nw = start.apply(Direction::NW);
        let sw = start.apply(Direction::SW);

        // board  expected to look like:
        //. . . .
        // . Q . .
        //. . Q .
        // . Q . .
        //. . . .

        let mut grid = HexGrid::new();
        let white_queen = Piece::new(PieceType::Queen, PieceColor::White);
        grid.add(white_queen, start);
        grid.add(white_queen, nw);
        grid.add(white_queen, sw);

        let start_string = grid.start_string();
        let expected = "start - [ 4 -8 ]";
        assert_eq!(start_string, expected);
    }

    #[test]
    pub(crate) fn test_pinned_pieces_single() {
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . a . . .\n",
            " . . a a . .\n",
            ". . a . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let answer = HexGrid::selector(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . * . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));

        assert_eq!(grid.pinned(), answer);
    }

    #[test]
    pub fn test_piece_pinned_boundary_conditions() {
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . a . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let answer = HexGrid::selector(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        assert_eq!(grid.pinned(), answer);

        let grid = HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let answer = HexGrid::selector(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));

        // Vacuously true
        assert_eq!(grid.pinned(), answer);
    }

    #[test]
    pub fn test_no_existing_pieces_pinned() {
        let grid = HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". a a a . .\n",
            " . a . a . .\n",
            ". . a a a .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let answer = HexGrid::selector(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));

        assert_eq!(grid.pinned(), answer);

        let grid = HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". a . . . .\n",
            " . a . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        let answer = HexGrid::selector(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        assert_eq!(grid.pinned(), answer);
    }

    #[test]
    pub fn test_long_pins() {
        let grid = HexGrid::from_dsl(concat!(
            " . . a . . .\n",
            ". a a . a .\n",
            " . . a a . .\n",
            ". . a . a .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));

        let answer = HexGrid::selector(concat!(
            " . . . . . .\n",
            ". . * . . .\n",
            " . . * * . .\n",
            ". . . . . .\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        assert_eq!(grid.pinned(), answer);

        let grid = HexGrid::from_dsl(concat!(
            " a . . . . .\n",
            ". a a . . .\n",
            " a . a a . .\n",
            ". . . . a a\n",
            " . . . . . a\n\n",
            "start - [0 0]\n\n",
        ));
        let answer = HexGrid::selector(concat!(
            " . . . . . .\n",
            ". * * . . .\n",
            " . . * * . .\n",
            ". . . . * *\n",
            " . . . . . .\n\n",
            "start - [0 0]\n\n",
        ));
        assert_eq!(grid.pinned(), answer);
    }

    #[test]
    pub fn test_outside() {
        let board = HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . a . a .\n",
            " . a a a . .\n",
            ". . a . . .\n",
            " . . a a . .\n",
            ". . . . . .\n\n",
            "start - [0 0]\n\n",
        ));

        let expected = HexGrid::selector(concat!(
            " . * * * * .\n",
            ". * a * a *\n",
            " * a a a * .\n",
            ". * a * * .\n",
            " . * a a * .\n",
            ". . * * * .\n\n",
            "start - [0 0]\n\n",
        ));

        let set_expected = expected.iter().cloned().collect::<HashSet<_>>();
        assert_eq!(board.outside().len(), set_expected.len());
        for location in board.outside() {
            assert!(
                set_expected.contains(&location),
                "Location {:?} not found in expected",
                location
            );
        }
    }
}
