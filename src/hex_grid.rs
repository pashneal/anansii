use crate::game::Position;
use crate::hex_grid_dsl::Parser;
pub use crate::location::*;
use std::collections::HashSet;
pub use crate::piece::*;

pub type Height = usize;
pub const HEX_GRID_SIZE: usize = 60;
pub const HEX_GRID_CENTER: (usize, usize) = (HEX_GRID_SIZE / 2, HEX_GRID_SIZE / 2);
pub const MAX_HEIGHT: usize = 7;

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
#[derive(Debug, Clone, Hash)]
pub struct HexGrid {
    grid: Vec<Vec<Vec<Option<Piece>>>>,
}

impl HexGrid {
    /// Translates a DSL string with "*" characters and reports the
    /// locations of each * on the resulting board
    pub fn selector(input: &str) -> Vec<HexLocation> {
        Parser::parse_selector(input).expect("Failed to parse selector")
    }

    pub fn from_dsl(input: &str) -> Self {
        Parser::parse_hex_grid(input).expect("Failed to parse input into HexGrid")
    }

    pub fn new() -> HexGrid {
        HexGrid {
            grid: vec![vec![vec![None; MAX_HEIGHT]; HEX_GRID_SIZE]; HEX_GRID_SIZE],
        }
    }

    /// Depth first search on the stack with a location not allowed to be visited
    /// to determine if the location contains a pinned piece
    fn dfs(&self, visited : &mut HashSet<HexLocation>, disallowed : HexLocation, current_location: HexLocation) {
        if visited.contains(&current_location) {
            return;
        }
        if current_location == disallowed {
            return;
        }
        visited.insert(current_location);
        for neighbor in self.get_neighbors(current_location){
            if self.peek(neighbor).len() > 0 {
                self.dfs(visited, disallowed, neighbor);
            }
        }
        
    }
    /// Returns the locations in the hive that are "pinned",
    /// in other words, removing the pieces in that stack would violate the One Hive rule
    ///
    /// returns in board order, that is, first top-to-bottom then left-to-right
    /// Assumes that the pieces on the board already form "One Hive"
    pub fn pinned(&self) -> Vec<HexLocation> {
        let mut pinned = vec![];
        let hive = self.pieces().into_iter().map(|(_, location)| location).collect::<Vec<_>>();

        for &candidate in hive.iter() {
            let mut visited = HashSet::new();
            let neighbors = self.get_neighbors(candidate);
            if neighbors.len() >= 1 {
                self.dfs(&mut visited, candidate, neighbors[0])
            }

            visited.insert(candidate);
            if visited.len() != hive.len() {
                pinned.push(candidate);
            }
        }
        pinned
    }

    /// Returns the stack of pieces surrounding a given location
    /// grouped together by respected stacks
    pub fn get_neighbors(&self, location: HexLocation) -> Vec<HexLocation> {
        let mut neighbors = vec![];
        for direction in Direction::all().iter() {
            let loc = location.apply(*direction);
            if self.peek(loc).len() > 0 {
                neighbors.push(loc);
            }
        }
        neighbors
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

    /// Return piece stacks on the board in "board order", that is,
    /// from top to bottom, then left to right, including their location
    /// in the grid
    pub fn pieces(&self) -> Vec<(Vec<Piece>, HexLocation)> {
        let mut pieces = vec![];

        let left = -(HEX_GRID_CENTER.0 as i8);
        let right = HEX_GRID_CENTER.0 as i8;
        let top = -(HEX_GRID_CENTER.1 as i8);
        let bottom = HEX_GRID_CENTER.1 as i8;

        for y in top..bottom {
            for x in left..right {
                let loc = HexLocation::new(x as i8, y as i8);
                let stack = self.peek(loc);
                if stack.len() > 0 {
                    pieces.push((stack, loc))
                };
            }
        }
        pieces
    }

    fn centralize(location: HexLocation) -> (usize, usize) {
        let (x, y) = (location.x, location.y);
        let (x, y) = (x + HEX_GRID_CENTER.0 as i8, y + HEX_GRID_CENTER.1 as i8);
        (x as usize, y as usize)
    }

    /// Adds a piece to the top of the stack at the given location
    pub fn add(&mut self, piece: Piece, location: HexLocation) {
        let (x, y) = HexGrid::centralize(location);
        for i in 0..MAX_HEIGHT {
            if self.grid[y][x][i].is_none() {
                self.grid[y][x][i] = Some(piece);
                break;
            }
        }
    }

    /// Removes the top-most piece from the stack at the given location
    pub fn remove(&mut self, location: HexLocation) -> Option<Piece> {
        let (x, y) = HexGrid::centralize(location);
        for i in (0..MAX_HEIGHT).rev() {
            if self.grid[y][x][i].is_some() {
                let piece = self.grid[y][x][i];
                self.grid[y][x][i] = None;
                return piece;
            }
        }

        None
    }

    /// Access a copy of the pieces at a given location
    /// stacked from bottom to top
    pub fn peek(&self, location: HexLocation) -> Vec<Piece> {
        let (x, y) = HexGrid::centralize(location);
        self.axial(x, y)
    }

    /// Access the grid using the axial coordinate system,
    /// with only the pieces that are present at the location
    /// https://www.redblobgames.com/grids/hexagons/#coordinates-cube
    fn axial(&self, x: usize, y: usize) -> Vec<Piece> {
        let mut pieces = vec![];
        if x < 0 || x >= HEX_GRID_SIZE || y < 0 || y >= HEX_GRID_SIZE {
            return vec![];
        }
        for piece in &self.grid[y][x] {
            if piece.is_some() {
                pieces.push(piece.unwrap());
            }
        }
        return pieces;
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
        start.push_str("]");
        start
    }

    pub fn num_pieces(&self) -> usize {
        let mut count = 0;
        for y in 0..HEX_GRID_SIZE {
            for x in 0..HEX_GRID_SIZE {
                for i in 0..MAX_HEIGHT {
                    if self.grid[y][x][i].is_some() {
                        count += 1;
                    } else {
                        break;
                    }
                }
            }
        }
        count
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
                        stack_string.push_str(" ");
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
                board.push_str(" ");
            }
            for col in left..=right {
                let pieces = self.oddr(row, col);

                match pieces.len() {
                    0 => board.push_str("."),
                    1 => board.push_str(&pieces[0].to_str()),
                    _ => board.push_str(&format!("{}", pieces.len())),
                }
                // Do not add a space after the last column
                if col != right {
                    board.push_str(" ");
                }
            }
            board.push_str("\n");
        }

        return board;
    }

    /// Returns a bounding box around all present pieces
    /// in the grid according the odd_r format as described here:
    /// https://www.redblobgames.com/grids/hexagons/#coordinates-offset
    fn bounds(&self) -> ((usize, usize), (usize, usize)) {
        let mut min_row = HEX_GRID_SIZE;
        let mut min_col = HEX_GRID_SIZE;
        let mut max_row = 0;
        let mut max_col = 0;

        for row in 0..HEX_GRID_SIZE {
            for col in 0..HEX_GRID_SIZE {
                if self.oddr(row, col).len() > 0 {
                    min_row = min_row.min(row);
                    min_col = min_col.min(col);
                    max_row = max_row.max(row);
                    max_col = max_col.max(col);
                }
            }
        }

        ((min_row, min_col), (max_row, max_col))
    }

    /// Checks to see if the board contains no pieces
    pub fn is_empty(&self) -> bool {
        for y in 0..HEX_GRID_SIZE {
            for x in 0..HEX_GRID_SIZE {
                if self.grid[y][x][0].is_some() {
                    return false;
                }
            }
        }

        true
    }
}

impl PartialEq for HexGrid {
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

#[test]
fn test_board_string_empty() {
    let grid = HexGrid::new();
    let board = grid.board_string();
    let expected = ".";
    assert_eq!(board, expected, "Empty board should be a single dot");
}

#[test]
fn test_board_string_single() {
    let mut grid = HexGrid::new();
    let piece = Piece::new(PieceType::Queen, PieceColor::White);
    grid.add(piece, HexLocation::new(0, 1));
    let board = grid.board_string();
    let expected = concat!(". . .\n", " . Q .\n", ". . .\n",);

    assert_eq!(board, expected, "Should represent a single piece");
}

#[test]
fn test_board_string_single_stack() {
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
fn test_board_string_multiple() {
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
fn test_board_multiple_stacks() {
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
fn test_board_string_padding() {
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
fn test_start_string1() {
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
fn test_start_string2() {
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
fn test_pinned_pieces_single() {
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
pub fn test_piece_pinned_boundary_conditions(){
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
