use super::*; 
use crate::location::*;
use crate::piece::*;
use crate::hex_grid::*;
use crate::testing_utils::is_localized;
use std::collections::HashSet;

pub const CENTER_BOARD_INDEX: usize = 24;
pub const CENTER_BITBOARD_INDEX : usize = 28;
pub const GRID_WIDTH: usize = 7;
pub const GRID_HEIGHT: usize = 7;
pub const GRID_SIZE: usize = GRID_WIDTH*GRID_HEIGHT;
pub const MAX_WRAP_BEFORE_COLLISION: usize = 28;
pub type GridLocation = usize;

/// Represents positions of Hive with Pillbug Mosquito and Ladybug 
/// that follow the One Hive rule and has no greater than 7 pieces atop the hive
///
/// Only beetles and mosquitos can be at height > 1 in this representation
/// as opposed to HexGrid which is more relaxed in its constraints.
///
/// TODO: may have to refactor tests
///
/// See the documentation of the bit grid's AxialBitboard to
/// understand how the grid is represented at the bit level
///
/// Zooming out, the grid is represented instead as
/// a 7x7 grid of AxialBitboards
///
/// The grid is represented as follows:
/// ```
///    48 47 46 45 44 43 42
///    41 40 39 38 37 36 35
///    34 33 32 31 30 29 28
///    27 26 25 24 23 22 21
///    20 19 18 17 16 15 14
///    13 12 11 10 09 08 07
///    06 05 04 03 02 01 00
/// ```
///
/// The center is assigned to board index 24 at the bitboard index 28
pub type Grid = [AxialBitboard; GRID_SIZE];
#[derive(Debug)]
pub struct BasicBitGrid {
    pub queens: Grid,
    pub beetles:Grid,
    pub spiders:Grid,
    pub grasshoppers: Grid,
    pub ants: Grid,
    pub pillbugs: Grid,
    pub ladybugs: Grid,
    pub mosquitos: Grid,
    pub all_pieces: Grid,
    pub white_pieces: Grid,
    pub black_pieces: Grid,
    pub stacks : BasicBitStack
}


impl BasicBitGrid {
    pub fn new() -> Self {
        BasicBitGrid {
            queens: [AxialBitboard::from_u64(0); GRID_SIZE],
            beetles: [AxialBitboard::from_u64(0); GRID_SIZE],
            spiders: [AxialBitboard::from_u64(0); GRID_SIZE],
            grasshoppers: [AxialBitboard::from_u64(0); GRID_SIZE],
            ants: [AxialBitboard::from_u64(0); GRID_SIZE],
            pillbugs: [AxialBitboard::from_u64(0); GRID_SIZE],
            ladybugs: [AxialBitboard::from_u64(0); GRID_SIZE],
            mosquitos: [AxialBitboard::from_u64(0); GRID_SIZE],
            all_pieces: [AxialBitboard::from_u64(0); GRID_SIZE],
            white_pieces: [AxialBitboard::from_u64(0); GRID_SIZE],
            black_pieces: [AxialBitboard::from_u64(0); GRID_SIZE],
            stacks: BasicBitStack::new()
        }
    }

    pub fn add(&mut self, piece : Piece, loc : BitGridLocation) {
        debug_assert!(loc.board_index < GRID_SIZE);
        let color = piece.color;
        let piece = piece.piece_type;

        let all_pieces  = self.get_mut_all_pieces().get_mut(loc.board_index).unwrap();
        if all_pieces.peek(loc.bitboard_index) {
            self.insert_stack(piece, loc, color);
        } else {
            *all_pieces |= 1 << loc.bitboard_index;

            let board = self.get_mut_piece(piece).get_mut(loc.board_index).unwrap();
            *board |= 1 << loc.bitboard_index;

            let color = self.get_mut_color(color).get_mut(loc.board_index).unwrap();
            *color |= 1 << loc.bitboard_index;
        }
    }

    fn insert_stack(&mut self, piece: PieceType, loc: BitGridLocation, color: PieceColor) {
        use PieceType::*;
        match piece {
            Beetle | Mosquito => {

                let index = self.stacks.find_one(loc.into());
                let height = if let Some(index) = index {
                    let entry = self.stacks.get(index);
                    let height = entry.height();
                    height
                }else {
                    2 as u8
                };

                let entry = BasicBitStackEntry::new(piece.into(), height, loc.into(), color.into());
                self.stacks.insert(entry);
            }
            _ => panic!("Cannot add a {:?} to the top of the hive", piece)
        }
    }

    pub fn remove(&mut self, piece : Piece, loc : BitGridLocation) {
        debug_assert!(loc.board_index < GRID_SIZE);
        let color = piece.color;
        let piece = piece.piece_type;

        let removed = self.remove_stack(loc);
        if removed {
            return;
        }

        let board = self.get_mut_piece(piece).get_mut(loc.board_index).unwrap();
        *board &= !(1 << loc.bitboard_index);

        let color = self.get_mut_color(color).get_mut(loc.board_index).unwrap();
        *color &= !(1 << loc.bitboard_index);

        let all_pieces  = self.get_mut_all_pieces().get_mut(loc.board_index).unwrap();
        *all_pieces &= !(1 << loc.bitboard_index);
    }

    fn remove_stack(&mut self, loc: BitGridLocation) -> bool {
        let indices = self.stacks.find_all(loc.into());
        if let Some(&highest_index) = indices.last() {
            self.stacks.remove(highest_index);
            true
        } else {
            false
        }
    }

    pub fn get_mut_piece(&mut self, piece_type : PieceType) -> &mut Grid {
        match piece_type {
            PieceType::Queen => &mut self.queens,
            PieceType::Beetle => &mut self.beetles,
            PieceType::Spider => &mut self.spiders,
            PieceType::Grasshopper => &mut self.grasshoppers,
            PieceType::Ant => &mut self.ants,
            PieceType::Pillbug => &mut self.pillbugs,
            PieceType::Ladybug => &mut self.ladybugs,
            PieceType::Mosquito => &mut self.mosquitos,
        }
    }

    pub fn get_piece(&self, piece_type : PieceType) -> &Grid {
        match piece_type {
            PieceType::Queen => &self.queens,
            PieceType::Beetle => &self.beetles,
            PieceType::Spider => &self.spiders,
            PieceType::Grasshopper => &self.grasshoppers,
            PieceType::Ant => &self.ants,
            PieceType::Pillbug => &self.pillbugs,
            PieceType::Ladybug => &self.ladybugs,
            PieceType::Mosquito => &self.mosquitos,
        }
    }

    pub fn peek(&self, loc : BitGridLocation) -> Vec<Piece> {
        let mut pieces = Vec::new();
        // Check for a piece on the ground
        let piece_types = [
            PieceType::Queen,
            PieceType::Beetle,
            PieceType::Spider,
            PieceType::Grasshopper,
            PieceType::Ant,
            PieceType::Pillbug,
            PieceType::Ladybug,
            PieceType::Mosquito,
        ];

        let mut piece_type = None;
        let color;
        for p in piece_types.iter() {
            let board = self.get_piece(*p)[loc.board_index];
            if board.peek(loc.bitboard_index) {
                piece_type = Some(*p);
            }
        }

        color = if self.white_pieces[loc.board_index].peek(loc.bitboard_index) {
            PieceColor::White
        } else {
            PieceColor::Black
        };

        if let Some(piece_type) = piece_type {
            pieces.push(Piece::new(piece_type, color));
        }


        // Check for pieces in the upper level
        let indices = self.stacks.find_all(loc.into()); 
        for index in indices {
            let entry = self.stacks.get(index);
            let piece_type = PieceType::from(entry.piece());
            let color = PieceColor::from(entry.color());
            let piece  = Piece::new(piece_type, color);
            pieces.push(piece);
        }

        pieces
    }
    
    pub fn get_mut_all_pieces(&mut self) -> &mut Grid {
        &mut self.all_pieces
    }

    pub fn get_mut_color(&mut self, color : PieceColor) -> &mut Grid {
        match color {
            PieceColor::White => &mut self.white_pieces,
            PieceColor::Black => &mut self.black_pieces,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BitGridLocation {
    pub board_index : usize,
    pub bitboard_index : usize 
}

impl BitGridLocation {
    pub fn new(board_index: usize, bitboard_index: usize) -> Self {
        BitGridLocation {
            board_index,
            bitboard_index
        }
    }

    fn apply(&self, direction : Direction) ->  BitGridLocation{
        use Direction::*;
        let x = self.bitboard_index % BITBOARD_WIDTH;
        let y = self.bitboard_index / BITBOARD_HEIGHT;
        let board = self.board_index as i8;

        let (x, y) = (x as i8, y as i8);
        let (x, y) = match direction {
            E => (x - 1, y),
            W => (x + 1, y),
            NW => (x, y + 1),
            NE => (x - 1, y + 1),
            SE => (x, y - 1),
            SW => (x + 1, y - 1),
        };

        self.wrap(board, x, y)
    }

    /// Deals with overflow and underflow of the x and y coordinates
    /// of a given board
    fn wrap(&self, board : i8, x : i8, y : i8) -> BitGridLocation {
        let width = BITBOARD_WIDTH as i8;
        let height = BITBOARD_HEIGHT as i8;

        let board_x = board % GRID_WIDTH as i8;
        let board_y = board / GRID_HEIGHT as i8;

        let dx = match x {
            x if x < 0 => -1,
            x if x >= width => 1,
            _ => 0
        };

        let dy = match y {
            y if y < 0 => -1,
            y if y >= height => 1,
            _ => 0
        };

        let board_x = (board_x + dx).rem_euclid(GRID_WIDTH as i8);
        let board_y = (board_y + dy).rem_euclid(GRID_HEIGHT as i8);

        let new_board_index = board_y * GRID_HEIGHT as i8 + board_x;
        let new_bitboard_index = y.rem_euclid(height) * height + x.rem_euclid(width);

        BitGridLocation::new(new_board_index as usize, new_bitboard_index as usize)
    }
}



impl FromHex for BitGridLocation {
    fn from_hex(hex: HexLocation) -> Self {
        let center_x = (CENTER_BITBOARD_INDEX % BITBOARD_WIDTH) as i8; 
        let center_y = (CENTER_BITBOARD_INDEX / BITBOARD_HEIGHT) as i8;
        let board_center_x = (CENTER_BOARD_INDEX % GRID_WIDTH) as i8;
        let board_center_y = (CENTER_BOARD_INDEX / GRID_HEIGHT) as i8;

        let bit_x = ( center_x - hex.x + BITBOARD_WIDTH as i8).rem_euclid(BITBOARD_WIDTH as i8);
        let bit_y = ( center_y - hex.y + BITBOARD_HEIGHT as i8 ).rem_euclid(BITBOARD_HEIGHT as i8);

        let board_x = -hex.x + (board_center_x * BITBOARD_WIDTH as i8) + center_x;
        let board_x = board_x.rem_euclid((BITBOARD_WIDTH * GRID_WIDTH) as i8);
        let board_x = board_x / BITBOARD_WIDTH as i8;

        let board_y = -hex.y + (board_center_y * BITBOARD_HEIGHT as i8) + center_y;
        let board_y = board_y.rem_euclid((BITBOARD_HEIGHT * GRID_HEIGHT) as i8);
        let board_y = board_y / BITBOARD_HEIGHT as i8;

        let board_index = (board_y * GRID_HEIGHT as i8 + board_x) as usize;
        let bitboard_index = (bit_y * BITBOARD_HEIGHT as i8 + bit_x) as usize;

        BitGridLocation::new(board_index, bitboard_index)
    }
}

impl From<HexLocation> for BitGridLocation {
    fn from(hex: HexLocation) -> Self {
        BitGridLocation::from_hex(hex)
    }
}

impl Shiftable for BitGridLocation {
    fn shift_west(&self) -> BitGridLocation {
        self.apply(Direction::W)
    }

    fn shift_east(&self) -> BitGridLocation {
        self.apply(Direction::E)
    }

    fn shift_northwest(&self) -> BitGridLocation {
        self.apply(Direction::NW)
    }

    fn shift_northeast(&self) -> BitGridLocation {
        self.apply(Direction::NE)
    }

    fn shift_southwest(&self) -> BitGridLocation {
        self.apply(Direction::SW)
    }

    fn shift_southeast(&self) -> BitGridLocation {
        self.apply(Direction::SE)
    }

    fn center() -> BitGridLocation {
        BitGridLocation::new(CENTER_BOARD_INDEX, CENTER_BITBOARD_INDEX)
    }
}

impl PieceIterator for BasicBitGrid {
    type Output = BitGridLocation;
    fn pieces(&self) ->  Vec<(Vec<Piece>, Self::Output)> {
        let mut pieces = Vec::new();
        // Needs to be in reverse to go in "board order"
        for board_index in (0..GRID_SIZE).rev() {
            let board = self.all_pieces[board_index];

            // Needs to be in reverse to go in "board order"
            for bitboard_index in (0..BITBOARD_WIDTH*BITBOARD_HEIGHT).rev() {
                if board.peek(bitboard_index) {
                    let loc = BitGridLocation::new(board_index, bitboard_index);
                    let piece = self.peek(loc);
                    pieces.push((piece, loc));
                }
            }
        }
        pieces
    }
}

impl PartialEq<HexGrid> for BasicBitGrid {
    fn eq(&self, other : &HexGrid) -> bool {
        let other_pieces = other.pieces().into_iter();
        let other_pieces = other_pieces.map(|(p, l)| (p, BitGridLocation::from_hex(l)));
        let other_pieces = other_pieces.collect::<Vec<_>>();

        self.pieces() == other_pieces
    }
}

impl TryInto<HexGrid> for BasicBitGrid{
    type Error = HexGridError;
    fn try_into(self) -> Result<HexGrid> {
        let left = -((HEX_GRID_SIZE/2)  as i8);
        let right = (HEX_GRID_SIZE/2)  as i8;
        let top = -((HEX_GRID_SIZE/2)  as i8);
        let bottom = (HEX_GRID_SIZE/2)  as i8;

        let mut start = None;
        for row in top..=bottom {
            for col in left..=right {
                let hex_location = HexLocation::new(row, col);
                let bit_location : BitGridLocation = hex_location.into();
                if self.peek(bit_location).len() > 0 {
                    start = Some(hex_location); 
                }
            }
        }

        // TODO: we assume that if there *is* a piece on the board
        // at least one must be within the bounds of the hex grid, check if 
        // this is a valid assumption down the line
        if start.is_none() {
            return Ok(HexGrid::new()) 
        }

        // Since we know the position must follow the One Hive rule,
        // we do a dfs starting from the start location to find all the pieces
        //
        // Note: the reason we can't convert directly is that there is 
        // no guarantee that BasicBitLocation -> HexLocation is an injective mapping -
        // there may be multiple BasicBitLocations that map to the same HexLocation
        // due to wrapping
        fn dfs(grid : &BasicBitGrid, current_loc: HexLocation, visited: &mut HashSet<HexLocation>, on_hive : &mut usize, result: &mut HexGrid) -> Result<()> {
            if visited.contains(&current_loc) {
                return Ok(());
            }

            let pieces = grid.peek(current_loc.into());

            if pieces.is_empty() {
                return Ok(());
            }

            *on_hive += pieces.len() - 1;

            if *on_hive > 6 {
                return Err(HexGridError::TooManyPiecesOnHive)
            }

            visited.insert(current_loc);
            for piece in pieces {
                result.add(piece, current_loc);
            }
            for direction in Direction::all() {
                let next_loc = current_loc.apply(direction);
                dfs(grid, next_loc, visited, on_hive, result)?;
            }

            return Ok(());
        }

        let mut visited = HashSet::new();
        let mut on_hive = 0;
        let mut result = HexGrid::new();
        match dfs(&self, start.unwrap(), &mut visited, &mut on_hive, &mut result) {
            Err(e) => Err(e),
            _ => Ok(result)
        }
    }
}

// All basic bit grids are legal HexGrids
impl From<HexGrid> for BasicBitGrid {
    fn from(hex_grid: HexGrid) -> Self {
        let mut bit_grid = BasicBitGrid::new();
        for (stack, loc) in hex_grid.pieces() {
            for piece in stack {
                bit_grid.add(piece, BitGridLocation::from_hex(loc));
            }
        }
        bit_grid
    }
}


#[test]
pub fn test_bitgrid_location() {
    let hex = HexLocation::center();
    let bitgrid = BitGridLocation::from_hex(hex);
    assert_eq!(bitgrid, BitGridLocation::center());
}

#[test] 
pub fn test_bitgrid_wrap() {
    let start  = BitGridLocation::center();
    let mut e = start;
    let mut w = start;
    let mut nw = start;
    let mut ne = start;
    let mut sw = start;
    let mut se = start;

    for _ in 0..BITBOARD_WIDTH {
        e = e.shift_east();
        w = w.shift_west();
        nw = nw.shift_northwest();
        ne = ne.shift_northeast();
        sw = sw.shift_southwest();
        se = se.shift_southeast();
    }

    assert_eq!(e.board_index , start.board_index - 1);
    assert_eq!(w.board_index , start.board_index + 1);
    assert_eq!(ne.board_index , start.board_index + GRID_WIDTH - 1);
    assert_eq!(nw.board_index , start.board_index + GRID_WIDTH);
    assert_eq!(se.board_index , start.board_index - GRID_WIDTH);
    assert_eq!(sw.board_index , start.board_index - GRID_WIDTH + 1);

    assert_eq!(e.bitboard_index, start.bitboard_index);
    assert_eq!(w.bitboard_index, start.bitboard_index);
    assert_eq!(ne.bitboard_index, start.bitboard_index);
    assert_eq!(nw.bitboard_index, start.bitboard_index);
    assert_eq!(se.bitboard_index, start.bitboard_index);
    assert_eq!(sw.bitboard_index, start.bitboard_index);

    for _ in 0..BITBOARD_WIDTH {
        e = e.shift_east();
        w = w.shift_west();
        nw = nw.shift_northwest();
        ne = ne.shift_northeast();
        sw = sw.shift_southwest();
        se = se.shift_southeast();
    }

    assert_eq!(e.board_index , start.board_index - 2);
    assert_eq!(w.board_index , start.board_index + 2);
    assert_eq!(ne.board_index , start.board_index + 2*GRID_WIDTH - 2);
    assert_eq!(nw.board_index , start.board_index + 2*GRID_WIDTH);
    assert_eq!(se.board_index , start.board_index - 2*GRID_WIDTH);
    assert_eq!(sw.board_index , start.board_index - 2*GRID_WIDTH + 2);

    assert_eq!(e.bitboard_index, start.bitboard_index);
    assert_eq!(w.bitboard_index, start.bitboard_index);
    assert_eq!(ne.bitboard_index, start.bitboard_index);
    assert_eq!(nw.bitboard_index, start.bitboard_index);
    assert_eq!(se.bitboard_index, start.bitboard_index);
    assert_eq!(sw.bitboard_index, start.bitboard_index);

}

#[test]
pub fn test_hex_grid_convert() {
    let hex_grid = HexGrid::from_dsl(concat!(
        " . a . . . .\n",
        ". . a . a .\n",
        " . a a a . .\n",
        ". . a . . .\n",
        " . . a a . .\n",
        ". . . . a .\n\n",
        "start - [0 0]\n\n",
    ));
    let mut bit_grid = BasicBitGrid::new();
    for (stack, loc) in hex_grid.pieces() {
        for piece in stack {
            bit_grid.add(piece, BitGridLocation::from_hex(loc));
        }
    }

    println!("{:#?}", bit_grid.pieces());
    println!("{:#?}", hex_grid.pieces().into_iter().map(|(p, l)| (p, BitGridLocation::from_hex(l))).collect::<Vec<_>>());
    assert_eq!(bit_grid, hex_grid, "These board's pieces should match");
}

#[test]
pub fn test_hex_grid_convert_with_stacks() {
    let hex_grid = HexGrid::from_dsl(concat!(
        " . a . . . .\n",
        ". . a . a .\n",
        " . a 3 a . .\n",
        ". . a . . .\n",
        " . . a 2 . .\n",
        ". . . . a .\n\n",
        "start - [0 0]\n\n",
        "3 - [a b M]\n",
        "2 - [a b]\n",
    ));
    let mut bit_grid = BasicBitGrid::new();
    for (stack, loc) in hex_grid.pieces() {
        for piece in stack {
            bit_grid.add(piece, BitGridLocation::from_hex(loc));
        }
    }

    println!("{:#?}", bit_grid.pieces());
    println!("{:#?}", hex_grid.pieces().into_iter().map(|(p, l)| (p, BitGridLocation::from_hex(l))).collect::<Vec<_>>());
    assert!(bit_grid == hex_grid, "These board's pieces should match");
}

#[test]
pub fn test_hex_grid_convert_with_max_stacks() {
    let hex_grid = HexGrid::from_dsl(concat!(
        " . a . . . .\n",
        ". . a . a .\n",
        " . a 7 a . .\n",
        ". . a . . .\n",
        " . . a a . .\n",
        ". . . . a .\n\n",
        "start - [0 0]\n\n",
        "7 - [a b M B M b B]\n",
    ));
    let mut bit_grid = BasicBitGrid::new();
    for (stack, loc) in hex_grid.pieces() {
        for piece in stack {
            bit_grid.add(piece, BitGridLocation::from_hex(loc));
        }
    }

    assert!(bit_grid == hex_grid, "These board's pieces should match");

    let hex_grid = HexGrid::from_dsl(concat!(
        " . a . . . .\n",
        ". . a . a .\n",
        " . a 2 a . .\n",
        ". . 3 . . .\n",
        " . . 3 a . .\n",
        ". . . . a .\n\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "3 - [a b M]\n",
        "3 - [a M B]\n",
    ));
    let mut bit_grid = BasicBitGrid::new();
    for (stack, loc) in hex_grid.pieces() {
        for piece in stack {
            bit_grid.add(piece, BitGridLocation::from_hex(loc));
        }
    }

    println!("{:#?}", bit_grid.pieces());
    println!("{:#?}", hex_grid.pieces().into_iter().map(|(p, l)| (p, BitGridLocation::from_hex(l))).collect::<Vec<_>>());
    assert!(bit_grid == hex_grid, "These board's pieces should match");
}

#[test]
pub fn test_remove() {
    let hex_grid = HexGrid::from_dsl(concat!(
        " . a . . . .\n",
        ". . a . a .\n",
        " . a 7 a . .\n",
        ". . a . . .\n",
        " . . a a . .\n",
        ". . . . a .\n\n",
        "start - [0 0]\n\n",
        "7 - [a b M B M b B]\n",
    ));
    let mut bit_grid = BasicBitGrid::new();
    for (stack, loc) in hex_grid.pieces() {
        for piece in stack {
            bit_grid.add(piece, BitGridLocation::from_hex(loc));
        }
    }

    assert!(bit_grid == hex_grid, "These board's pieces should match");

    for (stack, loc) in hex_grid.pieces() {
        for piece in stack {
            bit_grid.remove(piece, BitGridLocation::from_hex(loc));
        }
    }

    assert_eq!(bit_grid.pieces().len(), 0, "There should be no pieces left");
}

#[test]
pub fn test_convert() {
    let hex_grid = HexGrid::from_dsl(concat!(
        " . a . . . .\n",
        ". . a . a .\n",
        " . a 7 a . .\n",
        ". . a . . .\n",
        " . . a a . .\n",
        ". . . . a .\n\n",
        "start - [0 0]\n\n",
        "7 - [a b M B M b B]\n",
    ));

    let bit_grid : BasicBitGrid = hex_grid.clone().into();
    let result : HexGrid = bit_grid.try_into().unwrap();

    assert_eq!(result, hex_grid, "These board's pieces should match");
}

#[test]
pub fn test_center_localized() {
    let reference = HexLocation::center();
    let start : BitGridLocation = reference.into();

    assert!(is_localized::<BitGridLocation>(start, reference, MAX_WRAP_BEFORE_COLLISION - 1));
}

#[test]
pub fn test_few_locations_localized() {
    for row in -5..5 {
        for col in -5..5 {
            let reference = HexLocation::new(row, col);
            let start : BitGridLocation = reference.into();
            assert!(is_localized::<BitGridLocation>(start, reference, MAX_WRAP_BEFORE_COLLISION - 1));
        }
    }
}
#[ignore = "test is slow, be sure to run with --release -- --ignored"]
#[test]
pub fn test_many_locations_localized() {
    for row in -30..30 {
        for col in -30..30 {
            let reference = HexLocation::new(row, col);
            let start : BitGridLocation = reference.into();
            assert!(is_localized::<BitGridLocation>(start, reference, MAX_WRAP_BEFORE_COLLISION - 1));
        }
    }
}
