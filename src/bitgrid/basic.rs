use super::*; 
use crate::location::*;
use crate::piece::*;

pub const CENTER_BOARD_INDEX: usize = 24;
pub const CENTER_BITBOARD_INDEX : usize = 28;
pub const GRID_WIDTH: usize = 7;
pub const GRID_HEIGHT: usize = 7;
pub const GRID_SIZE: usize = GRID_WIDTH*GRID_HEIGHT;
pub const MAX_WRAP_BEFORE_COLLSION: usize = 32;
pub type GridLocation = usize;

/// Represents positions of Hive with Pillbug Mosquito and Ladybug 
/// that follow the One Hive rule and has no stack greater than 7. 
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

        let board = self.get_mut_piece(piece).get_mut(loc.board_index).unwrap();
        *board |= 1 << loc.bitboard_index;

        let color = self.get_mut_color(color).get_mut(loc.board_index).unwrap();
        *color |= 1 << loc.bitboard_index;

        let all_pieces  = self.get_mut_all_pieces().get_mut(loc.board_index).unwrap();
        *all_pieces |= 1 << loc.bitboard_index;
        //TODO: stacks
    }

    pub fn remove(&mut self, piece : Piece, loc : BitGridLocation) {
        debug_assert!(loc.board_index < GRID_SIZE);
        let color = piece.color;
        let piece = piece.piece_type;

        let board = self.get_mut_piece(piece).get_mut(loc.board_index).unwrap();
        *board &= !(1 << loc.bitboard_index);

        let color = self.get_mut_color(color).get_mut(loc.board_index).unwrap();
        *color &= !(1 << loc.bitboard_index);

        let all_pieces  = self.get_mut_all_pieces().get_mut(loc.board_index).unwrap();
        *all_pieces &= !(1 << loc.bitboard_index);
        //TODO: stacks
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

    
    pub fn piece_present(&self, loc : BitGridLocation) -> bool {
        debug_assert!(loc.board_index < GRID_SIZE);
        self.all_pieces[loc.board_index].peek(loc.bitboard_index)
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

// TODO: 
// add piece
// remove piece


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BitGridLocation {
    board_index : usize,
    bitboard_index : usize 
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
        let grid_length = GRID_SIZE as i8;

        let mut dx = if x < 0 { -1 } else { 0 };
        dx = if x >= width { 1 } else { dx };

        let mut dy = if y < 0 { -(GRID_WIDTH as i8) } else { 0 };
        dy = if y >= height { GRID_WIDTH as i8} else { dy };

        let new_board_index = board + dx + dy;
        let new_board_index = new_board_index.rem_euclid(grid_length);

        let new_bitboard_index = y.rem_euclid(height) * height + x.rem_euclid(width);
        println!("x : {}, y : {}, new_bitboard_index : {}", x, y, new_bitboard_index);


        BitGridLocation::new(new_board_index as usize, new_bitboard_index as usize)
    }
}



impl FromHex for BitGridLocation {
    fn from_hex(hex: HexLocation) -> Self {
        let center_x = (CENTER_BITBOARD_INDEX % BITBOARD_WIDTH) as i8; 
        let center_y = (CENTER_BITBOARD_INDEX / BITBOARD_HEIGHT) as i8;
        let board_center_x = (CENTER_BOARD_INDEX % GRID_WIDTH) as i8;
        let board_center_y = (CENTER_BOARD_INDEX / GRID_HEIGHT) as i8;

        let bit_x = ( center_x - hex.x + BITBOARD_WIDTH as i8) % BITBOARD_WIDTH as i8;
        let bit_y = ( hex.y + center_y + BITBOARD_HEIGHT as i8 ) % BITBOARD_HEIGHT as i8;

        let board_x = -hex.x + (board_center_x * BITBOARD_WIDTH as i8) + center_x;
        let board_x = board_x / BITBOARD_WIDTH as i8;

        let board_y = hex.y + (board_center_y * BITBOARD_HEIGHT as i8) + center_y;
        let board_y = board_y / BITBOARD_HEIGHT as i8;

        let board_index = (board_y * GRID_HEIGHT as i8 + board_x) as usize;
        let bitboard_index = (bit_y * BITBOARD_HEIGHT as i8 + bit_x) as usize;

        BitGridLocation::new(board_index, bitboard_index)
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
    
}
