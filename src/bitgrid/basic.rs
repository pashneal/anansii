use super::*; 
use crate::location::*;
use crate::piece::*;
pub const GRID_LENGTH: usize = 16;
pub const GRID_WIDTH: usize = 4;
pub const GRID_HEIGHT: usize = 4;
pub const GRID_SENTINEL: GridLocation = 0b10000;
pub const MAX_WRAP_BEFORE_COLLSION: usize = GRID_WIDTH*8;
pub type GridLocation = usize;

/// Represents only legal states of Hive with Pillbug Mosquito and Ladybug grids 
/// correctly. TODO: may have to refactor tests to account for this.
///
/// See the documentation of the bit grid's AxialBitboard to
/// understand how the grid is represented at the bit level
///
/// Zooming out, the grid is represented instead as
/// a 4x4 grid of AxialBitboards
///
/// The grid is represented as follows:
/// ```
///   15 14 13 12
///   11 10 09 08
///   07 06 05 04
///   03 02 01 00
/// ```
///
/// The center is assigned to board 10 at the 28th bit
pub type Grid = [AxialBitboard; GRID_LENGTH];
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
            queens: [AxialBitboard::from_u64(0); GRID_LENGTH],
            beetles: [AxialBitboard::from_u64(0); GRID_LENGTH],
            spiders: [AxialBitboard::from_u64(0); GRID_LENGTH],
            grasshoppers: [AxialBitboard::from_u64(0); GRID_LENGTH],
            ants: [AxialBitboard::from_u64(0); GRID_LENGTH],
            pillbugs: [AxialBitboard::from_u64(0); GRID_LENGTH],
            ladybugs: [AxialBitboard::from_u64(0); GRID_LENGTH],
            mosquitos: [AxialBitboard::from_u64(0); GRID_LENGTH],
            all_pieces: [AxialBitboard::from_u64(0); GRID_LENGTH],
            white_pieces: [AxialBitboard::from_u64(0); GRID_LENGTH],
            black_pieces: [AxialBitboard::from_u64(0); GRID_LENGTH],
            stacks: BasicBitStack::new()
        }
    }

    pub fn add(&mut self, piece : Piece, loc : BitGridLocation) {
        debug_assert!(loc.board_index < GRID_LENGTH);
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
        debug_assert!(loc.board_index < GRID_LENGTH);
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
        debug_assert!(loc.board_index < GRID_LENGTH);
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
// direction (without overflow)
// direction (with overflow)


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

    pub fn apply(&self, direction : Direction) ->  BitGridLocation{
        use Direction::*;
        let x = self.bitboard_index % BITBOARD_WIDTH;
        let y = self.bitboard_index / BITBOARD_HEIGHT;
        let board = self.board_index as i8;

        let (x, y) = (x as i8, y as i8);
        let (x, y) = match direction {
            E => (x - 1, y),
            W => (x + 1, y),
            NW => (x + 1, y - 1),
            NE => (x - 1, y - 1),
            SE => (x - 1, y + 1),
            SW => (x + 1, y + 1),
        };

        self.wrap(board, x, y)
    }

    fn wrap(&self, board : i8, x : i8, y : i8) -> BitGridLocation {
        let width = BITBOARD_WIDTH as i8;
        let height = BITBOARD_HEIGHT as i8;
        let grid_length = GRID_LENGTH as i8;

        let mut dx = if x < 0 { -1 } else { 0 };
        dx = if x >= width { 1 } else { dx };

        let mut dy = if y < 0 { -(GRID_HEIGHT as i8) } else { 0 };
        dy = if y >= height { GRID_HEIGHT as i8} else { dy };

        let new_board_index = board + dx + dy;
        let new_board_index = new_board_index.rem_euclid(grid_length);

        let new_x = x.rem_euclid(width);
        let new_y = y.rem_euclid(height);
        let new_bitboard_index = new_y * height + new_x;

        BitGridLocation::new(new_board_index as usize, new_bitboard_index as usize)
    }
}

pub const CENTER_BOARD_INDEX: usize = 10;
pub const CENTER_BITBOARD_INDEX : usize = 28;


impl Location for BitGridLocation {
    fn to_hex(&self) -> HexLocation {
        let center_x = (CENTER_BITBOARD_INDEX % BITBOARD_WIDTH) as i8;
        let center_y = (CENTER_BITBOARD_INDEX / BITBOARD_HEIGHT) as i8;

        let board_center_x = (CENTER_BOARD_INDEX % GRID_WIDTH) as i8;
        let board_center_y = (CENTER_BOARD_INDEX / GRID_HEIGHT) as i8;
        
        let board_x = (self.board_index % GRID_WIDTH) as i8;
        let board_y = (self.board_index / GRID_HEIGHT) as i8;

        // Adjust for the grid's position away from the center
        let mut x = (board_center_x - board_x) * BITBOARD_WIDTH as i8;
        let mut y = (board_y - board_center_y) * BITBOARD_HEIGHT as i8;

        let bit_x = (self.bitboard_index % BITBOARD_WIDTH) as i8;
        let bit_y = (self.bitboard_index / BITBOARD_HEIGHT) as i8;

        // Then adjust for the bit's position away from the center
        x += center_x - bit_x;
        y += bit_y - center_y;

        HexLocation::new(x, y)
    }

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

#[test]
pub fn test_bitgrid_location() {
    let location = BitGridLocation::new(CENTER_BOARD_INDEX, CENTER_BITBOARD_INDEX);
    let hex = location.to_hex();
    assert_eq!(hex, HexLocation::new(0, 0));
}

#[test]
pub fn test_bitgrid_location_from_hex() {
    let hex = HexLocation::new(0, 0);
    let location = BitGridLocation::from_hex(hex);
    let expected = BitGridLocation::new(CENTER_BOARD_INDEX, CENTER_BITBOARD_INDEX);
    assert_eq!(location, expected);
}

#[test]
pub fn test_bitgrid_location_many_quadrants() {
    // Test the various (x,y) quadrant conversion back and forth
    //          |
    // (-,+)    |    (+, +)
    //-------------------
    // (-,-)    |    (+, -)
    //          |
    let hex = HexLocation::new(1, 1);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);

    let hex = HexLocation::new(-1, 1);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);

    let hex = HexLocation::new(-1, -1);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);

    let hex = HexLocation::new(1, -1);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);
}

#[test]
pub fn test_bitgrid_at_least_3_boards_high() {
    let width = BITBOARD_WIDTH as i8;
    let height = BITBOARD_HEIGHT as i8;

    let hex = HexLocation::new(width, height);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);

    let hex = HexLocation::new(-width, height);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);

    let hex = HexLocation::new(-width, -height);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);

    let hex = HexLocation::new(width, -height);
    let result = BitGridLocation::from_hex(hex).to_hex();
    assert_eq!(result, hex);
}

#[test]
pub fn test_bitgrid_wrapping() {
    let mut location = HexLocation::new(0, 0);
    let mut bit_location = BitGridLocation::from_hex(location);
    let mut bit_grid = BasicBitGrid::new();
    for i in 0..MAX_WRAP_BEFORE_COLLSION {

        println!("-------------------");
        println!("i: {}", i);
        println!("location: {:?}", location);
        println!("bit_location: {:?}", bit_location);
        println!("converted back: {:?}", bit_location.to_hex());
        println!("and back again: {:?}", BitGridLocation::from_hex(location));
        bit_grid.add(Piece::new(PieceType::Queen, PieceColor::White), bit_location);
        assert!(bit_grid.piece_present(bit_location));
        assert!(bit_grid.piece_present(BitGridLocation::from_hex(location)));
        bit_grid.remove(Piece::new(PieceType::Queen, PieceColor::White), bit_location);
        assert!(!bit_grid.piece_present(BitGridLocation::from_hex(location)));

        location = location.apply(Direction::E);
        bit_location = bit_location.apply(Direction::E);
    }
}

#[test]
pub fn boundary() {
    let width = BITBOARD_WIDTH as i8;
    let height = BITBOARD_HEIGHT as i8;
    let center_y = CENTER_BITBOARD_INDEX / BITBOARD_HEIGHT;
    let center_y = center_y as i8;
    let center_x = CENTER_BITBOARD_INDEX % BITBOARD_WIDTH;
    let center_x = center_x as i8;

    let location = HexLocation::new(width + center_x + 1, 0);
    assert_eq!(location, BitGridLocation::from_hex(location).to_hex());

    let location = HexLocation::new(width + center_x + 0, 0);
    assert_eq!(location, BitGridLocation::from_hex(location).to_hex());

    let location = HexLocation::new(width + center_x + -1, 0);
    assert_eq!(location, BitGridLocation::from_hex(location).to_hex());

    let location = HexLocation::new(0, height + center_y + 1);
    assert_eq!(location, BitGridLocation::from_hex(location).to_hex());

    let location = HexLocation::new(0, height + center_y + 0);
    assert_eq!(location, BitGridLocation::from_hex(location).to_hex());

    let location = HexLocation::new(0, height + center_y + -1);
    assert_eq!(location, BitGridLocation::from_hex(location).to_hex());
}
