use super::*;
use crate::piece::{Piece, PieceType, PieceColor};
use std::fmt::{Display, self};
use crate::location::{Shiftable, FromHex, HexLocation};

const LEFT_OVERFLOW_MASK : u64 = 0x8080808080808080;
const RIGHT_OVERFLOW_MASK : u64 = 0x0101010101010101;
const BOTTOM_OVERFLOW_MASK : u64 = 0x00000000000000FF;
const TOP_OVERFLOW_MASK : u64 = 0xFF00000000000000;

const CENTER_BOARD_INDEX: usize = 0;
const CENTER_BIT_X: i8 = 4;
const CENTER_BIT_Y: i8 = 3;
const CENTER_BIT_INDEX: usize = CENTER_BIT_X as usize + CENTER_BIT_Y as usize * BITBOARD_WIDTH;
const GRID_SIZE: usize = GRID_WIDTH * GRID_HEIGHT;
const GRID_WIDTH: usize = 2;
const GRID_HEIGHT: usize = 2;

pub type MiniGrid = [AxialBitboard; 4]; 

/// Represents positions of Hive with Pillbug, Mosquito and Ladybug
/// that follow the One Hive rules (see Hive Rules for more information) 
/// and has no greater than 6 pieces with height > 1
///
/// Only beetles and mosquitos can be at height > 1.
///
/// See the documentation of the bit grid's AxialBitboard to
/// understand how the grid is represented at the bit level
///
/// Zooming out, the grid is represented instead as a cache friendly
/// 2x2 grid of AxialBitboards
///
/// The grid indices are laid out in the conventional x-y axis as follows:
///
/// TODO: conventional?? Isn't negative x going in the wrong direction? Perhaps 
/// compare it to HexGridLocation instead
/// ```
///     3 2
///     1 0
/// ```
///
/// The center is assigned to board index 0 at the bitboard index 28
///
/// Horizontal wrapping works in the following way:
///     0 wraps to 1 and vice versa
///     2 wraps to 3 and vice versa
///
/// Vertical wrapping works in the following way:
///     0 wraps to 2 and vice versa
///     1 wraps to 3 and vice versa
///
pub struct MiniBitGrid {
    pub queens: MiniGrid,
    pub beetles: MiniGrid,
    pub spiders: MiniGrid,
    pub grasshoppers: MiniGrid,
    pub ants: MiniGrid,
    pub pillbugs: MiniGrid,
    pub ladybugs: MiniGrid,
    pub mosquitos: MiniGrid,
    pub all_pieces: MiniGrid,
    pub outside: MiniGrid,
    pub white_pieces: MiniGrid,
    pub black_pieces: MiniGrid,
    pub stacks: BasicBitStack,
    pub metainfo: MiniMetaInfo,
}

pub struct MiniMetaInfo {
    /// Represents for each row, whether at least one piece is present
    pub row_presence: u16,
    /// Represents for each column, whether at least one piece is present
    pub column_presence: u16
}

impl MiniMetaInfo {
    pub fn new() -> Self {
        MiniMetaInfo {
            row_presence: 0,
            column_presence: 0,
        }
    }

    pub fn add_location(&mut self, location: MiniBitGridLocation) {
        self.row_presence |= location.row();
        self.column_presence |= location.column();
    }
}

/// Represents a location on the MiniBitGrid, 
///
/// must have a single bit set on mask representing the 
/// location as well as board_index between 0 <= board_index < 4
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct MiniBitGridLocation {
    pub board_index: usize,
    pub mask: u64,
}

impl MiniBitGrid {
    pub fn new() -> Self {
        MiniBitGrid {
            queens: [AxialBitboard::empty(); 4],
            beetles: [AxialBitboard::empty(); 4],
            spiders: [AxialBitboard::empty(); 4],
            grasshoppers: [AxialBitboard::empty(); 4],
            ants: [AxialBitboard::empty(); 4],
            pillbugs: [AxialBitboard::empty(); 4],
            ladybugs: [AxialBitboard::empty(); 4],
            mosquitos: [AxialBitboard::empty(); 4],
            all_pieces: [AxialBitboard::empty(); 4],
            outside: [AxialBitboard::empty(); 4],
            white_pieces: [AxialBitboard::empty(); 4],
            black_pieces: [AxialBitboard::empty(); 4],
            stacks: BasicBitStack::new(),
            metainfo: MiniMetaInfo::new(),  
        }
    }


    fn is_row_occupied(&self, bit_location : MiniBitGridLocation) -> bool {
        let (left, right) = match bit_location.board_index {
            0 | 1 => (self.all_pieces[0], self.all_pieces[1]),
            2 | 3 => (self.all_pieces[2], self.all_pieces[3]),
            _ => panic!("Invalid board index"),
        };

        let index = bit_location.mask.trailing_zeros() as usize;
        let row = index / BITBOARD_WIDTH;
        let row_mask = 0xFF << (row * BITBOARD_WIDTH);

        let bit_present = left & row_mask != 0 || right & row_mask != 0;
        bit_present
    }

    fn is_col_occupied(&self, bit_location : MiniBitGridLocation) -> bool {
        let (top, bottom) = match bit_location.board_index {
            0 | 2 => (self.all_pieces[0], self.all_pieces[2]),
            1 | 3 => (self.all_pieces[1], self.all_pieces[3]),
            _ => panic!("Invalid board index"),
        };

        let index = bit_location.mask.trailing_zeros() as usize;
        let col = index % BITBOARD_WIDTH;
        let col_mask = 0x0101010101010101 << col;

        let bit_present = top & col_mask != 0 || bottom & col_mask != 0;
        bit_present
    }

    pub fn add_piece(&mut self, piece: Piece, location: MiniBitGridLocation) {
        use PieceType::*;
        use PieceColor::*;

        let piece_bit = AxialBitboard::from_u64(location.mask);
        let board_index = location.board_index;
        let all_pieces = &mut self.all_pieces[board_index];
        let white_pieces = &mut self.white_pieces[board_index];
        let black_pieces = &mut self.black_pieces[board_index];

        match piece.piece_type {
            Queen => {
                self.queens[board_index] |= piece_bit;
            }
            Beetle => {
                self.beetles[board_index] |= piece_bit;
            }
            Spider => {
                self.spiders[board_index] |= piece_bit;
            }
            Grasshopper => {
                self.grasshoppers[board_index] |= piece_bit;
            }
            Ant => {
                self.ants[board_index] |= piece_bit;
            }
            Pillbug => {
                self.pillbugs[board_index] |= piece_bit;
            }
            Ladybug => {
                self.ladybugs[board_index] |= piece_bit;
            }
            Mosquito => {
                self.mosquitos[board_index] |= piece_bit;
            }
        }

        match piece.color {
            White => {
                *white_pieces |= piece_bit;
            }
            Black => {
                *black_pieces |= piece_bit;
            }
        }

        // TODO: update stacks
        // TODO: update outside
    
        // update metainfo
        self.metainfo.add_location(location);


    }

}


impl MiniBitGridLocation {
    pub fn from_index(board_index: usize, bitboard_index: usize) -> Self {
        debug_assert!(bitboard_index < 64);
        debug_assert!(board_index < 4);
        MiniBitGridLocation {
            board_index,
            mask: 1 << bitboard_index,
        }
    }

    pub fn from_u64(board_index: usize, mask: u64) -> Self {
        debug_assert!(board_index < 4);
        debug_assert!(mask.count_ones() == 1);
        MiniBitGridLocation {
            board_index,
            mask,
        }
    }

    /// Converts the set bit and board index to the *number* representing 
    /// its row in the grid. Returns 1 << number.
    pub fn row(&self) -> u16 {
        let row = self.mask.trailing_zeros() / BITBOARD_WIDTH as u32;
        let delta = if self.board_index % 2 == 0 { 0 } else { BITBOARD_WIDTH as u32};
        1 << (row + delta)
    }

    pub fn column(&self) -> u16 {
        let column = self.mask.trailing_zeros() % BITBOARD_WIDTH as u32;
        let delta = if self.board_index < 2 { 0 } else { BITBOARD_WIDTH as u32};
        1 << (column + delta)
    }
}

impl Shiftable for MiniBitGridLocation {
    fn shift_west(&self) -> MiniBitGridLocation {
        let overflow_found = (LEFT_OVERFLOW_MASK & self.mask) != 0;
        let new_mask = match overflow_found {
            true => self.mask >> BITBOARD_WIDTH - 1,
            false => self.mask << 1,
        };

        let new_board = match (overflow_found, self.board_index) {
            (true, 0) => 1,
            (true, 1) => 0,
            (true, 2) => 3,
            (true, 3) => 2,
            _ => self.board_index,
        };

        debug_assert!(new_mask.count_ones() == 1);

        MiniBitGridLocation {
            board_index: new_board,
            mask: new_mask,
        }

	}

    fn shift_east(&self) -> MiniBitGridLocation {
        let overflow_found = (RIGHT_OVERFLOW_MASK & self.mask) != 0;
        let new_mask = match overflow_found {
            true => self.mask << BITBOARD_WIDTH - 1,
            false => self.mask >> 1,
        };

        let new_board = match (overflow_found, self.board_index) {
            (true, 0) => 1,
            (true, 1) => 0,
            (true, 2) => 3,
            (true, 3) => 2,
            _ => self.board_index,
        };


        debug_assert!(new_mask.count_ones() == 1);

        MiniBitGridLocation {
            board_index: new_board,
            mask: new_mask,
        }

	}

    fn shift_northwest(&self) -> MiniBitGridLocation {
        let overflow_found = (TOP_OVERFLOW_MASK & self.mask) != 0;

        let new_mask = match overflow_found {
            true => self.mask >> (BITBOARD_WIDTH * (BITBOARD_HEIGHT - 1)),
            false => self.mask << BITBOARD_WIDTH,
        };

        let new_board = match (overflow_found, self.board_index) {
            (true, index) => (index + 2).rem_euclid(GRID_SIZE),
            _ => self.board_index,
        };

        debug_assert!(new_mask.count_ones() == 1);

        MiniBitGridLocation {
            board_index: new_board,
            mask: new_mask,
        }

	}

    fn shift_northeast(&self) -> MiniBitGridLocation {
        self.shift_east().shift_northwest()
	}

    fn shift_southwest(&self) -> MiniBitGridLocation {
        self.shift_west().shift_southeast()
	}

    fn shift_southeast(&self) -> MiniBitGridLocation {
        let overflow_found = (BOTTOM_OVERFLOW_MASK & self.mask) != 0;
        let new_mask = match overflow_found {
            true => self.mask << (BITBOARD_WIDTH * (BITBOARD_HEIGHT - 1)),
            false => self.mask >> BITBOARD_WIDTH,
        };

        let new_board = match (overflow_found, self.board_index) {
            (true, index) => (index + 2).rem_euclid(GRID_SIZE),
            _ => self.board_index,
        };

        debug_assert!(new_mask.count_ones() == 1);

        MiniBitGridLocation {
            board_index: new_board,
            mask: new_mask,
        }
	}

    fn center() -> MiniBitGridLocation {
        let mask = 1 << CENTER_BIT_INDEX;
        MiniBitGridLocation {
            board_index: CENTER_BOARD_INDEX,
            mask,
        }
    }
}

impl FromHex for MiniBitGridLocation {
    fn from_hex(hex: HexLocation) -> MiniBitGridLocation {
        let wrap = |x: i8, y: i8| -> usize {
            let board_x = (x + CENTER_BIT_X).div_euclid(BITBOARD_WIDTH as i8);
            let board_y = (y + CENTER_BIT_Y).div_euclid(BITBOARD_HEIGHT as i8);

            let board_x = board_x.rem_euclid(GRID_WIDTH as i8);
            let board_y = board_y.rem_euclid(GRID_HEIGHT as i8);

            let board_index = board_x + board_y * GRID_WIDTH as i8;

            board_index as usize
        };


        let bit_x = (-hex.x + CENTER_BIT_X).rem_euclid(BITBOARD_WIDTH as i8);
        let bit_y = (-hex.y + CENTER_BIT_Y).rem_euclid(BITBOARD_HEIGHT as i8);

        let bit_index = bit_x + bit_y * BITBOARD_WIDTH as i8;
        let bit_index = bit_index.rem_euclid(BITBOARD_SIZE as i8);
        let bit_index = bit_index as usize;
        let mask = 1 << bit_index as usize;

        let board_index = wrap(-hex.x, -hex.y);
        MiniBitGridLocation::from_u64(board_index, mask)
    }
}

impl From<HexLocation> for MiniBitGridLocation {
    fn from(hex: HexLocation) -> Self {
        MiniBitGridLocation::from_hex(hex)
    }
}

impl Display for MiniBitGridLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MiniBitGrid ({})\n{}", self.board_index, AxialBitboard::from_u64(self.mask))
    }
}

impl fmt::Debug for MiniBitGridLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MiniBitGrid ({})\n{}", self.board_index, AxialBitboard::from_u64(self.mask))
    }
}


#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::testing_utils::is_localized;

    #[test]
    pub fn size_sanity_check() {
        assert_eq!(std::mem::size_of::<MiniBitGrid>(), 416);
    }

    #[test]
    pub fn test_directions() {
        let start = MiniBitGridLocation::from_u64(0, 0x800000000);
        let e = MiniBitGridLocation::from_u64(0, 0x400000000);
        let w = MiniBitGridLocation::from_u64(0, 0x1000000000);
        let nw = MiniBitGridLocation::from_u64(0, 0x80000000000);
        let ne = MiniBitGridLocation::from_u64(0, 0x40000000000);
        let sw = MiniBitGridLocation::from_u64(0, 0x10000000);
        let se = MiniBitGridLocation::from_u64(0, 0x8000000);

        assert_eq!(start.shift_west(), w);

        assert_eq!(start.shift_east(), e);
        assert_eq!(start.shift_northwest(), nw);
        assert_eq!(start.shift_northeast(), ne);
        assert_eq!(start.shift_southwest(), sw);
        assert_eq!(start.shift_southeast(), se);
    }

    #[test]
    pub fn test_wrapping() {
        // Test corners and follow a hexagon to see
        // if it wraps back to original corner
        let tests = [ 
            MiniBitGridLocation::from_index(0, 63),
            MiniBitGridLocation::from_index(1, 56),
            MiniBitGridLocation::from_index(2, 7),
            MiniBitGridLocation::from_index(3, 0),
        ];

        for start in tests {
            let e = start.shift_east();
            let ne = e.shift_northeast();
            let nw = ne.shift_northwest();
            let w = nw.shift_west();
            let sw = w.shift_southwest();
            let end = sw.shift_southeast();

            assert_eq!(start, end);
        }
    }

    #[test]
    pub fn test_center_localized_15() {
        let reference = HexLocation::center();
        let start: MiniBitGridLocation = reference.into();

        assert!(is_localized::<MiniBitGridLocation>(
            start,
            reference,
            15
        ));
    }
}
