use super::*;
use std::fmt;
use std::fmt::Display;
use crate::location::Shiftable;

const LEFT_OVERFLOW_MASK : u64 = 0x8080808080808080;
const RIGHT_OVERFLOW_MASK : u64 = 0x0101010101010101;
const BOTTOM_OVERFLOW_MASK : u64 = 0x00000000000000FF;
const TOP_OVERFLOW_MASK : u64 = 0xFF00000000000000;

const CENTER_BOARD_INDEX: usize = 0;
const CENTER_BIT_INDEX: usize = 28;
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
/// ```
///     3 2
///     1 0
/// ```
///
/// The center is assigned to board index 0 at the bitboard index 28
///
/// Horizontal wrapping works in the following way:
///     0 - 3 - 2 - 1 - 0 ...
///
/// Vertical wrapping works in the following way:
///     3 - 1 - 2 - 0 - 3 ...
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
    pub white_pieces: MiniGrid,
    pub black_pieces: MiniGrid,
    pub stacks: BasicBitStack,
}


/// Represents a location on the MiniBitGrid, 
///
/// must have a single bit set on mask representing the 
/// location as well as board_index between 0 <= board_index < 4
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MiniBitGridLocation {
    pub board_index: usize,
    pub mask: u64,
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

}

impl Shiftable for MiniBitGridLocation {
    fn shift_west(&self) -> MiniBitGridLocation {
        let overflow_found = (LEFT_OVERFLOW_MASK & self.mask) != 0;
        let new_mask = match overflow_found {
            true => self.mask >> BITBOARD_WIDTH - 1,
            false => self.mask << 1,
        };

        let new_board = match overflow_found {
            true => (self.board_index + 1).rem_euclid(GRID_SIZE),
            false => self.board_index,
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

        let new_board = match overflow_found {
            true => (self.board_index - 1).rem_euclid(GRID_SIZE),
            false => self.board_index,
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
            (true, 2) => 1,
            (true, 3) => 0,
            (true , index) => index + 2,
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
            (true, 0) => 3,
            (true, 1) => 2,
            (true , index) => index - 2,
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

impl Display for MiniBitGridLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MiniBitGrid ({})\n{}", self.board_index, AxialBitboard::from_u64(self.mask))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    pub fn size_sanity_check() {
        assert_eq!(std::mem::size_of::<MiniBitGrid>(), 384);
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
}
