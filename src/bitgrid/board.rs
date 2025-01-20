use std::fmt::{Display, Formatter};
use std::ops;

const NORTHWEST_OVERFLOW_MASK: AxialBitboard = AxialBitboard(0xff00000000000000);
const SOUTHEAST_OVERFLOW_MASK: AxialBitboard = AxialBitboard(0x00000000000000ff);
const WEST_OVERFLOW_MASK: AxialBitboard = AxialBitboard(0x8080808080808080);
const EAST_OVERFLOW_MASK: AxialBitboard = AxialBitboard(0x0101010101010101);
const NORTHEAST_OVERFLOW_MASK: AxialBitboard = AxialBitboard(0xff01010101010101);
const SOUTHWEST_OVERFLOW_MASK: AxialBitboard = AxialBitboard(0x80808080808080ff);
const NORTHEAST_CORNER: AxialBitboard = AxialBitboard(0x100000000000000);
const SOUTHWEST_CORNER: AxialBitboard = AxialBitboard(0x80);
const NORTH_WITHOUT_CORNER: AxialBitboard = AxialBitboard(0xfe00000000000000);
const SOUTH_WITHOUT_CORNER: AxialBitboard = AxialBitboard(0x000000000000007f);
const NEIGHBORHOOD_HEIGHT : i8 = 3;
const NEIGHBORHOOD_WIDTH : i8 = 3;
const NEIGHBORHOOD_CENTER_INDEX : i8 = 4;

/// Represents a internal part of the Hive grid
///
/// Compositions of AxialBitboards are used to represent the full
/// hive grid and game state
///
/// Locations are represented by axial coordinates mapping to bit
/// indices as follows:
///
/// Assumes that the bitboard represents its individual bits as
/// follows (00 is the least significant bit, 63 is the most)
///
/// ```
///     63 62 61 60 59 58 57 56
///     55 54 53 52 51 50 49 48
///     47 46 45 44 43 42 41 40
///     39 38 37 36 35 34 33 32
///     31 30 29 28 27 26 25 24
///     23 22 21 20 19 18 17 16
///     15 14 13 12 11 10 09 08
///     07 06 05 04 03 02 01 00
/// ```
///
/// Then directions are represented as follows:
/// (where some location is represented by X)
///  
/// ```
///     .  .  .  .  .  .  .  .
///     .  .  .  .  .  .  .  .
///     .  .  .  NW NE .  .  .
///     .  .  W  X  E  .  .  .
///     .  .  SW SE .  .  .  .
///     .  .  .  .  .  .  .  .
///     .  .  .  .  .  .  .  .
///     .  .  .  .  .  .  .  .
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AxialBitboard(u64);

pub const BITBOARD_HEIGHT: usize = 8;
pub const BITBOARD_WIDTH: usize = 8;
pub const BITBOARD_SIZE: usize = BITBOARD_HEIGHT * BITBOARD_WIDTH;

#[derive(Debug, Copy, Clone)]
pub struct BitboardCoords {
    pub x: usize,
    pub y: usize,
}

impl BitboardCoords {
    pub fn index(&self) -> usize {
        self.y * BITBOARD_WIDTH + self.x
    }
}

#[derive(Debug, Copy, Clone)]
pub struct BitboardBounds {
    pub top_left: BitboardCoords,
    pub bottom_right: BitboardCoords,
}

/// Represents a collection of boards around a central board
/// in the follow structure
///
/// top_left      top        top_right
/// center_left  center   center_right
/// bottom_left  bottom   bottom_right
///
/// with indices as follows:
///
/// 8 7 6
/// 5 4 3
/// 2 1 0
///
#[derive(Clone, Debug)]
pub struct Neighborhood {
    boards: [AxialBitboard; 9],
}

impl Neighborhood {
    pub fn new() -> Self {
        Neighborhood {
            boards: [AxialBitboard::empty(); 9],
        }
    }

    pub fn bottom(&mut self) -> &mut AxialBitboard {
        &mut self.boards[1]
    }

    pub fn top(&mut self) -> &mut AxialBitboard {
        &mut self.boards[7]
    }

    pub fn center_right(&mut self) -> &mut AxialBitboard {
        &mut self.boards[3]
    }

    pub fn center_left(&mut self) -> &mut AxialBitboard {
        &mut self.boards[5]
    }

    pub fn center(&mut self) -> &mut AxialBitboard {
        &mut self.boards[4]
    }

    pub fn top_left(&mut self) -> &mut AxialBitboard {
        &mut self.boards[8]
    }

    pub fn top_right(&mut self) -> &mut AxialBitboard {
        &mut self.boards[6]
    }

    pub fn bottom_left(&mut self) -> &mut AxialBitboard {
        &mut self.boards[2]
    }

    pub fn bottom_right(&mut self) -> &mut AxialBitboard {
        &mut self.boards[0]
    }

    fn combine(&mut self, other: &Neighborhood, other_center_index: usize) {

        let x = NEIGHBORHOOD_CENTER_INDEX % NEIGHBORHOOD_HEIGHT;
        let y = NEIGHBORHOOD_CENTER_INDEX / NEIGHBORHOOD_WIDTH;
        let dx = x - (other_center_index as i8 % NEIGHBORHOOD_HEIGHT);
        let dy = y - (other_center_index as i8 / NEIGHBORHOOD_WIDTH);

        // adjust from other neighborhood's index to this neighborhood's indices
        let translated_index = move |index: usize| -> usize {
            let x = index as i8 % NEIGHBORHOOD_WIDTH;
            let y = index as i8 / NEIGHBORHOOD_HEIGHT;
            let new_x = (x + dx).rem_euclid(NEIGHBORHOOD_WIDTH);
            let new_y = (y + dy).rem_euclid(NEIGHBORHOOD_HEIGHT);
            (new_y * NEIGHBORHOOD_HEIGHT + new_x) as usize
        };

        for (i, board) in self.boards.iter_mut().enumerate() { 
            let index = translated_index(i);
            *board |= other.boards[index];
        }
    }

    //TODO: candidate for optimization, current implementation's correctness
    //is easy to reason about, but at a major performance cost 
    pub fn neighborhood(&self) -> Self {
        let mut reference = self.clone();
        let mut result = reference.center().neighborhood();

        println!("First neighborhood:\n{}", result);
        for (index, board) in reference.boards.iter().enumerate() {
            result.combine(&board.neighborhood(), index);
            println!("Result after combining with board at index {}:\n{}", index, result);
        }
        result
    }
}


impl Display for Neighborhood {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for start_board in (0..9).step_by(3).rev() {
            let mut rows = vec![String::new(); BITBOARD_HEIGHT];
            for offset in (0..3).rev() {
                let board = self.boards[start_board + offset];
                for i in (0..BITBOARD_HEIGHT).rev() {
                    for j in (0..BITBOARD_WIDTH).rev() {
                        let index = i * BITBOARD_WIDTH + j;
                        if board.peek(index) {
                            rows[BITBOARD_HEIGHT - i - 1].push_str("■ ");
                        } else {
                            rows[BITBOARD_HEIGHT - i - 1].push_str("□ ");
                        }
                    }
                    rows[BITBOARD_HEIGHT - i - 1].push_str(" ");
                }
            }

            for row in rows {
                writeln!(f, "{}", row)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl AxialBitboard {
    #[inline(always)]
    pub fn empty() -> Self {
        AxialBitboard(0)
    }

    #[inline(always)]
    pub fn from_u64(board: u64) -> Self {
        AxialBitboard(board)
    }

    #[inline(always)]
    fn flip_west(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> BITBOARD_WIDTH - 1)
    }

    #[inline(always)]
    pub fn shift_west(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 1)
    }

    #[inline(always)]
    fn flip_east(&self) -> AxialBitboard {
        AxialBitboard(self.0 << BITBOARD_WIDTH - 1)
    }

    #[inline(always)]
    pub fn shift_east(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> 1)
    }

    #[inline(always)]
    fn flip_northwest(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> (BITBOARD_HEIGHT - 1) * BITBOARD_WIDTH)
    }

    #[inline(always)]
    pub fn shift_northwest(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 8)
    }

    #[inline(always)]
    fn flip_northeast(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> (BITBOARD_HEIGHT - 1) * (BITBOARD_WIDTH - 1))
    }

    #[inline(always)]
    pub fn shift_northeast(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 7)
    }

    #[inline(always)]
    fn flip_southwest(&self) -> AxialBitboard {
        AxialBitboard(self.0 << (BITBOARD_HEIGHT - 1) * (BITBOARD_WIDTH - 1))
    }

    #[inline(always)]
    pub fn shift_southwest(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> 7)
    }

    #[inline(always)]
    fn flip_southeast(&self) -> AxialBitboard {
        AxialBitboard(self.0 << (BITBOARD_HEIGHT - 1) * BITBOARD_WIDTH)
    }

    #[inline(always)]
    pub fn shift_southeast(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> 8)
    }

    #[inline(always)]
    pub fn peek(&self, index: usize) -> bool {
        (self.0 & (1 << index)) != 0
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    /// Returns the smallest bounding box of the bitboard, containing all
    /// set bits in the bitboard if there are any
    pub fn bounding_box(&self) -> Option<BitboardBounds> {
        if self.is_empty() {
            return None;
        }

        let mut max_x = 0;
        let mut max_y = 0;
        let mut min_x = BITBOARD_WIDTH;
        let mut min_y = BITBOARD_HEIGHT;

        for y in 0..BITBOARD_HEIGHT {
            for x in 0..BITBOARD_WIDTH {
                let index = y * BITBOARD_WIDTH + x;
                if self.peek(index) {
                    min_x = min_x.min(x);
                    min_y = min_y.min(y);
                    max_x = max_x.max(x);
                    max_y = max_y.max(y);
                }
            }
        }

        Some(BitboardBounds {
            top_left: BitboardCoords { x: max_x, y: max_y },
            bottom_right: BitboardCoords { x: min_x, y: min_y },
        })
    }

    /// Returns a neighborhood representing all empty adjacent spaces
    /// next to current existing set bits. These adjacent neighbors are
    /// specifically unset bits arrived at by shifting the current bitboard
    /// in all hive directions (W, NW, NE, SE, SW, E).
    pub fn neighborhood(&self) -> Neighborhood {
        let mut neighbors = Neighborhood::new();
        let original = *self;

        *neighbors.center() |= (original & !WEST_OVERFLOW_MASK).shift_west();
        *neighbors.center() |= (original & !EAST_OVERFLOW_MASK).shift_east();
        *neighbors.center() |= (original & !NORTHWEST_OVERFLOW_MASK).shift_northwest();
        *neighbors.center() |= (original & !NORTHEAST_OVERFLOW_MASK).shift_northeast();
        *neighbors.center() |= (original & !SOUTHWEST_OVERFLOW_MASK).shift_southwest();
        *neighbors.center() |= (original & !SOUTHEAST_OVERFLOW_MASK).shift_southeast();
        *neighbors.center() &= !original;

        *neighbors.center_right() |= (original & EAST_OVERFLOW_MASK).flip_east();
        *neighbors.center_right() |= (original & EAST_OVERFLOW_MASK)
            .shift_northwest()
            .flip_east();

        *neighbors.center_left() |= (original & WEST_OVERFLOW_MASK).flip_west();
        *neighbors.center_left() |= (original & WEST_OVERFLOW_MASK)
            .shift_southeast()
            .flip_west();

        *neighbors.top() |= (original & NORTHWEST_OVERFLOW_MASK).flip_northwest();
        *neighbors.top() |= (original & NORTH_WITHOUT_CORNER)
            .shift_east()
            .flip_northwest();

        *neighbors.bottom() |= (original & SOUTHEAST_OVERFLOW_MASK).flip_southeast();
        *neighbors.bottom() |= (original & SOUTH_WITHOUT_CORNER)
            .shift_west()
            .flip_southeast();

        *neighbors.top_right() |= (original & NORTHEAST_CORNER).flip_northeast();
        *neighbors.bottom_left() |= (original & SOUTHWEST_CORNER).flip_southwest();

        neighbors
    }
}

pub struct AxialBitboardIter {
    board: AxialBitboard,
}

impl Iterator for AxialBitboardIter {
    type Item = BitboardCoords;

    fn next(&mut self) -> Option<Self::Item> {
        if self.board.is_empty() {
            return None;
        }

        let lsb = self.board.0.trailing_zeros() as usize;
        let x = lsb % BITBOARD_WIDTH;
        let y = lsb / BITBOARD_WIDTH;

        self.board.0 &= self.board.0 - 1;

        Some(BitboardCoords { x, y })
    }
}

impl IntoIterator for AxialBitboard {
    type Item = BitboardCoords;
    type IntoIter = AxialBitboardIter;

    fn into_iter(self) -> Self::IntoIter {
        AxialBitboardIter { board: self }
    }
}

impl ops::Add<Self> for AxialBitboard {
    type Output = Self;

    #[inline(always)]
    fn add(self, rhs: Self) -> Self {
        AxialBitboard(self.0 + rhs.0)
    }
}

impl ops::Add<u64> for AxialBitboard {
    type Output = Self;

    #[inline(always)]
    fn add(self, rhs: u64) -> Self {
        AxialBitboard(self.0 + rhs)
    }
}

impl ops::BitOr<Self> for AxialBitboard {
    type Output = Self;

    #[inline(always)]
    fn bitor(self, rhs: Self) -> Self {
        AxialBitboard(self.0 | rhs.0)
    }
}

impl ops::BitOr<u64> for AxialBitboard {
    type Output = Self;

    #[inline(always)]
    fn bitor(self, rhs: u64) -> Self {
        AxialBitboard(self.0 | rhs)
    }
}

impl ops::BitOrAssign<Self> for AxialBitboard {
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl ops::BitOrAssign<u64> for AxialBitboard {
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: u64) {
        self.0 |= rhs;
    }
}

impl ops::BitAnd<Self> for AxialBitboard {
    type Output = Self;

    #[inline(always)]
    fn bitand(self, rhs: Self) -> Self {
        AxialBitboard(self.0 & rhs.0)
    }
}

impl ops::BitAnd<u64> for AxialBitboard {
    type Output = Self;

    #[inline(always)]
    fn bitand(self, rhs: u64) -> Self {
        AxialBitboard(self.0 & rhs)
    }
}

impl ops::BitAndAssign<Self> for AxialBitboard {
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl ops::BitAndAssign<u64> for AxialBitboard {
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: u64) {
        self.0 &= rhs;
    }
}

impl ops::Not for AxialBitboard {
    type Output = Self;

    #[inline(always)]
    fn not(self) -> Self {
        AxialBitboard(!self.0)
    }
}

impl Display for AxialBitboard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for i in (0..BITBOARD_HEIGHT).rev() {
            for j in (0..BITBOARD_WIDTH).rev() {
                let index = i * BITBOARD_WIDTH + j;
                if self.peek(index) {
                    write!(f, "■ ")?;
                } else {
                    write!(f, "□ ")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl PartialEq<u64> for AxialBitboard {
    #[inline(always)]
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn size_is_small() {
        // Sanity checking rust sizes
        assert_eq!(std::mem::size_of::<AxialBitboard>(), 8);
    }

    #[test]
    pub fn test_directions() {
        let start = AxialBitboard::from_u64(0x800000000);
        let e = AxialBitboard::from_u64(0x400000000);
        let w = AxialBitboard::from_u64(0x1000000000);
        let nw = AxialBitboard::from_u64(0x80000000000);
        let ne = AxialBitboard::from_u64(0x40000000000);
        let sw = AxialBitboard::from_u64(0x10000000);
        let se = AxialBitboard::from_u64(0x8000000);

        assert_eq!(start.shift_west(), w);
        assert_eq!(start.shift_east(), e);
        assert_eq!(start.shift_northwest(), nw);
        assert_eq!(start.shift_northeast(), ne);
        assert_eq!(start.shift_southwest(), sw);
        assert_eq!(start.shift_southeast(), se);
    }

    #[test]
    pub fn test_direction_cohesion() {
        let start = AxialBitboard::from_u64(0x800000000);
        let everywhere = start
            .shift_west()
            .shift_east()
            .shift_northwest()
            .shift_northeast()
            .shift_southwest()
            .shift_southeast();
        assert_eq!(start, everywhere);
    }

    #[test]
    pub fn test_display() {
        let output = concat!(
            "□ □ □ □ □ □ □ □ \n",
            "□ □ □ □ □ □ □ □ \n",
            "□ □ □ □ □ □ □ □ \n",
            "□ □ □ □ □ □ □ □ \n",
            "□ □ □ □ □ □ □ □ \n",
            "□ □ □ □ □ □ □ □ \n",
            "□ □ □ □ □ □ □ □ \n",
            "■ □ □ □ □ □ □ □ \n",
        );

        let start = AxialBitboard::from_u64(0x80);
        assert_eq!(format!("{}", start), output);
    }

    #[test]
    pub fn test_bounding_box() {
        let board = AxialBitboard::from_u64(0x705018121e0400);
        let bounds = board.bounding_box().unwrap();
        assert_eq!(bounds.top_left.x, 6);
        assert_eq!(bounds.top_left.y, 6);
        assert_eq!(bounds.bottom_right.x, 1);
        assert_eq!(bounds.bottom_right.y, 1);

        let board = AxialBitboard::from_u64(0x200000080000);
        let bounds = board.bounding_box().unwrap();
        assert_eq!(bounds.top_left.x, 5);
        assert_eq!(bounds.top_left.y, 5);
        assert_eq!(bounds.bottom_right.x, 3);
        assert_eq!(bounds.bottom_right.y, 2);
    }

    #[test]
    pub fn test_bounding_box_empty() {
        let board = AxialBitboard::from_u64(0);
        assert!(board.is_empty());
        assert!(board.bounding_box().is_none());
    }

    #[test]
    pub fn test_neighborhood_center_board() {
        let start = AxialBitboard::from_u64(0x725e242000);
        let expected = AxialBitboard::from_u64(0x7b8da1da5c60);
        let mut result = start.neighborhood();

        assert_eq!(*result.center(), expected);
        assert_eq!(*result.top(), AxialBitboard::empty());
        assert_eq!(*result.bottom(), AxialBitboard::empty());
        assert_eq!(*result.center_right(), AxialBitboard::empty());
        assert_eq!(*result.center_left(), AxialBitboard::empty());
        assert_eq!(*result.top_right(), AxialBitboard::empty());
        assert_eq!(*result.top_left(), AxialBitboard::empty());
        assert_eq!(*result.bottom_right(), AxialBitboard::empty());
        assert_eq!(*result.bottom_left(), AxialBitboard::empty());
    }

    #[test]
    pub fn test_neighborhood_all_boards() {
        let start = AxialBitboard(0x9100008001000091);
        let center = AxialBitboard(0x6ab3c0418203d96a);
        let center_right = AxialBitboard(0x8000008080008080);
        let center_left = AxialBitboard(0x101000101000001);
        let top = AxialBitboard(0xd9);
        let top_right = AxialBitboard(0x80);
        let bottom_left = AxialBitboard(0x100000000000000);
        let bottom = AxialBitboard(0xb300000000000000);

        let bottom_right = AxialBitboard::empty();
        let top_left = AxialBitboard::empty();

        let mut result = start.neighborhood();
        println!("{}", result);

        println!("Center:");
        println!("{}", center);
        println!("{}", *result.center());
        assert_eq!(*result.center(), center);

        println!("Center Right:");
        println!("{}", center_right);
        println!("{}", *result.center_right());
        assert_eq!(*result.center_right(), center_right);

        println!("Center Left:");
        println!("{}", center_left);
        println!("{}", *result.center_left());
        assert_eq!(*result.center_left(), center_left);

        println!("Top:");
        println!("{}", top);
        println!("{}", *result.top());
        assert_eq!(*result.top(), top);

        println!("Top Right:");
        println!("{}", top_right);
        println!("{}", *result.top_right());
        assert_eq!(*result.top_right(), top_right);

        println!("Top Left:");
        println!("{}", top_left);
        println!("{}", *result.top_left());
        assert_eq!(*result.top_left(), top_left);

        println!("Bottom Left:");
        println!("{}", bottom_left);
        println!("{}", *result.bottom_left());
        assert_eq!(*result.bottom_left(), bottom_left);

        println!("Bottom:");
        println!("{}", bottom);
        println!("{}", *result.bottom());
        assert_eq!(*result.bottom(), bottom);

        println!("Bottom Right:");
        println!("{}", bottom_right);
        println!("{}", *result.bottom_right());
        assert_eq!(*result.bottom_right(), bottom_right);
    }

    #[test]
    pub fn test_neighborhood_neighborhood() {
        let start = AxialBitboard(0x8100000000000081);
        let expected_center = AxialBitboard(0xe7c7870000e1e3e7);
        let expected_center_left = AxialBitboard(0x303030000000103);
        let expected_center_right = AxialBitboard(0xc080000000c0c0c0);
        let expected_top = AxialBitboard(0xe1e3);
        let expected_bottom = AxialBitboard(0xc787000000000000);
        let expected_top_right = AxialBitboard(0xc0c0);
        let expected_bottom_right = AxialBitboard(0x8000000000000000);
        let expected_top_left = AxialBitboard(0x1);
        let expected_bottom_left = AxialBitboard(0x303000000000000);

        let expected = Neighborhood {
            boards: [ 
                expected_bottom_right,
                expected_bottom,
                expected_bottom_left,
                expected_center_right,
                expected_center,
                expected_center_left,
                expected_top_right,
                expected_top,
                expected_top_left,
            ]
        };


        println!("Expected:\n{}", expected);

        let n1 = start.neighborhood();


        let mut result = n1.neighborhood();
        result.combine(&n1, NEIGHBORHOOD_CENTER_INDEX as usize);
        *result.center() |= start;

        println!("Starting from:\n{}", start);
        println!("Got:\n{}", result);
        assert_eq!(*result.center(), expected_center);
        assert_eq!(*result.center_left(), expected_center_left);
        assert_eq!(*result.center_right(), expected_center_right);
        assert_eq!(*result.top(), expected_top);
        assert_eq!(*result.bottom(), expected_bottom);
        assert_eq!(*result.top_right(), expected_top_right);
        assert_eq!(*result.bottom_right(), expected_bottom_right);
        assert_eq!(*result.top_left(), expected_top_left);
        assert_eq!(*result.bottom_left(), expected_bottom_left);


    }
}
