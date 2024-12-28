use std::fmt::{Display, Formatter};
use std::ops;

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

#[derive(Debug, Copy, Clone)]
pub struct BitboardCoords{
    pub x: usize, 
    pub y: usize
}
#[derive(Debug, Copy, Clone)]
pub struct BitboardBounds{
    pub top_left : BitboardCoords,
    pub bottom_right: BitboardCoords,
}

impl AxialBitboard {
    #[inline(always)]
    pub fn from_u64(board: u64) -> Self {
        AxialBitboard(board)
    }

    #[inline(always)]
    pub fn from_bitboard(other: AxialBitboard) -> Self {
        AxialBitboard(other.0)
    }

    #[inline(always)]
    pub fn shift_west(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 1)
    }

    #[inline(always)]
    pub fn shift_east(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> 1)
    }

    #[inline(always)]
    pub fn shift_northwest(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 8)
    }

    #[inline(always)]
    pub fn shift_northeast(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 7)
    }

    #[inline(always)]
    pub fn shift_southwest(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> 7)
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
        if self.is_empty() { return None; }

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

        Some(BitboardBounds{
            top_left: BitboardCoords{x: max_x, y: max_y},
            bottom_right: BitboardCoords{x: min_x, y: min_y}
        })
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

impl Display for AxialBitboard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for i in (0..BITBOARD_HEIGHT).rev() {
            for j in (0..BITBOARD_WIDTH).rev() {
                let index = i * BITBOARD_WIDTH + j;
                if self.peek(index) {
                    write!(f, "■")?;
                } else {
                    write!(f, "□")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
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
            "□□□□□□□□\n",
            "□□□□□□□□\n",
            "□□□□□□□□\n",
            "□□□□□□□□\n",
            "□□□□□□□□\n",
            "□□□□□□□□\n",
            "□□□□□□□□\n",
            "■□□□□□□□\n",
        );

        let start = AxialBitboard::from_u64(0x80);
        assert_eq!(format!("{}", start), output);
    }
}
