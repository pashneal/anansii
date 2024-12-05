use std::ops;

/// Represents a internal part of the Hive grid
///
/// Compositions of AxialBitboards are used to represent the full
/// hive grid and game state
///
/// Locations are represented by axial coordinates mapping to bit
/// numbers as follows:
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

pub const BITBOARD_HEIGHT : usize = 8;
pub const BITBOARD_WIDTH : usize = 8;

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
    pub fn shift_north_west(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 8)
    }

    #[inline(always)]
    pub fn shift_north_east(&self) -> AxialBitboard {
        AxialBitboard(self.0 << 7)
    }

    #[inline(always)]
    pub fn shift_south_west(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> 7)
    }

    #[inline(always)]
    pub fn shift_south_east(&self) -> AxialBitboard {
        AxialBitboard(self.0 >> 8)
    }

    #[inline(always)]
    pub fn peek(&self, index : usize) -> bool {
        (self.0 & (1 << index)) != 0
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
    assert_eq!(start.shift_north_west(), nw);
    assert_eq!(start.shift_north_east(), ne);
    assert_eq!(start.shift_south_west(), sw);
    assert_eq!(start.shift_south_east(), se);
}

#[test]
pub fn test_direction_cohesion() {
    let start = AxialBitboard::from_u64(0x800000000);
    let everywhere = start
        .shift_west()
        .shift_east()
        .shift_north_west()
        .shift_north_east()
        .shift_south_west()
        .shift_south_east();
    assert_eq!(start, everywhere);
}
