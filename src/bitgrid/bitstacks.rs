use std::iter::IntoIterator;
//IntoIterator

/// A densly packed representation of pieces in a stack
/// greater than 1 height in the game of Hive. Represents
/// pieces, their height, color all in a few bytes!
///
/// Supports operations for BasicBitGrids.
///
/// TODO: can we do better for more complicated structures
/// if we don't have to worry about board num,
/// height, presence, or piece (for example in games without mosquitos)
/// we could even shave a bit off locations if we assume things can't be
/// further than 32 from any other? more expensive computation though
///
/// Another idea is to assign holes to white mosquito, black beetle etc,
///
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BasicBitStackEntry {
    /// 1 bit for piece (beetle/mosquito)
    /// 3 bits for height (can represent 2 - 7)
    /// 10 bits for location (x => 3 , y => 3, board_num => 4)
    /// 1 bit for color (white/black)
    /// 1 bit for presence (is this a valid entry)
    data: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BasicBitStack {
    bitset: SmallBitset,
    stack: [BasicBitStackEntry; 6],
}

pub const PIECE_BITS: u8 = 1;
pub const HEIGHT_BITS: u8 = 3;
pub const LOCATION_BITS: u8 = 10;
pub const COLOR_BITS: u8 = 1;

pub const PRESENCE_MASK: u16 = 1 << 15;

#[repr(u8)]
pub enum StackPiece {
    Beetle = 0,
    Mosquito = 1,
}

#[repr(u8)]
pub enum StackColor {
    Beetle = 0,
    Mosquito = 1,
}

// TODO: Perhaps experiment with smaller representation of
// location
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BasicBitLocation {
    pub x: u16,
    pub y: u16,
    pub board_num: u16,
}

impl BasicBitStackEntry {
    // TODO: check how how this gets translated to assembly
    pub fn new(
        piece: StackPiece,
        height: u8,
        location: BasicBitLocation,
        color: StackColor,
    ) -> Self {
        let piece = piece as u16;
        let height = height as u16;
        let color = color as u16;

        let height_shift = PIECE_BITS;
        let location_shift = PIECE_BITS + HEIGHT_BITS;
        let color_shift = PIECE_BITS + HEIGHT_BITS + LOCATION_BITS;

        let location = location.x | (location.y << 3) | (location.board_num << 6);
        let mut data = piece
            | (height << height_shift)
            | (location << location_shift)
            | (color << color_shift);
        data |= PRESENCE_MASK;
        BasicBitStackEntry { data }
    }
}

impl Default for BasicBitStackEntry {
    fn default() -> Self {
        BasicBitStackEntry { data: 0 }
    }
}

impl BasicBitStack {
    pub fn new() -> Self {
        BasicBitStack {
            bitset: SmallBitset::new(),
            stack: [BasicBitStackEntry::default(); 6],
        }
    }

    pub fn insert(&mut self, entry: BasicBitStackEntry) {
        let index = self.bitset.insert();
        self.stack[index] = entry;
    }

    pub fn remove(&mut self, index: usize) {
        self.bitset.remove_index(index);
    }

    /// TODO: transmute shenanigans
    pub fn find(&self, location: BasicBitLocation) -> Option<usize> {
        for index in self.bitset.into_iter() {
            let entry = self.stack[index];
            let entry_location = BasicBitLocation {
                x: (entry.data >> (PIECE_BITS + HEIGHT_BITS)) & 0b111,
                y: (entry.data >> (PIECE_BITS + HEIGHT_BITS + 3)) & 0b111,
                board_num: (entry.data >> (PIECE_BITS + HEIGHT_BITS + 6)) & 0b1111,
            };
            if entry_location == location {
                return Some(index);
            }
        }

        todo!()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SmallBitset {
    set: u8,
}

impl SmallBitset {
    pub fn new() -> Self {
        SmallBitset { set: 0 }
    }

    #[inline(always)]
    pub fn insert(&mut self) -> usize {
        let empty_mask = self.get_empty();
        self.set |= empty_mask;
        empty_mask.trailing_ones() as usize
    }

    #[inline(always)]
    pub fn get_empty(&self) -> u8 {
        let empty = 1 << self.set.trailing_ones() as u8;
        debug_assert!(empty & self.set == 0);
        empty
    }

    #[inline(always)]
    pub fn has_index(&self, index: u8) -> bool {
        (self.set & (1 << index)) != 0
    }

    #[inline(always)]
    pub fn remove_index(&mut self, index: usize) {
        self.set &= !(1 << index as u8);
    }
}

pub struct SmallBitsetIterator {
    set: u8,
}

impl Iterator for SmallBitsetIterator {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.set == 0 {
            return None;
        }
        let index = self.set.trailing_zeros();
        let lsb = 1 << index;
        self.set &= !lsb;
        Some(index as usize)
    }
}

impl IntoIterator for SmallBitset {
    type Item = usize;
    type IntoIter = SmallBitsetIterator;

    fn into_iter(self) -> Self::IntoIter {
        SmallBitsetIterator { set: self.set }
    }
}

#[test]
pub fn stack_size_is_small() {
    assert_eq!(std::mem::size_of::<BasicBitStackEntry>(), 2);
    assert!(std::mem::size_of::<BasicBitStack>() <= 16);
}
