use std::iter::IntoIterator;
use crate::piece::*;
use super::*;
pub const PIECE_BITS: u8 = 1;
pub const HEIGHT_BITS: u8 = 3;
pub const HEIGHT_MASK: u8 = 0b111;
pub const COORD_MASK: u32 = 0b111;
pub const LOCATION_BITS: u8 = 12;
pub const LOCATION_MASK: u8 = 0b111111;
pub const COLOR_BITS: u8 = 1;
pub const PRESENCE_MASK: u32 = 1 << 17;

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
    /// 12 bits for location (x => 3 , y => 3, board_num => 6)
    /// 1 bit for color (white/black)
    data: u32,
}

impl BasicBitStackEntry {
    pub fn height(&self) -> u8 {
        ((self.data >> PIECE_BITS) as u8 & HEIGHT_MASK) + 1
    }

    pub fn piece(&self) -> StackPiece {
        unsafe { std::mem::transmute((self.data & 0b1) as u8) }
    }

    pub fn color(&self) -> StackColor {
        unsafe { std::mem::transmute((self.data >> (PIECE_BITS + HEIGHT_BITS + LOCATION_BITS)) as u8) }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BasicBitStack {
    bitset: SmallBitset,
    stack: [BasicBitStackEntry; 6],
}


#[repr(u8)]
pub enum StackPiece {
    Beetle = 0,
    Mosquito = 1,
}

impl From<PieceType> for  StackPiece{
    fn from(piece : PieceType) -> Self {
        match piece {
            PieceType::Beetle => StackPiece::Beetle,
            PieceType::Mosquito => StackPiece::Mosquito,
            _ => panic!("Invalid piece type")
        }
    }
}

impl From<StackPiece> for PieceType {
    fn from(piece : StackPiece) -> Self {
        match piece {
            StackPiece::Beetle => PieceType::Beetle,
            StackPiece::Mosquito => PieceType::Mosquito,
        }
    }
}

#[repr(u8)]
pub enum StackColor {
    White = 0,
    Black = 1,
}

impl From<StackColor> for PieceColor {
    fn from(color : StackColor) -> Self {
        match color {
            StackColor::White => PieceColor::White,
            StackColor::Black => PieceColor::Black,
        }
    }
} 

impl From<PieceColor> for StackColor {
    fn from(color : PieceColor) -> Self {
        match color {
            PieceColor::White => StackColor::White,
            PieceColor::Black => StackColor::Black,
        }
    }
}

// TODO: Perhaps experiment with smaller representation of
// location
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BasicBitLocation {
    pub x: u32,
    pub y: u32,
    pub board_num: u32,
}

impl From<BitGridLocation> for BasicBitLocation {
    fn from(location: BitGridLocation) -> Self {
        BasicBitLocation {
            x: location.bitboard_index as u32 % BITBOARD_WIDTH as u32,
            y: location.bitboard_index as u32 / BITBOARD_HEIGHT as u32,
            board_num: location.board_index as u32,
        }
    }

}

impl BasicBitStackEntry {
    // TODO: check how how this gets translated to assembly
    pub fn new(
        piece: StackPiece,
        height: u8,
        location: BasicBitLocation,
        color: StackColor,
    ) -> Self {
        let piece = piece as u32;
        let height = height as u32;
        let color = color as u32;

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
    
    pub fn get_height(&mut self, index: usize) -> u8 {
        let entry = self.stack[index];
        (entry.data >> PIECE_BITS) as u8 & 0b111
    }

    pub fn get(&self, index: usize) -> BasicBitStackEntry {
        self.stack[index]
    }

    /// TODO: transmute shenanigans
    pub fn find_one(&self, location: BasicBitLocation) -> Option<usize> {
        for index in self.bitset.into_iter() {
            let entry = self.stack[index];
            let entry_location = BasicBitLocation {
                x: (entry.data >> (PIECE_BITS + HEIGHT_BITS)) & COORD_MASK,
                y: (entry.data >> (PIECE_BITS + HEIGHT_BITS + 3)) & COORD_MASK,
                board_num: (entry.data >> (PIECE_BITS + HEIGHT_BITS + 6)) & LOCATION_MASK as u32,
            };
            if entry_location == location {
                return Some(index);
            }
        }

        None
    }

    /// Returns all of the pieces at a given stack in height
    /// order from lowest to highest
    pub fn find_all(&self, location: BasicBitLocation) -> Vec<usize> {
        let mut indices = Vec::new();
        for index in self.bitset.into_iter() {
            let entry = self.stack[index];
            let entry_location = BasicBitLocation {
                x: (entry.data >> (PIECE_BITS + HEIGHT_BITS)) & COORD_MASK,
                y: (entry.data >> (PIECE_BITS + HEIGHT_BITS + 3)) & COORD_MASK,
                board_num: (entry.data >> (PIECE_BITS + HEIGHT_BITS + 6)) & LOCATION_MASK as u32,
            };

            let height = (entry.data >> PIECE_BITS) & HEIGHT_MASK as u32;
            if entry_location == location {
                indices.push((height, index));
            }
        }

        indices.sort();
        let indices = indices.into_iter().map(|(_, index)| index).collect();
        indices
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
        empty_mask.trailing_zeros() as usize
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

