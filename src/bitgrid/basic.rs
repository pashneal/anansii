use super::*;
use crate::hex_grid::*;
use crate::location::*;
use crate::piece::*;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

const CENTER_BOARD_INDEX: usize = 24;
const CENTER_BITBOARD_INDEX: usize = 28;
const GRID_WIDTH: usize = 7;
const GRID_HEIGHT: usize = 7;
const GRID_SIZE: usize = GRID_WIDTH * GRID_HEIGHT;

/// Represents positions of Hive with Pillbug Mosquito and Ladybug
/// that follow the One Hive rules (see Hive Rules for more information)
/// and has no greater than 6 pieces with height > 1
///
/// Only beetles and mosquitos can be at height > 1 in this representation
/// as opposed to HexGrid which is more relaxed in its constraints.
/// As a result, all BasicBitGrids can be converted to HexGrids.
///
/// See the documentation of the bit grid's AxialBitboard to
/// understand how the grid is represented at the bit level
///
/// Zooming out, the grid is represented instead as
/// a 7x7 grid of AxialBitboards
///
/// The grid indices are laid out in the conventional x-y axis follows:
/// TODO: conventional?? Isn't negative x going in the wrong direction? Perhaps
/// compare it to HexGridLocation instead
///
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GridBounds {
    pub top_left: BitGridLocation,
    pub bottom_right: BitGridLocation,
}

impl GridBounds {
    pub fn width(&self) -> usize {
        let (left_x, right_x) = (
            self.top_left.bitboard_index % BITBOARD_WIDTH,
            self.bottom_right.bitboard_index % BITBOARD_WIDTH,
        );

        let (max_board_x, min_board_x) = (
            self.top_left.board_index % GRID_WIDTH,
            self.bottom_right.board_index % GRID_WIDTH,
        );

        let board_difference = max_board_x as i8 - min_board_x as i8;
        let extra_padding = (board_difference - 1) * BITBOARD_WIDTH as i8;

        // Contribution from left width of rightmost bitboard
        let mut width = (BITBOARD_WIDTH - right_x) as i8;

        width += extra_padding as i8;
        width += left_x as i8 + 1;

        width as usize
    }

    pub fn height(&self) -> usize {
        let (top_y, bottom_y) = (
            self.top_left.bitboard_index / BITBOARD_HEIGHT,
            self.bottom_right.bitboard_index / BITBOARD_HEIGHT,
        );

        let (max_board_y, min_board_y) = (
            self.top_left.board_index / GRID_HEIGHT,
            self.bottom_right.board_index / GRID_HEIGHT,
        );

        let board_difference = max_board_y as i8 - min_board_y as i8;
        let extra_padding = (board_difference - 1) * BITBOARD_HEIGHT as i8;

        // Contribution from top height of bottommost bitboard
        let mut height = (BITBOARD_HEIGHT - bottom_y) as i8;

        height += extra_padding as i8;
        height += top_y as i8 + 1;

        height as usize
    }
}
#[derive(Debug, Clone)]
pub struct BasicBitGrid {
    pub queens: Grid,
    pub beetles: Grid,
    pub spiders: Grid,
    pub grasshoppers: Grid,
    pub ants: Grid,
    pub pillbugs: Grid,
    pub ladybugs: Grid,
    pub mosquitos: Grid,
    pub all_pieces: Grid,
    pub white_pieces: Grid,
    pub black_pieces: Grid,
    pub stacks: BasicBitStack,
}

impl Default for BasicBitGrid {
    fn default() -> Self {
        Self::new()
    }
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
            stacks: BasicBitStack::new(),
        }
    }

    /// Deterministically chooses a HexLocation that contains to a single piece
    /// on the board. Returns none if no piece exists within the bounds of an equivalent HexGrid
    fn find_one_hex(&self) -> Option<HexLocation> {
        let left = -((HEX_GRID_SIZE / 2) as i8);
        let right = (HEX_GRID_SIZE / 2) as i8;
        let top = -((HEX_GRID_SIZE / 2) as i8);
        let bottom = (HEX_GRID_SIZE / 2) as i8;

        for row in top..=bottom {
            for col in left..=right {
                let hex_location = HexLocation::new(row, col);
                let bit_location: BitGridLocation = hex_location.into();
                if !self.peek(bit_location).is_empty() {
                    return Some(hex_location);
                }
            }
        }
        None
    }

    pub fn add(&mut self, piece: Piece, loc: BitGridLocation) {
        debug_assert!(loc.board_index < GRID_SIZE);
        let color = piece.color;
        let piece = piece.piece_type;
        let all_pieces = self.get_mut_all_pieces().get_mut(loc.board_index).unwrap();

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
                let stack = self.stacks.find_all(loc.into());

                let height = stack.len() + 1;
                let height = height as u8;

                let entry = BasicBitStackEntry::new(piece.into(), height, loc.into(), color.into());
                self.stacks.insert(entry);
            }
            _ => panic!("Cannot add a {:?} to the top of the hive", piece),
        }
    }
    pub fn top(&self, loc: BitGridLocation) -> Option<Piece> {
        let pieces = self.peek(loc);
        pieces.last().cloned()
    }

    pub fn remove(&mut self, piece: Piece, loc: BitGridLocation) {
        debug_assert!(
            self.top(loc).expect("piece should exist") == piece,
            "input piece should match existing top piece"
        );
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

        let all_pieces = self.get_mut_all_pieces().get_mut(loc.board_index).unwrap();
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

    pub fn get_mut_piece(&mut self, piece_type: PieceType) -> &mut Grid {
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

    pub fn get_piece(&self, piece_type: PieceType) -> &Grid {
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

    pub fn peek(&self, loc: BitGridLocation) -> Vec<Piece> {
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

        for p in piece_types.iter() {
            let board = self.get_piece(*p)[loc.board_index];
            if board.peek(loc.bitboard_index) {
                piece_type = Some(*p);
            }
        }

        let color = if self.white_pieces[loc.board_index].peek(loc.bitboard_index) {
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
            let piece = Piece::new(piece_type, color);
            pieces.push(piece);
        }

        pieces
    }

    pub fn get_mut_all_pieces(&mut self) -> &mut Grid {
        &mut self.all_pieces
    }

    pub fn get_mut_color(&mut self, color: PieceColor) -> &mut Grid {
        match color {
            PieceColor::White => &mut self.white_pieces,
            PieceColor::Black => &mut self.black_pieces,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.all_pieces.iter().all(|&board| board.is_empty())
    }

    /// Returns a smallest bounding box that contains all pieces on the grid.
    /// If no pieces on the grid, returns None
    pub fn bounding_box(&self) -> Option<GridBounds> {
        if self.is_empty() {
            return None;
        }

        let mut min_board_x = GRID_WIDTH;
        let mut min_board_y = GRID_HEIGHT;
        let mut max_board_x = 0;
        let mut max_board_y = 0;

        let mut bottom_right = BitboardCoords {
            x: BITBOARD_WIDTH,
            y: BITBOARD_HEIGHT,
        };
        let mut top_left = BitboardCoords {
            x: BITBOARD_WIDTH,
            y: BITBOARD_HEIGHT,
        };

        for board_x in 0..GRID_WIDTH {
            for board_y in 0..GRID_HEIGHT {
                let board_index = board_y * GRID_HEIGHT + board_x;
                let bitboard = self.all_pieces[board_index];

                let Some(BitboardBounds {
                    top_left: tl,
                    bottom_right: br,
                }) = bitboard.bounding_box()
                else {
                    continue;
                };

                if board_x == min_board_x {
                    bottom_right.x = br.x.min(bottom_right.x);
                }

                if board_x == max_board_x {
                    top_left.x = tl.x.max(top_left.x);
                }

                if board_x < min_board_x {
                    min_board_x = board_x;
                    bottom_right.x = br.x;
                }

                if board_x > max_board_x {
                    max_board_x = board_x;
                    top_left.x = tl.x;
                }

                if board_y == min_board_y {
                    bottom_right.y = br.y.min(bottom_right.y);
                }

                if board_y == max_board_y {
                    top_left.y = tl.y.max(top_left.y);
                }

                if board_y < min_board_y {
                    min_board_y = board_y;
                    bottom_right.y = br.y;
                }

                if board_y > max_board_y {
                    max_board_y = board_y;
                    top_left.y = tl.y;
                }
            }
        }

        let top_left_board_index = max_board_y * GRID_WIDTH + max_board_x;
        let top_left_bitboard_index = top_left.y * BITBOARD_WIDTH + top_left.x;
        let top_left = BitGridLocation::new(top_left_board_index, top_left_bitboard_index);

        let bottom_right_board_index = min_board_y * GRID_WIDTH + min_board_x;
        let bottom_right_bitboard_index = bottom_right.y * BITBOARD_WIDTH + bottom_right.x;
        let bottom_right =
            BitGridLocation::new(bottom_right_board_index, bottom_right_bitboard_index);

        Some(GridBounds {
            top_left,
            bottom_right,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BitGridLocation {
    pub board_index: usize,
    pub bitboard_index: usize,
}

impl BitGridLocation {
    pub fn new(board_index: usize, bitboard_index: usize) -> Self {
        BitGridLocation {
            board_index,
            bitboard_index,
        }
    }

    fn apply(&self, direction: Direction) -> BitGridLocation {
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
    fn wrap(&self, board: i8, x: i8, y: i8) -> BitGridLocation {
        let width = BITBOARD_WIDTH as i8;
        let height = BITBOARD_HEIGHT as i8;

        let board_x = board % GRID_WIDTH as i8;
        let board_y = board / GRID_HEIGHT as i8;

        let dx = match x {
            x if x < 0 => -1,
            x if x >= width => 1,
            _ => 0,
        };

        let dy = match y {
            y if y < 0 => -1,
            y if y >= height => 1,
            _ => 0,
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

        let bit_x = (center_x - hex.x + BITBOARD_WIDTH as i8).rem_euclid(BITBOARD_WIDTH as i8);
        let bit_y = (center_y - hex.y + BITBOARD_HEIGHT as i8).rem_euclid(BITBOARD_HEIGHT as i8);

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
    fn pieces(&self) -> Vec<(Vec<Piece>, HexLocation)> {
        // We use the fact that the equivalence of
        // BasicBitGridLocation to HexLocation is closed under adjacency to
        // convert a set of BasicBitGridLocations to an equivalent set of HexLocations
        //
        // In other words, even though there are multiple valid HexLocations
        // that map to the same BasicBitGridLocation (due to wrapping), and thus multiple valid
        // conversions we can guarantee a deterministic conversion by:
        //
        //  1. deterministically choosing a single starting BasicBitGridLocation
        //  2. deterministically converting that to a HexLocation
        //  3. performing deterministic adjacency operations on that HexLocation
        //  4. converting the resulting HexLocations back to BasicBitGridLocations
        //     (via the FromHex trait), continuing this process until
        //     we have found all desired locations
        //
        // This will find a deterministic set of HexLocations that are on or adjacent
        // to our chosen starting BasicBitGridLocation

        if None == self.find_one_hex() {
            return Vec::new();
        }

        let hex = self.find_one_hex().unwrap();

        fn dfs(
            grid: &BasicBitGrid,
            current_loc: HexLocation,
            visited: &mut HashSet<HexLocation>,
            result: &mut Vec<(Vec<Piece>, HexLocation)>,
        ) {
            if visited.contains(&current_loc) {
                return;
            }

            let pieces = grid.peek(current_loc.into());

            if pieces.is_empty() {
                return;
            }

            visited.insert(current_loc);
            result.push((pieces, current_loc));
            for direction in Direction::all() {
                let next_loc = current_loc.apply(direction);
                dfs(grid, next_loc, visited, result);
            }
        }

        // Perform the conversion as described above
        // we can use depth first search because all piece
        // locations are guaranteed to be connected
        let mut visited = HashSet::new();
        let mut result = Vec::new();
        dfs(self, hex, &mut visited, &mut result);

        // First sort by Hexlocation y then by x
        result.sort_by(|a, b| a.1.y.cmp(&b.1.y).then(a.1.x.cmp(&b.1.x)));

        result
    }
}

impl PartialEq<HexGrid> for BasicBitGrid {
    fn eq(&self, other: &HexGrid) -> bool {
        self.pieces() == other.pieces()
    }
}

impl PartialEq<BasicBitGrid> for BasicBitGrid {
    fn eq(&self, other: &BasicBitGrid) -> bool {
        self.pieces() == other.pieces()
    }
}

/// TODO: this needs to be a try from, not all HexGrids are valid BasicBitGrids
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

impl From<&HexGrid> for BasicBitGrid {
    fn from(hex_grid: &HexGrid) -> Self {
        let mut bit_grid = BasicBitGrid::new();
        for (stack, loc) in hex_grid.pieces() {
            for piece in stack {
                bit_grid.add(piece, BitGridLocation::from_hex(loc));
            }
        }
        bit_grid
    }
}

impl Display for BasicBitGrid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let hex_grid: HexGrid = self.clone().into();
        write!(f, "{}", hex_grid.to_dsl())?;
        Ok(())
    }
}

/// Marker trait for types that can iterate over pieces.
/// Promises the compiler that the type can be converted to a BasicBitGrid
pub trait BasicBitConvertible: PieceIterator {}

impl<I: BasicBitConvertible> From<I> for BasicBitGrid {
    fn from(iter: I) -> Self {
        let mut grid = BasicBitGrid::new();
        for (stack, loc) in iter.pieces() {
            for piece in stack {
                grid.add(piece, BitGridLocation::from_hex(loc));
            }
        }
        grid
    }
}

impl<I: BasicBitConvertible> PartialEq<I> for BasicBitGrid {
    fn eq(&self, other: &I) -> bool {
        self.pieces() == other.pieces()
    }
}

/// Marker trait for types that can iterate over pieces.
/// Promises the compiler that the type can be converted to a HexGrid
impl HexGridConvertible for BasicBitGrid {}

#[cfg(test)]
mod tests {
    use super::BasicBitGrid;
    use super::BitGridLocation;
    use super::*;
    use crate::testing_utils::is_localized;
    pub const MAX_WRAP_BEFORE_COLLISION: usize = 28;

    #[test]
    pub fn test_bitgrid_location() {
        let hex = HexLocation::center();
        let bitgrid = BitGridLocation::from_hex(hex);
        assert_eq!(bitgrid, BitGridLocation::center());
    }

    #[test]
    pub fn test_bitgrid_wrap() {
        let start = BitGridLocation::center();
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

        assert_eq!(e.board_index, start.board_index - 1);
        assert_eq!(w.board_index, start.board_index + 1);
        assert_eq!(ne.board_index, start.board_index + GRID_WIDTH - 1);
        assert_eq!(nw.board_index, start.board_index + GRID_WIDTH);
        assert_eq!(se.board_index, start.board_index - GRID_WIDTH);
        assert_eq!(sw.board_index, start.board_index - GRID_WIDTH + 1);

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

        assert_eq!(e.board_index, start.board_index - 2);
        assert_eq!(w.board_index, start.board_index + 2);
        assert_eq!(ne.board_index, start.board_index + 2 * GRID_WIDTH - 2);
        assert_eq!(nw.board_index, start.board_index + 2 * GRID_WIDTH);
        assert_eq!(se.board_index, start.board_index - 2 * GRID_WIDTH);
        assert_eq!(sw.board_index, start.board_index - 2 * GRID_WIDTH + 2);

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
        println!(
            "{:#?}",
            hex_grid
                .pieces()
                .into_iter()
                .map(|(p, l)| (p, BitGridLocation::from_hex(l)))
                .collect::<Vec<_>>()
        );
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
        println!(
            "{:#?}",
            hex_grid
                .pieces()
                .into_iter()
                .map(|(p, l)| (p, BitGridLocation::from_hex(l)))
                .collect::<Vec<_>>()
        );
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
        println!(
            "{:#?}",
            hex_grid
                .pieces()
                .into_iter()
                .map(|(p, l)| (p, BitGridLocation::from_hex(l)))
                .collect::<Vec<_>>()
        );
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
        println!(
            "{}",
            HexGrid::from_dsl(concat!("a\n\n", "start - [0 0]\n\n",)).to_dsl()
        );
        let mut bit_grid = BasicBitGrid::new();
        for (stack, loc) in hex_grid.pieces() {
            for piece in stack {
                bit_grid.add(piece, BitGridLocation::from_hex(loc));
            }
        }

        let test: HexGrid = bit_grid.clone().try_into().unwrap();
        println!("{}", test.to_dsl());
        println!("{}", hex_grid.to_dsl());
        assert!(test == hex_grid, "These board's pieces should match");
        assert!(bit_grid == test, "These board's pieces should match");
        assert!(bit_grid == hex_grid, "These board's pieces should match");

        for (stack, loc) in hex_grid.pieces() {
            for &piece in stack.iter().rev() {
                bit_grid.remove(piece, BitGridLocation::from_hex(loc));
            }
        }

        let test: HexGrid = bit_grid.clone().try_into().unwrap();
        println!("{}", test.to_dsl());

        println!("{:#?}", bit_grid.pieces());
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

        let bit_grid: BasicBitGrid = hex_grid.clone().into();
        let result: HexGrid = bit_grid.try_into().unwrap();

        assert_eq!(result, hex_grid, "These board's pieces should match");
    }

    #[test]
    pub fn test_center_localized() {
        let reference = HexLocation::center();
        let start: BitGridLocation = reference.into();

        assert!(is_localized::<BitGridLocation>(
            start,
            reference,
            MAX_WRAP_BEFORE_COLLISION - 1
        ));
    }

    #[test]
    pub fn test_few_locations_localized() {
        for row in -5..5 {
            for col in -5..5 {
                let reference = HexLocation::new(row, col);
                let start: BitGridLocation = reference.into();
                assert!(is_localized::<BitGridLocation>(
                    start,
                    reference,
                    MAX_WRAP_BEFORE_COLLISION - 1
                ));
            }
        }
    }

    #[ignore = "test is slow, be sure to run with --release -- --ignored"]
    #[test]
    pub fn test_many_locations_localized() {
        for row in -30..30 {
            for col in -30..30 {
                let reference = HexLocation::new(row, col);
                let start: BitGridLocation = reference.into();
                assert!(is_localized::<BitGridLocation>(
                    start,
                    reference,
                    MAX_WRAP_BEFORE_COLLISION - 1
                ));
            }
        }
    }

    fn add_board_to_grid(grid: &mut BasicBitGrid, board_index: usize, bitboard: AxialBitboard) {
        for coords in bitboard.into_iter() {
            let bit_index = coords.y * BITBOARD_WIDTH + coords.x;
            let loc = BitGridLocation::new(board_index, bit_index);
            let filler_piece = Piece::new(PieceType::Ant, PieceColor::Black);
            grid.add(filler_piece, loc);
        }
    }

    #[test]
    pub fn test_small_bounding_box() {
        // Note: we have to make sure that this still follows the One Hive rule!
        // so all the boards form at most one connected component
        let bottom_board = AxialBitboard::from_u64(0x10705018121e0400);
        let top_board = AxialBitboard::from_u64(0xf80808081010);

        let mut grid = BasicBitGrid::new();
        let center = CENTER_BOARD_INDEX;
        let above_center = center + GRID_WIDTH;

        add_board_to_grid(&mut grid, center, bottom_board);
        add_board_to_grid(&mut grid, above_center, top_board);

        let bounds = grid.bounding_box().unwrap();
        let expected_bottom_right = bottom_board.bounding_box().unwrap().bottom_right;
        let expected_top_left = top_board.bounding_box().unwrap().top_left;

        let expected_tl = expected_top_left.index();
        let expected_br = expected_bottom_right.index();

        let expected_bounds = GridBounds {
            top_left: BitGridLocation::new(above_center, expected_tl),
            bottom_right: BitGridLocation::new(center, expected_br),
        };

        assert_eq!(bounds, expected_bounds);
    }

    #[test]
    pub fn test_single_board_bounding_box() {
        // Note: we have to make sure that this still follows the One Hive rule!
        // so all the boards form at most one connected component
        let board = AxialBitboard::from_u64(0x4fc0808081000);
        let mut grid = BasicBitGrid::new();

        add_board_to_grid(&mut grid, CENTER_BOARD_INDEX, board);

        let bounds = grid.bounding_box().unwrap();
        let expected_bounds = board.bounding_box().unwrap();

        let expected_tl = expected_bounds.top_left.index();
        let expected_br = expected_bounds.bottom_right.index();

        let expected_bounds = GridBounds {
            top_left: BitGridLocation::new(CENTER_BOARD_INDEX, expected_tl),
            bottom_right: BitGridLocation::new(CENTER_BOARD_INDEX, expected_br),
        };

        assert_eq!(bounds, expected_bounds);
    }

    #[test]
    pub fn test_empty_board_bounding_box() {
        let grid = BasicBitGrid::new();
        assert!(grid.bounding_box().is_none());
    }

    #[test]
    pub fn test_large_bounding_box() {
        let mut grid = BasicBitGrid::new();

        // Boards laid out like this
        // Note: we have to make sure that this still follows the One Hive rule!
        // so all the boards form at most one connected component
        // 8 7 6
        // 5 4 3
        // 2 1 0
        let board_0 = AxialBitboard::from_u64(0xfcfcfcfcfcfc0000);
        let board_1 = AxialBitboard::from_u64(0xffffffff10101000); // lowest
        let board_2 = AxialBitboard::from_u64(0x707000000000000); // leftest
        let board_3 = AxialBitboard::from_u64(0x80808ff08080808); // rightest
        let board_4 = AxialBitboard::from_u64(0xffffffffffffffff); // completely full
        let board_5 = AxialBitboard::from_u64(0x0); // completely empty
        let board_6 = AxialBitboard::from_u64(0x20f0);
        let board_7 = AxialBitboard::from_u64(0x1010ff); // highest
        let board_8 = AxialBitboard::from_u64(0x1);

        println!(
            "{}\n{}\n{}\n\n{}\n{}\n{}\n\n{}\n{}\n{}",
            board_8, board_7, board_6, board_5, board_4, board_3, board_2, board_1, board_0
        );

        let top_left_coords = BitboardCoords { x: 2, y: 2 };
        let bottom_right_coords = BitboardCoords { x: 0, y: 1 };
        let expected_bounds = GridBounds {
            top_left: BitGridLocation::new(2 * GRID_WIDTH + 2, top_left_coords.index()),
            bottom_right: BitGridLocation::new(0, bottom_right_coords.index()),
        };

        add_board_to_grid(&mut grid, 0 * GRID_WIDTH + 0, board_0);
        add_board_to_grid(&mut grid, 0 * GRID_WIDTH + 1, board_1);
        add_board_to_grid(&mut grid, 0 * GRID_WIDTH + 2, board_2);

        add_board_to_grid(&mut grid, 1 * GRID_WIDTH + 0, board_3);
        add_board_to_grid(&mut grid, 1 * GRID_WIDTH + 1, board_4);
        add_board_to_grid(&mut grid, 1 * GRID_WIDTH + 2, board_5);

        add_board_to_grid(&mut grid, 2 * GRID_WIDTH + 0, board_6);
        add_board_to_grid(&mut grid, 2 * GRID_WIDTH + 1, board_7);
        add_board_to_grid(&mut grid, 2 * GRID_WIDTH + 2, board_8);

        let bounds = grid.bounding_box().expect("Bounds should not be empty!");
        assert_eq!(bounds, expected_bounds);
    }

    #[test]
    pub fn test_small_bounding_box_dimensions() {
        // Note: we have to make sure that this still follows the One Hive rule!
        // so all the boards form at most one connected component
        let bottom_board = AxialBitboard::from_u64(0x10705018121e0400);
        let top_board = AxialBitboard::from_u64(0xf80808081010);

        let mut grid = BasicBitGrid::new();
        let center = CENTER_BOARD_INDEX;
        let above_center = center + GRID_WIDTH;

        add_board_to_grid(&mut grid, center, bottom_board);
        add_board_to_grid(&mut grid, above_center, top_board);

        let bounds = grid.bounding_box().unwrap();
        let width = bounds.width();
        let height = bounds.height();

        assert_eq!(width, 7);
        assert_eq!(height, 13);
    }

    #[test]
    pub fn test_single_board_bounding_box_dimensions() {
        // Note: we have to make sure that this still follows the One Hive rule!
        // so all the boards form at most one connected component
        let board = AxialBitboard::from_u64(0x4fc0808081000);
        let mut grid = BasicBitGrid::new();

        add_board_to_grid(&mut grid, CENTER_BOARD_INDEX, board);

        let bounds = grid.bounding_box().unwrap();
        let width = bounds.width();
        let height = bounds.height();

        assert_eq!(width, 6);
        assert_eq!(height, 6);
    }

    #[test]
    pub fn test_large_bounding_box_dimensions() {
        let mut grid = BasicBitGrid::new();

        // Boards laid out like this
        // Note: we have to make sure that this still follows the One Hive rule!
        // so all the boards form at most one connected component
        // 8 7 6
        // 5 4 3
        // 2 1 0
        let board_0 = AxialBitboard::from_u64(0xfcfcfcfcfcfc0000);
        let board_1 = AxialBitboard::from_u64(0xffffffff10101000); // lowest
        let board_2 = AxialBitboard::from_u64(0x707000000000000); // leftest
        let board_3 = AxialBitboard::from_u64(0x80808ff08080808); // rightest
        let board_4 = AxialBitboard::from_u64(0xffffffffffffffff); // completely full
        let board_5 = AxialBitboard::from_u64(0x0); // completely empty
        let board_6 = AxialBitboard::from_u64(0x20f0);
        let board_7 = AxialBitboard::from_u64(0x1010ff); // highest
        let board_8 = AxialBitboard::from_u64(0x1);

        add_board_to_grid(&mut grid, 0 * GRID_WIDTH + 0, board_0);
        add_board_to_grid(&mut grid, 0 * GRID_WIDTH + 1, board_1);
        add_board_to_grid(&mut grid, 0 * GRID_WIDTH + 2, board_2);

        add_board_to_grid(&mut grid, 1 * GRID_WIDTH + 0, board_3);
        add_board_to_grid(&mut grid, 1 * GRID_WIDTH + 1, board_4);
        add_board_to_grid(&mut grid, 1 * GRID_WIDTH + 2, board_5);

        add_board_to_grid(&mut grid, 2 * GRID_WIDTH + 0, board_6);
        add_board_to_grid(&mut grid, 2 * GRID_WIDTH + 1, board_7);
        add_board_to_grid(&mut grid, 2 * GRID_WIDTH + 2, board_8);

        let bounds = grid.bounding_box().expect("Bounds should not be empty!");
        let width = bounds.width();
        let height = bounds.height();

        assert_eq!(width, 19);
        assert_eq!(height, 18);
    }

    #[test]
    pub fn test_one_bounding_box_dimensions() {
        let mut grid = BasicBitGrid::new();
        let board = AxialBitboard::from_u64(0x1);
        add_board_to_grid(&mut grid, CENTER_BOARD_INDEX, board);
        let bounds = grid.bounding_box().unwrap();

        let width = bounds.width();
        let height = bounds.height();

        assert_eq!(width, 1);
        assert_eq!(height, 1);
        assert_eq!(bounds.top_left, bounds.bottom_right);
    }
}
