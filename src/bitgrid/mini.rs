use super::*;
use std::fmt::{Display, self};
use std::collections::HashSet;
use crate::hex_grid::HexGridConvertible;
use crate::piece::{Piece, PieceType, PieceColor, PieceIterator};
use crate::location::{Shiftable, FromHex, HexLocation, Direction};

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
/// TODO(optimization): move away from BasicBitStack to smaller / more efficient
/// representation after measuring performance
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
#[derive(Clone)]
pub struct MiniBitGrid {
    queens: MiniGrid,
    beetles: MiniGrid,
    spiders: MiniGrid,
    grasshoppers: MiniGrid,
    ants: MiniGrid,
    pillbugs: MiniGrid,
    ladybugs: MiniGrid,
    mosquitos: MiniGrid,
    all_pieces: MiniGrid,
    outside: MiniGrid,
    white_pieces: MiniGrid,
    black_pieces: MiniGrid,
    stacks: BasicBitStack,
    metainfo: MiniMetaInfo,
}

#[derive(Clone, Debug)]
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

    /// Deterministically chooses a HexLocation that contains at least one piece
    /// on the board. Returns none if the board is empty 
    fn find_one_hex(&self) -> Option<HexLocation> {
        let left = -(BITBOARD_WIDTH as i8);
        let right = BITBOARD_WIDTH as i8; 
        let top = -(BITBOARD_HEIGHT as i8);
        let bottom = BITBOARD_HEIGHT as i8;

        for row in top..=bottom {
            for col in left..=right {
                let hex_location = HexLocation::new(row, col);
                let bit_location: MiniBitGridLocation = hex_location.into();
                if self.top(bit_location).is_some() {
                    return Some(hex_location);
                }
            }
        }
        None
    }

    fn stamp(&mut self, neighborhood: &mut Neighborhood, board_index: usize) {
        debug_assert!(board_index < 4);
        let vertical_index = ( board_index + 2 ) % 4;
        let horizontal_index = board_index ^ 1;
        let diagonal_index = (( board_index + 2 ) % 4) ^ 1;


        let vertical = &mut self.outside[vertical_index];
        *vertical |= *neighborhood.top() | *neighborhood.bottom();
        *vertical &= !self.all_pieces[vertical_index];

        let horizontal = &mut self.outside[horizontal_index];
        *horizontal |= *neighborhood.center_left() | *neighborhood.center_right();
        *horizontal &= !self.all_pieces[horizontal_index];

        let diagonal = &mut self.outside[diagonal_index];
        *diagonal |= *neighborhood.top_right() | *neighborhood.bottom_left();
        *diagonal &= !self.all_pieces[diagonal_index];

        let center = &mut self.outside[board_index];
        *center |= *neighborhood.center();
        *center &= !self.all_pieces[board_index];
    }

    fn update_outside(&mut self) {
        for board_index in 0..4 {
            self.outside[board_index] = AxialBitboard::empty();
        }
        for board_index in 0..4 {
            let mut neighborhood = self.all_pieces[board_index].neighborhood();
            self.stamp(&mut neighborhood, board_index);
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

    /// Returns the piece type at the bottom of a location that contains at least
    /// one piece
    fn piece_type(&self, location: MiniBitGridLocation) -> PieceType {
        debug_assert!(self.presence(location));
        let queen = self.queens[location.board_index];
        let beetle = self.beetles[location.board_index];
        let spider = self.spiders[location.board_index];
        let grasshopper = self.grasshoppers[location.board_index];
        let ant = self.ants[location.board_index];
        let pillbug = self.pillbugs[location.board_index];
        let ladybug = self.ladybugs[location.board_index];
        let mask = location.mask;

        if queen & mask != 0 {
            return PieceType::Queen;
        } else if beetle & mask != 0 {
            return PieceType::Beetle;
        } else if spider & mask != 0 {
            return PieceType::Spider;
        } else if grasshopper & mask != 0 {
            return PieceType::Grasshopper;
        } else if ant & mask != 0 {
            return PieceType::Ant;
        } else if pillbug & mask != 0 {
            return PieceType::Pillbug;
        } else if ladybug & mask != 0 {
            return PieceType::Ladybug;
        } else {
            return PieceType::Mosquito;
        }
    }

    /// Returns the color of the piece at the bottom of the given location
    /// Note: Must have a piece at the location
    fn color(&self, location: MiniBitGridLocation) -> PieceColor {
        debug_assert!(self.presence(location));
        let white = self.white_pieces[location.board_index];
        let mask = location.mask;

        if white & mask != 0 {
            return PieceColor::White;
        } else { 
            return PieceColor::Black;
        }
    }

    fn color_mut(&mut self, color : PieceColor, board_index: usize) -> &mut AxialBitboard {
        match color {
            PieceColor::White => &mut self.white_pieces[board_index],
            PieceColor::Black => &mut self.black_pieces[board_index],
        }
    }

    fn piece_type_mut(&mut self, piece_type : PieceType, board_index: usize) -> &mut AxialBitboard {
        use PieceType::*;
        match piece_type {
            Queen => &mut self.queens[board_index],
            Beetle => &mut self.beetles[board_index],
            Spider => &mut self.spiders[board_index],
            Grasshopper => &mut self.grasshoppers[board_index],
            Ant => &mut self.ants[board_index],
            Pillbug => &mut self.pillbugs[board_index],
            Ladybug => &mut self.ladybugs[board_index],
            Mosquito => &mut self.mosquitos[board_index],
        }
    }

    fn is_empty(&self) -> bool {
        for board in &self.all_pieces {
            if *board != AxialBitboard::empty() {
                return false;
            }
        }
        true
    } 


    /// Checks to see if an addition preserves the invariants of this board.
    /// That is, does the addition follow the One Hive rule?
    fn addition_preserves_one_hive(&self, location : MiniBitGridLocation) -> bool {
        if self.is_empty() {
            return true;
        } 

        if self.presence(location) {
            return true;
        }

        // A single addition that is adjacent to existing pieces necessarily
        // follows the one hive rule...
        // so check all neighbors of given location
        let mut neighbors = AxialBitboard::from_u64(location.mask).neighborhood();

        let center_index = location.board_index;
        let vertical_wrap = |index| (index + 2) % 4;
        let horizontal_wrap = |index| index ^ 1;
        let diagonal_wrap = |index| vertical_wrap(horizontal_wrap(index));
    
        // 1. is there a piece in vertical overflow that is adjacent?
        let board = self.all_pieces[vertical_wrap(center_index)];
        if (board & *neighbors.top() | *neighbors.bottom()) != 0 {
            return true;
        }

        // 2. is there a piece in horizontal overflow that is adjacent?
        let board = self.all_pieces[horizontal_wrap(center_index)];
        if (board & *neighbors.center_left() | *neighbors.center_right()) != 0 {
            return true;
        }

        // 3. is there a piece in diagonal overflow that is adjacent?
        let board = self.all_pieces[diagonal_wrap(center_index)];
        if (board & *neighbors.top_right() | *neighbors.bottom_left()) != 0 {
            return true;
        }

        // 4. is there a piece without overflow that is adjacent?
        let board = self.all_pieces[center_index];
        if (board & *neighbors.center()) != 0 {
            return true;
        }

        // No piece is adjacent to the given location, so the addition is invalid!
        return false;
    }

    /// Returns the stack of all pieces at the given location from bottom to top
    fn peek(&self, loc: MiniBitGridLocation) -> Vec<Piece> {
        let mut pieces = Vec::new();

        if self.presence(loc) == false {
            return pieces;
        }

        let color = self.color(loc);
        let piece_type = self.piece_type(loc);
        pieces.push(Piece::new(piece_type, color));

        for index in self.stacks.find_all(loc.into()) {
            let entry = self.stacks.get(index);
            let piece_type : PieceType = entry.piece().into();
            let color: PieceColor = entry.color().into();
            pieces.push(Piece::new(piece_type, color));
        }


        pieces
    }
    
    /// Returns true if a piece is present at the given location
    /// or there is a stack at the location
    pub fn presence(&self, location: MiniBitGridLocation) -> bool {
        self.all_pieces[location.board_index] & location.mask != 0
    }

    /// Returns the top piece at the given location
    pub fn top(&self, location: MiniBitGridLocation) -> Option<Piece> {
        if self.presence(location) == false {
            debug_assert!(self.stacks.top(location.into()).is_none());
            return None;
        }
        let stack = self.stacks.top(location.into());
        if stack.is_some() {
            return stack
        }

        let color = self.color(location);
        let piece_type = self.piece_type(location);
        Some(Piece::new(piece_type, color))
    }

    /// Adds a piece to the top of the stack at the given location,
    /// addition must preserve the One Hive rule
    pub fn add_top(&mut self, piece: Piece, location: MiniBitGridLocation) {
        assert!(self.addition_preserves_one_hive(location));
        self.add_top_unchecked(piece, location);
    }

    /// Adds a piece to the top of the stack at the given location,
    /// But does not necessarily guarantee that 
    /// the addition preserves the One Hive rule
    fn add_top_unchecked(&mut self, piece: Piece, location: MiniBitGridLocation) {
        use PieceType::*;
        use PieceColor::*;

        let piece_bit = AxialBitboard::from_u64(location.mask);
        let board_index = location.board_index;
        let all_pieces = &mut self.all_pieces[board_index];
        let white_pieces = &mut self.white_pieces[board_index];
        let black_pieces = &mut self.black_pieces[board_index];



        let bottom_piece_exists =  *all_pieces & piece_bit != 0;
        
        if bottom_piece_exists {
            self.stacks.add_piece(piece, location);
        }  else {
            *all_pieces |= piece_bit;
            match piece.color {
                White => *white_pieces |= piece_bit,
                Black => *black_pieces |= piece_bit,
            }
            match piece.piece_type {
                Queen => self.queens[board_index] |= piece_bit,
                Beetle => self.beetles[board_index] |= piece_bit,
                Spider => self.spiders[board_index] |= piece_bit,
                Grasshopper => self.grasshoppers[board_index] |= piece_bit,
                Ant => self.ants[board_index] |= piece_bit,
                Pillbug => self.pillbugs[board_index] |= piece_bit,
                Ladybug => self.ladybugs[board_index] |= piece_bit,
                Mosquito => self.mosquitos[board_index] |= piece_bit,
            }
        }
        
        // TODO: update outside with single bit, for now
        // we just use the whole board
        self.update_outside();
    
        self.metainfo.add_location(location);
    }

    pub fn remove_top(&mut self, location : MiniBitGridLocation) -> Option<Piece> {
        if self.presence(location) == false {
            return None;
        }

        if let Some(_) = self.stacks.find_one(location.into()) {
            return self.stacks.remove_top(location.into());
        }

        let piece = self.top(location);
        let piece_type = piece.unwrap().piece_type;
        let color = piece.unwrap().color;
        let board_index = location.board_index;
        let mask = location.mask;

        self.all_pieces[board_index] &= !mask;
        *self.piece_type_mut(piece_type, board_index) &= !mask;
        *self.color_mut(color, board_index) &= !mask;
        self.update_outside();

        // update metainfo
        if self.is_row_occupied(location) == false {
            self.metainfo.row_presence &= !location.row();
        }  
        if self.is_col_occupied(location) == false {
            self.metainfo.column_presence &= !location.column();
        }

        piece
    }

    /// Returns true if the grid should promote and transfer all
    /// pieces to a larger version of the bit grid.
    ///
    /// Prevents "hazardous overflow", that is, if should_promote() is false,
    /// the pieces on the hive are guaranteed to not wrap around and collide 
    /// with itself.
    pub fn should_promote(&self) -> bool {
        let width_heuristic = BITBOARD_WIDTH * GRID_WIDTH - 1;
        let height_heuristic = BITBOARD_HEIGHT * GRID_HEIGHT - 1;

        self.metainfo.row_presence.count_ones() >=  width_heuristic as u32 ||
        self.metainfo.column_presence.count_ones() >= height_heuristic as u32
    }
}

impl PieceIterator for MiniBitGrid {
    fn pieces(&self) -> Vec<(Vec<Piece>, HexLocation)> {

        // We use the fact that the equivalence of
        // MiniBitGridLocation to HexLocation is closed under adjacency to 
        // convert a set of MiniBitGridLocations to an equivalent set of HexLocations
        //
        // In other words, even though there are multiple valid HexLocations
        // that map to the same MiniBitGridLocation (due to wrapping), we can guarantee a 
        // deterministic conversion by:
        //
        //  1. deterministically choosing a single starting MiniBitGridLocation
        //  2. deterministically converting that to a HexLocation
        //  3. performing deterministic adjacency operations on that HexLocation 
        //  4. converting the resulting HexLocations back to MiniBitGridLocations
        //     (via the FromHex trait), continuing this process until 
        //     we have found all desired locations
        // 
        // This will find a deterministic set of HexLocations that are on or adjacent
        // to our chosen starting MiniBitGridLocation
        

        if self.is_empty() {
            return Vec::new();
        }

        // -> Perform steps (1) and (2)
        let hex = self.find_one_hex().unwrap();


        fn dfs(
            grid: &MiniBitGrid,
            current_loc: HexLocation,
            visited: &mut HashSet<HexLocation>,
            result: &mut Vec<(Vec<Piece>, HexLocation)>,
        ) {
            if visited.contains(&current_loc) {
                return;
            }

            // -> Perform step (4)
            let bit_location : MiniBitGridLocation = current_loc.into(); 

            let pieces = grid.peek(bit_location);
            if pieces.is_empty() {
                return;
            }

            visited.insert(current_loc);
            result.push((pieces, current_loc));

            // -> Perform step (3)
            for direction in Direction::all() {
                let next_loc = current_loc.apply(direction);
                dfs(grid, next_loc, visited, result);
            }
        }

        let mut visited = HashSet::new();
        let mut result = Vec::new();

        // Perform the conversion from our MiniBitGridLocations to HexLocations,
        // we can use depth first search because all piece
        // locations are guaranteed to be connected
        dfs(self, hex, &mut visited, &mut result);

        // First sort by Hexlocation y then by x
        result.sort_by(|a, b| a.1.y.cmp(&b.1.y).then(a.1.x.cmp(&b.1.x)));

        result
        
    }
}

impl std::fmt::Debug for MiniBitGrid {
    fn fmt(&self, f : &mut fmt::Formatter) -> Result<(), fmt::Error>{
        write!(f, "MiniBitGrid all_pieces\n")?;
        for start_board in (0..GRID_SIZE).step_by(GRID_WIDTH).rev() {
            let mut rows = vec![String::new(); BITBOARD_HEIGHT];
            for offset in (0..GRID_WIDTH).rev() {
                let board  = self.all_pieces[start_board + offset];
                for i in (0..BITBOARD_HEIGHT).rev() {
                    for j in (0..BITBOARD_WIDTH).rev() {
                        let index = i * BITBOARD_WIDTH + j;
                        if board.peek(index) {
                            rows[BITBOARD_HEIGHT - i - 1].push_str("■  ");
                        } else {
                            rows[BITBOARD_HEIGHT - i - 1].push_str("□  ");
                        }
                    }
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
        let delta = if self.board_index < 2 { 0 } else { BITBOARD_WIDTH as u32};
        1 << (row + delta)
    }

    /// Converts the set bit and board index to the *number* representing 
    /// its column in the grid. Returns 1 << number.
    pub fn column(&self) -> u16 {
        let column = self.mask.trailing_zeros() % BITBOARD_WIDTH as u32;
        let delta = if self.board_index % 2 == 0 { 0 } else { BITBOARD_WIDTH as u32};
        1 << (column + delta)
    }
}

impl Into<BasicStackLocation> for MiniBitGridLocation {
    fn into(self) -> BasicStackLocation {
        let x = self.mask.trailing_zeros() % BITBOARD_WIDTH as u32;
        let y = self.mask.trailing_zeros() / BITBOARD_HEIGHT as u32;
        BasicStackLocation {
            x: x as u32,
            y: y as u32,
            board_num: self.board_index as u32,
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

/// Promises the compiler that all MiniBitGrids can be converted to a BasicBitGrid
impl BasicBitConvertible for MiniBitGrid {}
/// Promises the compiler that all MiniBitGrids can be converted to a HexGrid
impl HexGridConvertible for MiniBitGrid {} 

impl TryFrom<BasicBitGrid> for MiniBitGrid {
    type Error = &'static str;

    fn try_from(grid: BasicBitGrid) -> Result<Self, Self::Error> {
        let bounds = grid.bounding_box();
        if bounds.is_none() {
            return Ok(MiniBitGrid::new());
        }

        let bounds = bounds.unwrap();
        let size = bounds.width().max(bounds.height());

        if size >= 15 {
            return Err("Cannot convert BasicBitGrid to MiniBitGrid, size too large");
        }

        let mut mini = MiniBitGrid::new();
        for (stack, location) in grid.pieces() {
            let mini_location: MiniBitGridLocation = location.into();
            for piece in stack {
                mini.add_top_unchecked(piece, mini_location);
            }
        }
        // TODO: could do a debug assert to see if one hive rule is preserved?

        Ok(mini)
    }
}

#[cfg(test)]
pub mod tests {
    use crate::hex_grid::HexGrid;
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

    #[test]
    pub fn test_promotion_basic_east() {
        let mut grid = MiniBitGrid::new();
        let mut loc = MiniBitGridLocation::center();
        let dummy_piece = Piece::new(PieceType::Queen, PieceColor::White);

        for _ in 0..(BITBOARD_WIDTH * GRID_WIDTH - 2) {
            grid.add_top(dummy_piece, loc);
            loc = loc.shift_east();
            assert!(!grid.should_promote());
        }

        // Note: After this addition, the "outside" metadata would wrap and collide 
        // with itself and yield invalid results. Be sure should_promote() 
        // returns true!
        grid.add_top(dummy_piece, loc);
        assert!(grid.should_promote());
    }

    #[test]
    pub fn test_promotion_basic_west() {
        let mut grid = MiniBitGrid::new();
        let mut loc = MiniBitGridLocation::center();
        let dummy_piece = Piece::new(PieceType::Queen, PieceColor::White);

        for _ in 0..(BITBOARD_WIDTH * GRID_WIDTH - 2) {
            grid.add_top(dummy_piece, loc);
            loc = loc.shift_west();
            assert!(!grid.should_promote());
        }

        // Note: After this addition, the "outside" metadata would wrap and collide 
        // with itself and yield invalid results. Be sure should_promote() 
        // returns true!
        grid.add_top(dummy_piece, loc);
        assert!(grid.should_promote());
    }

    #[test]
    pub fn test_promotion_basic_nw() {
        let mut grid = MiniBitGrid::new();
        let mut loc = MiniBitGridLocation::center();
        let dummy_piece = Piece::new(PieceType::Queen, PieceColor::White);

        for _ in 0..(BITBOARD_WIDTH * GRID_WIDTH - 2) {
            grid.add_top(dummy_piece, loc);
            loc = loc.shift_northwest();
            assert!(!grid.should_promote());
        }

        // Note: After this addition, the "outside" metadata would wrap and collide 
        // with itself and yield invalid results. Be sure should_promote() 
        // returns true!
        grid.add_top(dummy_piece, loc);
        assert!(grid.should_promote());
    }

    #[test]
    pub fn test_promotion_basic_sw() {
        let mut grid = MiniBitGrid::new();
        let mut loc = MiniBitGridLocation::center();
        let dummy_piece = Piece::new(PieceType::Queen, PieceColor::White);

        for _ in 0..(BITBOARD_WIDTH * GRID_WIDTH - 2) {
            grid.add_top(dummy_piece, loc);
            loc = loc.shift_southwest();
            assert!(!grid.should_promote());
        }

        // Note: After this addition, the "outside" metadata would wrap and collide 
        // with itself and yield invalid results. Be sure should_promote() 
        // returns true!
        grid.add_top(dummy_piece, loc);
        assert!(grid.should_promote());
    }

    #[test]
    pub fn test_promotion_basic_ne() {
        let mut grid = MiniBitGrid::new();
        let mut loc = MiniBitGridLocation::center();
        let dummy_piece = Piece::new(PieceType::Queen, PieceColor::White);

        for _ in 0..(BITBOARD_WIDTH * GRID_WIDTH - 2) {
            grid.add_top(dummy_piece, loc);
            loc = loc.shift_northeast();
            assert!(!grid.should_promote());
        }

        // Note: After this addition, the "outside" metadata would wrap and collide 
        // with itself and yield invalid results. Be sure should_promote() 
        // returns true!
        grid.add_top(dummy_piece, loc);
        assert!(grid.should_promote());
    }

    #[test]
    pub fn test_promotion_basic_se() {
        let mut grid = MiniBitGrid::new();
        let mut loc = MiniBitGridLocation::center();
        let dummy_piece = Piece::new(PieceType::Queen, PieceColor::White);

        for _ in 0..(BITBOARD_WIDTH * GRID_WIDTH - 2) {
            grid.add_top(dummy_piece, loc);
            loc = loc.shift_southeast();
            assert!(!grid.should_promote());
        }

        // Note: After this addition, the "outside" metadata would wrap and collide 
        // with itself and yield invalid results. Be sure should_promote() 
        // returns true!
        grid.add_top(dummy_piece, loc);
        assert!(grid.should_promote());
    }

    #[test]
    pub fn test_top() {
        let mut grid = MiniBitGrid::new();
        let loc = MiniBitGridLocation::center();
        let queen = Piece::new(PieceType::Queen, PieceColor::White);
        let mosquito = Piece::new(PieceType::Mosquito, PieceColor::Black);
        let beetle = Piece::new(PieceType::Beetle, PieceColor::Black);

        assert_eq!(grid.top(loc), None);
        grid.add_top(queen, loc);
        assert_eq!(grid.top(loc).unwrap(), queen);
        grid.add_top(mosquito, loc);
        assert_eq!(grid.top(loc).unwrap(), mosquito);
        grid.add_top(beetle, loc);
        assert_eq!(grid.top(loc).unwrap(), beetle);
        grid.add_top(mosquito, loc);
        grid.remove_top(loc);
        grid.add_top(mosquito, loc);
        assert_eq!(grid.top(loc).unwrap(), mosquito);
        grid.add_top(beetle, loc);
        assert_eq!(grid.top(loc).unwrap(), beetle);
        grid.add_top(beetle, loc);
        assert_eq!(grid.top(loc).unwrap(), beetle);
        grid.add_top(beetle, loc);
        assert_eq!(grid.top(loc).unwrap(), beetle);

        let removed = grid.remove_top(loc);
        assert_eq!(removed.unwrap(), beetle);
        assert_eq!(grid.top(loc).unwrap(), beetle);

        grid.remove_top(loc);
        grid.remove_top(loc);
        assert_eq!(grid.top(loc).unwrap(), mosquito);
        let removed = grid.remove_top(loc);
        assert_eq!(removed.unwrap(), mosquito);

        grid.remove_top(loc);
        grid.remove_top(loc);
        assert_eq!(grid.top(loc).unwrap(), queen);
        grid.remove_top(loc);
        assert_eq!(grid.top(loc), None);
        assert!(grid.remove_top(loc).is_none());
    }

    #[test]
    pub fn test_promotion_basic_remove() {
        let mut grid = MiniBitGrid::new();
        let mut loc = MiniBitGridLocation::center();
        let dummy_piece = Piece::new(PieceType::Queen, PieceColor::White);

        for _ in 0..(BITBOARD_WIDTH * GRID_WIDTH - 2) {
            grid.add_top(dummy_piece, loc);
            loc = loc.shift_southwest();
            assert!(!grid.should_promote());
        }

        // Note: After this addition, the "outside" metadata would wrap and collide 
        // with itself and yield invalid results. Be sure should_promote() 
        // returns true!
        grid.add_top(dummy_piece, loc);
        assert!(grid.should_promote());

        // .. and it should be rectified after removing the piece
        grid.remove_top(loc);
        assert!(!grid.should_promote());
    }

    #[test]
    pub fn test_conversion() {
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

        let basic : BasicBitGrid = hex_grid.clone().into();
        let mini : MiniBitGrid = basic.clone().try_into().unwrap();

        
        println!("-----------------");
        for stack in mini.pieces() {
            if stack.0.len() > 1 {
                println!("mini {:#?}", stack);
            }
        }
        println!("-----------------");
        for stack in basic.pieces() {
            if stack.0.len() > 1 {
                println!("basic {:#?}", stack);
            }
        }

        assert_eq!(basic.pieces(), mini.pieces());
        assert_eq!(basic, mini);

        let converted_basic : BasicBitGrid = mini.clone().into();
        assert_eq!(basic, converted_basic);

        let converted_hex : HexGrid = mini.into();
        assert_eq!(hex_grid, converted_hex);
    }
}
