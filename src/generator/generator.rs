use crate::hex_grid::{HexGrid, HexLocation, Shiftable, FromHexLocation};
use crate::location::Direction;
use crate::piece::{IntoPieces, Piece, PieceColor, PieceType, PIECE_COUNTS};
use crate::uhp::GameType;
use std::collections::HashSet;

pub trait FromHexGrid {
    fn from_hex_grid(
        grid: &HexGrid,
        game_type: GameType,
        previous_change: Option<HexLocation>,
    ) -> Self;

    fn from_default(grid: &HexGrid) -> Self
    where
        Self: Sized,
    {
        Self::from_hex_grid(grid, GameType::default(), None)
    }
}

pub trait MoveGenerator: FromHexGrid {
    type Position: IntoPieces;
    type PieceLocation: Shiftable + FromHexLocation;

    /// Returns a list of all possible moves for a spider at a given location
    /// if the spider is not covered by any other pieces and is on the ground.
    /// (ignores pillbug swaps)
    fn spider_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }

    /// Returns a list of all possible moves for a grasshopper at a given location
    /// if the grasshopper is not covered by any other pieces and is on the ground.
    /// (ignores pillbug swaps)
    fn grasshopper_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }

    /// Returns a list of all possible moves for a queen at a given location
    /// if the queen is not covered by any other pieces and is on the ground.
    /// (ignores pillbug swaps)
    fn queen_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }

    /// Returns a list of all possible moves for an ant at a given location
    /// if the ant is not covered by any other pieces and is on the ground.
    /// (ignores pillbug swaps)
    fn ant_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }

    /// Returns a list of all possible moves for a beetle at a given location
    /// if the beetle is not covered by any other pieces.
    /// (ignores pillbug swaps)
    fn beetle_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }

    /// Returns a list of all possible moves for a ladybug at a given location
    /// if the ladybug is not covered by any other pieces and is on the ground.
    /// (ignores pillbug swaps)
    fn ladybug_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }

    /// Returns a list of all possible moves for a pillbug at a given location
    /// if the pillbug is not covered by any other pieces. Returns an empty 
    /// list if the pillbug is not on the ground.
    /// (ignores pillbug swaps)
    fn pillbug_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }

    /// Returns a list of all possible moves for a mosquito at a given location
    /// if the mosquito is not covered by any other pieces. 
    /// (ignores pillbug swaps)
    fn mosquito_moves(&mut self, location: Self::PieceLocation) -> Vec<Self::Position> {
        unimplemented!();
    }
}

pub trait PlacementGenerator<I: IntoPieces>: FromHexGrid {



    /// Returns positions that follow the typical placement rules for a given
    /// color. These are all locations which are:
    ///  1) adjacent to some piece on the hive
    ///  2) not adjacent to a piece of the opposite color
    ///  3) unoccupied
    ///
    /// If the board has no pieces, placement occurs at the center HexLocation
    /// If the board has one piece, placement only needs follow rule 1
    fn placements(&self, placing_color: PieceColor) -> Vec<I::PieceLocation>;

    fn current_grid(&self) -> I;
}

pub trait SwapGenerator: FromHexGrid {
    type Position: IntoPieces;
    /// Returns a list of all positions with each possible swap applied to adjacent pieces by
    /// a top-facing pillbug at a given *location*.
    ///
    /// Adjacent pieces that are not allowed to be swapped are:
    ///
    /// - a piece at the specified *immobilized* location
    /// - pieces in a stack of height > 1
    /// - pieces whose absence would violate the One Hive Rule
    /// - pieces that must pass through a gate of height > 1 to slide on/off the top of the pillbug
    ///
    /// Additionally, the pillbug is not able to swap if it is at the immobilized
    /// location. Returns an empty list if the piece at the pillbug_location is not on the ground.
    fn pillbug_swaps(
        &mut self,
        pillbug_location: HexLocation,
        immobilized: Option<HexLocation>,
    ) -> Vec<Self::Position> {
        unimplemented!();
    }
}

pub trait PositionGenerator<I: IntoPieces>:
    MoveGenerator<Position = I> + PlacementGenerator<I> + SwapGenerator<Position = I>
{
    /// Returns the legal positions reachable from the current board state
    /// as if it is the turn of the specified color.
    fn generate_positions_for(&mut self, color: PieceColor) -> HashSet<I>;
}

