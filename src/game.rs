use crate::hex_grid::HexGrid;
use crate::piece::*;
use crate::uhp::*;
use std::marker::PhantomData;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GameDebuggerError {
    #[error("Positions could not convert to legal annotations, {0}")]
    AnnotationError(UHPError),
}

pub type Result<T> = std::result::Result<T, GameDebuggerError>;

/// Represents a game of Hive with only legal moves taken and positions resulted
/// from legal moves.
///
/// It is intended that game positions, moves, locations and other game state
/// information can be translated to (and from) this struct.
///
/// This means optimized code does not need to concern itself with
/// simplicity, visual representation, slow debugging methods,
/// or extremely complicated testing harnesses - a hard lesson learned
/// from the first version of this project. It leaves optimized code to more easily
pub struct GameDebugger {
    /// All Hive positions in order arrived through the game but
    /// through the lens of the Annotator which translates
    /// moves to human-readable formats
    annotations: Vec<Annotator>,
}

pub enum GameResult {
    WhiteWins,
    BlackWins,
    Draw,
}

impl GameResult {
    pub fn to_str(&self) -> &str {
        use GameResult::*;
        match self {
            WhiteWins => "WhiteWins",
            BlackWins => "BlackWins",
            Draw => "Draw",
        }
    }
}

impl GameDebugger {

    /// Makes a legal UHP move from the UHP-compatible string passed in
    pub fn make_move(&mut self, move_string: &str) -> Result<()> {
        let mut annotator = self.annotations.last().unwrap().clone();
        annotator = annotator
            .next_uhp_move(&move_string)
            .map_err(|e| GameDebuggerError::AnnotationError(e))?;
        self.annotations.push(annotator);
        Ok(())
    }

    /// Adds a new position to the game, assuming it arrived from a legal move
    /// from the last position stored in the game.
    pub fn append_position<P: Position>(&mut self, position: &P) -> Result<()> {
        let mut annotator = self.annotations.last().unwrap().clone();
        let grid = position.to_hex_grid();
        annotator = annotator
            .next_state(&grid)
            .map_err(|e| GameDebuggerError::AnnotationError(e))?;
        self.annotations.push(annotator);

        Ok(())
    }

    /// Given all positions arrived at within the game create
    /// and return a GameDebugger with the positions.
    pub fn from_positions<P: Position>(positions: &Vec<P>) -> Result<Self> {
        let annotator = Annotator::new();
        let annotations = vec![annotator];
        let mut game = GameDebugger { annotations };

        // Must begin with the empty board
        debug_assert!(positions.len() > 0 && positions[0].to_hex_grid() == HexGrid::new());

        for pos in positions.iter() {
            game.append_position(pos)?;
        }

        Ok(game)
    }

    /// If the game is over, returns the result of the game.
    /// Otherwise, returns None.
    pub fn game_result(&self) -> Option<GameResult> {
        use PieceColor::*;
        use PieceType::*;
        let annotator = self.annotations.last().unwrap();
        let grid = annotator.position();

        let white_queen = grid.find(Piece::new(Ladybug, White));
        let black_queen = grid.find(Piece::new(Ladybug, Black));

        if let Some((queen_loc, _)) = white_queen {
            if grid.get_neighbors(queen_loc).len() == 6 {
                return Some(GameResult::BlackWins);
            }
        }

        if let Some((queen_loc, _)) = black_queen {
            if grid.get_neighbors(queen_loc).len() == 6 {
                return Some(GameResult::WhiteWins);
            }
        }

        let mut position_count = 0;
        for annotator in self.annotations.iter() {
            if position_count > 2 {
                return Some(GameResult::Draw);
            }
            if annotator.position() == grid {
                position_count += 1;
            }
        }

        None
    }
}

pub trait Position {
    fn new() -> Self;
    fn to_hex_grid(&self) -> HexGrid;
    fn from_hex_grid(&self, grid: &HexGrid) -> Self;
}

#[test]
pub fn test_win() {
    //TODO
}

#[test]
pub fn test_draw() {
    //TODO
}
