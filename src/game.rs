use crate::hex_grid::*;
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
/// be iterated on.
pub struct GameDebugger {
    /// All Hive positions in order arrived through the game but
    /// through the lens of the Annotator which translates
    /// moves to human-readable formats
    annotations: Vec<Annotator>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    /// Give a list of legal UHP moves starting from the empty board,
    /// create and return a GameDebugger with the moves accounted for.
    pub fn from_moves(moves: &[String]) -> Result<Self> {
        let annotator = Annotator::new();
        let annotations = vec![annotator];
        let mut game = GameDebugger { annotations };

        for mv in moves.iter() {
            game.make_move(mv)?;
        }

        Ok(game)
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

        let white_queen = grid.find(Piece::new(Queen, White));
        let black_queen = grid.find(Piece::new(Queen, Black));

        let queen_surrounded = |(queen_loc, _): (HexLocation, _)| -> bool {
            let neighbors = grid.get_neighbors(queen_loc);
            neighbors.len() == 6
        };

        let white_queen_surrounded = white_queen.map(queen_surrounded).unwrap_or(false);
        let black_queen_surrounded = black_queen.map(queen_surrounded).unwrap_or(false);

        match (white_queen_surrounded, black_queen_surrounded) {
            (true, false) => return Some(GameResult::BlackWins),
            (false, true) => return Some(GameResult::WhiteWins),
            (true, true) => return Some(GameResult::Draw),
            _ => {}
        }

        let mut position_count = 0;
        for annotator in self.annotations.iter() {
            if annotator.position() == grid {
                position_count += 1;
            }
            if position_count > 2 {
                return Some(GameResult::Draw);
            }
        }

        None
    }

    /// Returns the locations in the hive that are "pinned",
    /// in other words, removing the pieces in that stack would violate the One Hive rule
    ///
    /// returns in board order, that is, first top-to-bottom then left-to-right
    pub fn pinned(&self) -> Vec<HexLocation> {
        todo!()
    }

    /// Get the latest position in the game
    pub fn position(&self) -> &HexGrid {
        self.annotations.last().unwrap().position()
    }
}

pub trait Position {
    fn new() -> Self;
    fn to_hex_grid(&self) -> HexGrid;
    fn from_hex_grid(grid: &HexGrid) -> Self;
}

#[test]
pub fn pinned_pieces() {}

#[test]
pub fn test_win() {
    let white_wins = [
        String::from(r"wP"),
        String::from(r"bL wP-"),
        String::from(r"wB1 \wP"),
        String::from(r"bQ bL/"),
        String::from(r"wA1 /wB1"),
        String::from(r"bA1 \bQ"),
        String::from(r"wQ wA1\"),
        String::from(r"bB1 bQ/"),
        String::from(r"wB1 wP"),
        String::from(r"bG1 bB1\"),
        String::from(r"wA1 bQ\"),
        String::from(r"bG2 bG1/"),
        String::from(r"wB1 \bL"),
    ];

    let mut game = GameDebugger::from_positions(&vec![HexGrid::new()]).unwrap();
    for pos in white_wins.iter() {
        assert!(game.game_result().is_none());
        game.make_move(pos).unwrap();
    }
    assert_eq!(game.game_result(), Some(GameResult::WhiteWins));

    let black_wins = [
        String::from(r"wP"),
        String::from(r"bL wP-"),
        String::from(r"wB1 \wP"),
        String::from(r"bQ bL/"),
        String::from(r"wA1 /wB1"),
        String::from(r"bA1 \bQ"),
        String::from(r"wQ wA1\"),
        String::from(r"bA2 bQ/"),
        String::from(r"wB1 wP"),
        String::from(r"bA1 /wA1"),
        String::from(r"wB1 wP\"),
        String::from(r"bA2 bA1\"),
        String::from(r"wA2 \wP"),
        String::from(r"bA3 bQ\"),
        String::from(r"wA2 wQ\"),
    ];

    let mut game = GameDebugger::from_positions(&vec![HexGrid::new()]).unwrap();
    for move_ in black_wins.iter() {
        assert!(game.game_result().is_none());
        game.make_move(move_).unwrap();
    }
    assert_eq!(game.game_result(), Some(GameResult::BlackWins));
}

#[test]
pub fn test_draw() {
    // Test three fold repetition
    let draw = [
        String::from(r"wA1"),
        String::from(r"bA1 wA1-"),
        String::from(r"wQ -wA1"),
        String::from(r"bQ bA1-"),
        String::from(r"wQ \wA1"),
        String::from(r"bQ bA1/"),
        String::from(r"wQ -wA1"),
        String::from(r"bQ bA1-"),
        String::from(r"wQ /wA1"),
        String::from(r"bQ bA1\"),
        String::from(r"wQ -wA1"),
        String::from(r"bQ bA1-"),
    ];

    let game = GameDebugger::from_moves(&draw).unwrap();
    println!("game\n:{}", game.position().to_dsl());
    assert_eq!(game.game_result(), Some(GameResult::Draw));
}

#[test]
pub fn test_double_surround_draw() {
    // Test both queens surrounded at same time
    let draw = [
        String::from(r"wB1"),
        String::from(r"bB1 wB1-"),
        String::from(r"wG1 \wB1"),
        String::from(r"bG1 bB1/"),
        String::from(r"wM /wG1"),
        String::from(r"bB2 bG1\"),
        String::from(r"wQ wM\"),
        String::from(r"bQ bB1\"),
        String::from(r"wA1 wG1/"),
        String::from(r"bL bB2/"),
        String::from(r"wA1 /wM"),
        String::from(r"bL bB2\"),
        String::from(r"wA2 \wM"),
        String::from(r"bA1 bB2/"),
        String::from(r"wB2 wA1\"),
        String::from(r"bG2 bQ\"),
        String::from(r"wA1 wQ\"),
        String::from(r"bA1 /bQ"),
        String::from(r"wA2 \wB2"),
        String::from(r"bG1 wB1\"),
    ];

    let game = GameDebugger::from_moves(&draw).unwrap();
    println!("game\n:{}", game.position().to_dsl());
    assert_eq!(game.game_result(), Some(GameResult::Draw));
}
