use crate::generator::debug::*;
use crate::generator::*;

use crate::hex_grid::*;
use crate::piece::*;
use crate::uhp::*;
use std::collections::HashSet;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GameDebuggerError {
    #[error("Positions could not convert to legal annotations, {0}")]
    AnnotationError(UHPError),
}

pub type Result<T> = std::result::Result<T, GameDebuggerError>;

/// Represents a game of Hive.
///
/// Invariant:
/// - all moves in the move history of this GameDebugger are legal
///
/// Why?:
/// So that optimized code does not need to concern itself with
/// simplicity, visual representation, slow debugging methods,
/// or extremely complicated testing harnesses - a hard lesson learned
/// from the first version of this project. It leaves optimized code to more easily
/// be iterated on.
#[derive(Clone, Debug)]
pub struct GameDebugger {
    annotations: Vec<Annotator>,
    generator: PositionGeneratorDebugger,
    game_type: GameType,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GameResult {
    WhiteWins,
    BlackWins,
    Draw,
}

impl GameDebugger {
    pub fn new(moves: &[String], game_type: GameType) -> Result<Self> {
        let annotator = Annotator::new();
        let annotations = vec![annotator];
        let mut game = GameDebugger {
            annotations,
            generator: PositionGeneratorDebugger::new(game_type),
            game_type,
        };

        for mv in moves.iter() {
            game.make_move(mv)?;
        }
        Ok(game)
    }

    /// Undoes the last move made in the game if possible
    pub fn undo_move(&mut self) -> Result<()> {
        if self.annotations.len() == 1 {
            return Err(GameDebuggerError::AnnotationError(UHPError::UndoError));
        }
        self.annotations.pop();
        let last_move = self.annotations.last().unwrap().last_move();
        self.generator = PositionGeneratorDebugger::from_hex_grid(
            self.annotations.last().unwrap().current_position(),
            self.game_type,
            last_move,
        );
        Ok(())
    }

    /// Makes a legal UHP move from the UHP-compatible string passed in
    pub fn make_move(&mut self, move_string: &str) -> Result<()> {
        let mut annotator = self.annotations.last().unwrap().clone();
        annotator = annotator
            .next_uhp_move(move_string)
            .map_err(GameDebuggerError::AnnotationError)?;

        self.append_position(annotator.current_position())
    }

    pub fn get_player_to_move(&self) -> PieceColor {
        match self.annotations.len() % 2 {
            1 => PieceColor::White,
            _ => PieceColor::Black,
        }
    }

    /// Adds a new position to the game, assuming it arrived after a legal move
    pub fn append_position<P: Position>(&mut self, position: &P) -> Result<()> {
        let grid = position.to_hex_grid();

        if !self.legal_positions().contains(&grid) {
            return Err(GameDebuggerError::AnnotationError(UHPError::IllegalMove {
                info: format!(
                    "\nPrevious position:\n{}\nLatest position:\n{}",
                    self.current_position().to_dsl(),
                    grid.to_dsl()
                ),
            }));
        }

        let annotator = self.annotations.last().unwrap().clone();
        let annotator = annotator
            .next_state(&grid)
            .map_err(GameDebuggerError::AnnotationError)?;

        self.generator = PositionGeneratorDebugger::from_hex_grid(
            annotator.current_position(),
            self.game_type,
            annotator.last_move(),
        );
        self.annotations.push(annotator);

        Ok(())
    }

    /// Returns a set of legal positions that can occur after 
    /// the current game state
    pub fn legal_positions(&mut self) -> HashSet<HexGrid> {
        // If the game is over, no legal moves
        match self.game_result() {
            Some(_) => HashSet::new(),
            _ => self.generator.generate_positions_for(self.get_player_to_move()),
        }
    }

    /// If the game is over, returns the result of the game.
    /// Otherwise, returns None.
    pub fn game_result(&self) -> Option<GameResult> {
        use PieceColor::*;
        use PieceType::*;
        let annotator = self.annotations.last().unwrap();
        let grid = annotator.current_position();

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
            if annotator.current_position() == grid {
                position_count += 1;
            }
            if position_count > 2 {
                return Some(GameResult::Draw);
            }
        }

        None
    }

    /// Get the latest position in the game
    pub fn current_position(&self) -> &HexGrid {
        self.annotations.last().unwrap().current_position()
    }
}

#[cfg(test)]
mod tests {
    use super::GameDebugger;
    use super::*;

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

        let mut game = GameDebugger::new(&[], GameType::MLP).unwrap();
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

        let mut game = GameDebugger::new(&[], GameType::MLP).unwrap();
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

        let game = GameDebugger::new(&draw, GameType::MLP).unwrap();
        println!("game\n:{}", game.current_position().to_dsl());
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

        let game = GameDebugger::new(&draw, GameType::MLP).unwrap();
        println!("game\n:{}", game.current_position().to_dsl());
        assert_eq!(game.game_result(), Some(GameResult::Draw));
    }
}
