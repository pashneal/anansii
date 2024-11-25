use thiserror::Error;
use crate::hex_grid::*;

pub type Result<T> = std::result::Result<T, InterpreterError>;

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("Parse error: {0}")]
    ParseError(String),
}

/// Domain specific language interpreter for HexGrids
///
/// The idea is to take a string of a format such as the following, and to 
/// interpret it deterministically as a HexGrid:
///
/// ```
///  . . . . .
///   . Q 3 g .
///  . . A b .
///   . 2 . m .
///  . . . . .
///
///  start - [ 3, -2 ]
///
///  3 - [G b B]
///  2 - [a M]
///
/// ```
///
/// TODO: concrete specs for DSL, for now it's vibes
///
struct Parser {
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BoardInput {
    Piece(Piece),
    Stack,
}

impl Parser {

    /// Given a HexGrid board string, interpret it as a set of board inputs 
    /// with locations arbitrarily assigned, but correct relative to each other. 
    ///
    /// In other words, while the HexLocation may be incorrect, 
    /// all directions and distances between pieces should be
    /// correct.
    ///
    /// If an interpretation is not possible, return an error.
    pub fn parse_board(input: &str) ->  Result<Vec<(BoardInput, HexLocation)>> {
        Err(InterpreterError::ParseError("Not implemented".to_string()))
    }
}


#[test]
pub fn test_board_parse_simple() {
    use PieceType::*;
    use PieceColor::*;

    let board_string = concat!(
        ". . . . .\n",
        " . Q 3 g .\n",
        ". . A b .\n",
        " . 2 . m .\n",
        ". . . . .\n",
    );

    let result = Parser::parse_board(board_string);
    assert!(result.is_ok());

    let board = result.unwrap();
    assert_eq!(board.len(), 7);
    let white_queen = board.iter().find(|(input, _)| {
        match input {
            BoardInput::Piece(piece) => *piece == Piece::new(Queen, White), 
            _ => false,
        }
    });

    assert!(white_queen.is_some(), "Couldn't find white queen on board");
    let (_, white_queen_loc) = white_queen.unwrap();
    let stack_3_loc = white_queen_loc.apply(Direction::E);
    let black_grasshopper_loc = stack_3_loc.apply(Direction::E);
    let white_ant_loc = white_queen_loc.apply(Direction::SE);
    let black_beetle_loc = white_ant_loc.apply(Direction::E);
    let stack_2_loc = white_ant_loc.apply(Direction::SW);
    let white_mosquito_loc = black_beetle_loc.apply(Direction::SE);

    let stack_3 = BoardInput::Stack;
    let stack_2 = BoardInput::Stack;
    let black_grasshopper = BoardInput::Piece(Piece::new(Grasshopper, Black));
    let white_ant = BoardInput::Piece(Piece::new(Ant, White));
    let black_beetle = BoardInput::Piece(Piece::new(Beetle, Black));
    let white_mosquito = BoardInput::Piece(Piece::new(Mosquito, White));

    assert!(board.contains(&(stack_3, stack_3_loc)));
    assert!(board.contains(&(stack_2, stack_2_loc)));
    assert!(board.contains(&(black_grasshopper, black_grasshopper_loc)));
    assert!(board.contains(&(white_ant, white_ant_loc)));
    assert!(board.contains(&(black_beetle, black_beetle_loc)));
    assert!(board.contains(&(white_mosquito, white_mosquito_loc)));
}


#[test]
pub fn test_board_parse_empty() {
    let board_string = concat!(
        ". . . . .\n",
        " . . . . .\n",
        ". . . . .\n",
        " . . . . .\n",
        ". . . . .\n",
    );

    let result = Parser::parse_board(board_string);
    assert!(result.is_ok());
    let board = result.unwrap();
    assert_eq!(board.len(), 0);
}

#[test]
pub fn test_board_parse_single() {
    use PieceType::*;
    use PieceColor::*;

    let board_string = concat!(
        " . . . . .\n",
        ". . . . .\n",
        " A . . . .\n",
    );

    let result = Parser::parse_board(board_string);
    assert!(result.is_ok());
    let board = result.unwrap();
    assert_eq!(board.len(), 1);
    let (input, _) = board[0];
    assert_eq!(input, BoardInput::Piece(Piece::new(Ant, White)));
}

#[test]
pub fn test_board_parse_fitted() {
    use PieceType::*;
    use PieceColor::*;
    let board_string = concat!(
        " . l m . 4\n",
        ". q . g 2\n",
        " A Q b 3 P\n",
    );

    let result = Parser::parse_board(board_string);
    assert!(result.is_ok());

    let board = result.unwrap();
    assert_eq!(board.len(), 11);
    let white_ant = board.iter().find(|(input, _)| {
        match input {
            BoardInput::Piece(piece) => *piece == Piece::new(Ant, White), 
            _ => false,
        }
    });

    assert!(white_ant.is_some(), "Couldn't find white ant on board");
    let (_, white_ant_loc) = white_ant.unwrap();
    let white_queen_loc = white_ant_loc.apply(Direction::E);
    let black_beetle_loc = white_queen_loc.apply(Direction::E);
    let stack_3_loc = black_beetle_loc.apply(Direction::E); 
    let white_pillbug_loc = stack_3_loc.apply(Direction::E);
    let black_queen_loc = white_ant_loc.apply(Direction::NE);
    let black_ladybug_loc = black_queen_loc.apply(Direction::NE);
    let black_mosquito_loc = black_ladybug_loc.apply(Direction::E);
    let stack_4_loc = black_mosquito_loc.apply(Direction::E).apply(Direction::E);
    let stack_2_loc = white_queen_loc.apply(Direction::SW);
    let black_grasshopper_loc = stack_2_loc.apply(Direction::W);


    let stack_4 = BoardInput::Stack;
    let stack_3 = BoardInput::Stack;
    let stack_2 = BoardInput::Stack;

    let white_queen = BoardInput::Piece(Piece::new(Queen, White));
    let black_beetle = BoardInput::Piece(Piece::new(Beetle, Black));
    let white_pillbug = BoardInput::Piece(Piece::new(Pillbug, White));
    let black_queen = BoardInput::Piece(Piece::new(Queen, Black));
    let black_ladybug = BoardInput::Piece(Piece::new(Ladybug, Black));
    let black_mosquito = BoardInput::Piece(Piece::new(Mosquito, Black));
    let black_grasshopper = BoardInput::Piece(Piece::new(Grasshopper, Black));

    assert!(board.contains(&(stack_4, stack_4_loc)));
    assert!(board.contains(&(stack_3, stack_3_loc)));
    assert!(board.contains(&(stack_2, stack_2_loc)));
    assert!(board.contains(&(white_queen, white_queen_loc)));
    assert!(board.contains(&(black_beetle, black_beetle_loc)));
    assert!(board.contains(&(white_pillbug, white_pillbug_loc)));
    assert!(board.contains(&(black_queen, black_queen_loc)));
    assert!(board.contains(&(black_ladybug, black_ladybug_loc)));
    assert!(board.contains(&(black_mosquito, black_mosquito_loc)));
    assert!(board.contains(&(black_grasshopper, black_grasshopper_loc)));

}
