use crate::hex_grid::*;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, ParserError>;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Could not parse board row: {0}")]
    RowError(String),
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
struct Parser {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BoardInput {
    Piece(Piece),
    Stack,
    Empty,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Alignment {
    /// Row is flush with the left side of the input
    Standard,
    /// Row is shifted to the right by one space
    Shifted,
}

impl Parser {
    /// Parses a row of dots, numbers, or letters and attempts
    /// to convert to BoardInputs and Alignment.
    /// Numbers are convertered to Stacks, letters to Pieces, and dots to Empty.
    ///
    /// The board row must follow the DSL specification.
    fn parse_row(row: &str) -> Result<(Vec<BoardInput>, Alignment)> {
        use ParserError::*;
        let mut alignment = Alignment::Standard;
        let mut board_inputs = Vec::new();
        let mut space_count = 0; // Consecutive # of spaces
        let mut piece_count = 0; // Consecutive # of pieces

        for input in row.chars() {
            match input {
                '.' => {
                    board_inputs.push(BoardInput::Empty);
                    space_count = 0;
                }
                '0'..='9' => {
                    if piece_count > 0 {
                        return Err(RowError("Invalid stack placement".to_string()));
                    }

                    board_inputs.push(BoardInput::Stack);
                    space_count = 0;
                    piece_count = 1;
                }
                'a'..='z' => {
                    if piece_count > 0 {
                        return Err(RowError("Invalid piece placement".to_string()));
                    }

                    let piece_type = PieceType::try_from_char(&input);
                    if piece_type.is_err() {
                        return Err(RowError("Invalid piece type found".to_string()));
                    }

                    let piece = Piece::new(piece_type.unwrap(), PieceColor::Black);
                    board_inputs.push(BoardInput::Piece(piece));

                    space_count = 0;
                    piece_count = 1;
                }

                'A'..='Z' => {
                    if piece_count > 0 {
                        return Err(RowError("Invalid piece placement".to_string()));
                    }

                    let piece_type = PieceType::try_from_char(&input);
                    if piece_type.is_err() {
                        return Err(RowError("Invalid piece type found".to_string()));
                    }

                    let piece = Piece::new(piece_type.unwrap(), PieceColor::White);
                    board_inputs.push(BoardInput::Piece(piece));

                    space_count = 0;
                    piece_count = 1;
                }
                ' ' => {
                    // Space must be 0
                    // if board is empty, aligment is shifted
                    if space_count > 0 {
                        return Err(RowError("Invalid space placement".to_string()));
                    }
                    if board_inputs.is_empty() {
                        alignment = Alignment::Shifted;
                    }

                    space_count += 1;
                    piece_count = 0;
                }
                _ => return Err(RowError(format!("Invalid character: {}", input))),
            }
        }

        Ok((board_inputs, alignment))
    }

    /// Given a HexGrid board string according to the DSL specication,
    /// interpret it as a set of board inputs with locations
    /// arbitrarily assigned, but correct relative to each other.
    ///
    /// In other words, while the HexLocation may be incorrect,
    /// all directions and distances between pieces should be
    /// correct.
    ///
    /// If an interpretation is not possible, return an error.
    pub fn parse_board(input: &str) -> Result<Vec<(BoardInput, HexLocation)>> {
        let input = input.trim_end();
        let mut result = Vec::new();
        let mut row_size: Option<usize> = None;
        let mut last_alignment = None;
        let mut first_row_alignment = None;

        for (y, row) in input.lines().enumerate() {
            let (board_inputs, row_alignment) = Parser::parse_row(row)?;
            match first_row_alignment {
                None => first_row_alignment = Some(row_alignment),
                Some(alignment) => {}
            }

            // Be sure row size is consitent throughout the board
            match row_size {
                None => row_size = Some(board_inputs.len()),
                Some(size) => {
                    if size != board_inputs.len() {
                        return Err(ParserError::ParseError(
                            "Row lengths do not match".to_string(),
                        ));
                    }
                }
            }

            // Be sure row alignment is alternating throughout the board
            match last_alignment {
                None => last_alignment = Some(row_alignment),
                Some(alignment) => {
                    if alignment == row_alignment {
                        return Err(ParserError::ParseError(
                            "Row alignment is not alternating".to_string(),
                        ));
                    }
                    last_alignment = Some(row_alignment);
                }
            }

            for (x, input) in board_inputs.iter().enumerate() {
                if *input == BoardInput::Empty {
                    continue;
                }

                // If the first row is shifted, make sure our x coordinate reflects
                // even-r offset coordinates rather than odd-r
                let (q, r) = if first_row_alignment == Some(Alignment::Shifted) {
                    HexGrid::evenr_to_axial(y, x)
                } else {
                    HexGrid::oddr_to_axial(y, x)
                };

                result.push((*input, HexLocation::new(q, r)))
            }
        }

        Ok(result)
    }
}

#[test]
pub fn test_board_parse_simple() {
    use PieceColor::*;
    use PieceType::*;

    let board_string = concat!(
        ". . . . .\n",
        " . Q 3 g .\n",
        ". . A b .\n",
        " . 2 . m .\n",
        ". . . . .\n",
    );

    let result = Parser::parse_board(board_string);
    assert!(
        result.is_ok(),
        "expected parse_board to give result, got {:?}",
        result
    );

    let board = result.unwrap();
    assert_eq!(board.len(), 7);
    let white_queen = board.iter().find(|(input, _)| match input {
        BoardInput::Piece(piece) => *piece == Piece::new(Queen, White),
        _ => false,
    });

    assert!(white_queen.is_some(), "Couldn't find white queen on board");
    let (_, white_queen_loc) = white_queen.unwrap();
    let stack_3_loc = white_queen_loc.apply(Direction::E);
    let black_grasshopper_loc = stack_3_loc.apply(Direction::E);
    let white_ant_loc = white_queen_loc.apply(Direction::SE);
    let black_beetle_loc = white_ant_loc.apply(Direction::E);
    let stack_2_loc = white_ant_loc.apply(Direction::SW);
    let black_mosquito_loc = black_beetle_loc.apply(Direction::SE);

    let stack_3 = BoardInput::Stack;
    let stack_2 = BoardInput::Stack;
    let black_grasshopper = BoardInput::Piece(Piece::new(Grasshopper, Black));
    let white_ant = BoardInput::Piece(Piece::new(Ant, White));
    let black_beetle = BoardInput::Piece(Piece::new(Beetle, Black));
    let black_mosquito = BoardInput::Piece(Piece::new(Mosquito, Black));

    assert!(board.contains(&(stack_3, stack_3_loc)));
    assert!(board.contains(&(stack_2, stack_2_loc)));
    assert!(board.contains(&(black_grasshopper, black_grasshopper_loc)));
    assert!(board.contains(&(white_ant, white_ant_loc)));
    assert!(board.contains(&(black_beetle, black_beetle_loc)));
    assert!(board.contains(&(black_mosquito, black_mosquito_loc)));
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
    assert!(
        result.is_ok(),
        "expected parse_board to give result, got {:?}",
        result
    );
    let board = result.unwrap();
    assert_eq!(board.len(), 0);
}

#[test]
pub fn test_board_parse_single() {
    use PieceColor::*;
    use PieceType::*;

    let board_string = concat!(" . . . . .\n", ". . . . .\n", " A . . . .\n",);

    let result = Parser::parse_board(board_string);
    assert!(
        result.is_ok(),
        "expected parse_board to give result, got {:?}",
        result
    );
    let board = result.unwrap();
    assert_eq!(board.len(), 1);
    let (input, _) = board[0];
    assert_eq!(input, BoardInput::Piece(Piece::new(Ant, White)));
}

#[test]
pub fn test_board_parse_fitted() {
    use PieceColor::*;
    use PieceType::*;
    let board_string = concat!(" . l m . 4\n", ". q . g 2\n", " A Q b 3 P\n",);

    let result = Parser::parse_board(board_string);
    assert!(
        result.is_ok(),
        "expected parse_board to give result, got {:?}",
        result
    );

    let board = result.unwrap();
    assert_eq!(board.len(), 11);
    let white_ant = board.iter().find(|(input, _)| match input {
        BoardInput::Piece(piece) => *piece == Piece::new(Ant, White),
        _ => false,
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
    let stack_2_loc = stack_4_loc.apply(Direction::SW);
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
    assert!(board.contains(&(white_queen, white_queen_loc)));
    assert!(board.contains(&(black_beetle, black_beetle_loc)));
    assert!(board.contains(&(white_pillbug, white_pillbug_loc)));
    assert!(board.contains(&(black_ladybug, black_ladybug_loc)));
    assert!(board.contains(&(black_mosquito, black_mosquito_loc)));
    assert!(board.contains(&(black_grasshopper, black_grasshopper_loc)));
    assert!(board.contains(&(stack_2, stack_2_loc)));
    assert!(board.contains(&(black_queen, black_queen_loc)));
}
