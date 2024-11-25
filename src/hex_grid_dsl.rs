use crate::hex_grid::*;
use thiserror::Error;
use regex::Regex;

pub type Result<T> = std::result::Result<T, ParserError>;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Could not parse board row: {0}")]
    RowError(String),
    #[error("Could not parse start location")]
    StartSyntaxError,
    #[error("Could not parse stack line: {0}")]
    StackLineSyntaxError(String),
    #[error("Could not parse stack: {0}")]
    StackParseError(String),
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
///  start - [ 3 -2 ]
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
    Stack(u8),
    StackPieces([Option<Piece>; 7]),
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
                '2'..='7' => {
                    if piece_count > 0 {
                        return Err(RowError("Invalid stack placement".to_string()));
                    }

                    let digit = input.to_digit(10).unwrap() as u8;
                    board_inputs.push(BoardInput::Stack(digit));
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

    /// Given a HexGrid board string according to the DSL specification,
    /// interpret it as a set of board inputs with locations
    /// correct relative to each other and the top left corner of the board 
    /// assigned to (0,0).
    ///
    /// In other words, while the HexLocation may be incorrect relative to the 
    /// ultimate HexGrid, all directions and distances between pieces should be
    /// correct.
    ///
    /// The pieces must be returned in "board order", that is,
    /// pieces are return first by row, then by column.
    ///
    /// If an interpretation is not possible, return an error.
    fn parse_board(input: &str) -> Result<Vec<(BoardInput, HexLocation)>> {
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

    /// Given a HexGrid start string according to the DSL specification,
    /// interpret it as a HexLocation. 
    ///
    /// Create a new array with the given (piece, hex location) 
    /// reflecting the correct axial locations relative to the start location.
    fn parse_start(input: &str, pieces : &Vec<(Piece, HexLocation)>) -> Result<Vec<(Piece, HexLocation)>> {
        let re = Regex::new(r"start\s*-\s*\[\s*(-?\d+)\s*(-?\d+)\s*\]").unwrap();
        let captures = re.captures(input);
        let Some(captures) = captures else {
            return Err(ParserError::StartSyntaxError);
        };

        let x = captures[1].parse::<i8>().unwrap();
        let y = captures[2].parse::<i8>().unwrap();
        let start_location = HexLocation::new(x, y);

        let mut result = Vec::new();
        for loc in pieces {
            let (piece, piece_loc) = loc;
            let new_loc = piece_loc.add(start_location);
            result.push((*piece, new_loc));
        }

        return Ok(result);
    }


    /// Given a HexGrid stack string according to the DSL specification,
    /// convert the match Stack -> StackPieces in a given tuple of (BoardInput,
    /// HexLocation) pairs and return the new array with everything else unchanged.
    fn parse_stacks(input: &str, pieces : &Vec<(BoardInput, HexLocation)>) -> Result<Vec<(BoardInput, HexLocation)>> {
        let mut result = Vec::new();
        let mut index = 0;
        for line in input.lines() {
            let re = Regex::new(r"(\d+)\s*-\s*\[\s*((\w\s*){2,})\s*\]").unwrap();
            let captures = re.captures(line);
            let Some(captures) = captures else {
                return Err(ParserError::StackLineSyntaxError(line.to_string()));
            };

            let stack_num = captures[1].parse::<u8>().expect("Couldn't parse stack number");
            while index < pieces.len() {
                let (input, _) = pieces[index];
                if matches!(input, BoardInput::Stack(_)) {
                    break;
                }
                result.push(pieces[index]);
                index += 1
            }

            if index >= pieces.len() {
                return Err(ParserError::StackParseError("More stacks specified than input".to_string()));
            }

            let (stack, loc) = pieces[index];

            let num = match stack {
                BoardInput::Stack(num) => num,
                _ => unreachable!(),
            };
            
            if num != stack_num {
                return Err(ParserError::StackParseError(format!("Stack number mismatch, check that stack string is in board order. Expected {} Got {} on line {}", num, stack_num, line.to_string())));
            }

            let mut stack = [None ; 7];
            for (i, piece) in captures[2].split_whitespace().enumerate() {
                let piece_type = PieceType::try_from_char(&piece.chars().next().unwrap());
                if piece_type.is_err() {
                    return Err(ParserError::StackParseError(format!("Invalid piece type: {}", piece)));
                }

                let color = if piece.chars().nth(0).unwrap().is_lowercase(){
                    PieceColor::Black
                } else {
                    PieceColor::White
                };

                let piece = Piece::new(piece_type.unwrap(), color);
                stack[i] = Some(piece)
            }

            result.push((BoardInput::StackPieces(stack), loc));
            index += 1;

        }

        while index < pieces.len() {
            result.push(pieces[index]);
            index += 1;
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

    let stack_3 = BoardInput::Stack(3);
    let stack_2 = BoardInput::Stack(2);
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

    let stack_4 = BoardInput::Stack(4);
    let stack_3 = BoardInput::Stack(3);
    let stack_2 = BoardInput::Stack(2);

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

#[test]
pub fn test_board_follows_board_order() {
    use PieceColor::*;
    use PieceType::*;
    let board_string = concat!(
        " . l m . 4\n", 
        ". q . g 2\n", 
        " . . . . .\n",
        "A Q b 3 P\n",
        " . . . . .\n"
    );

    let result = Parser::parse_board(board_string);
    assert!(
        result.is_ok(),
        "expected parse_board to give result, got {:?}",
        result
    );


    let board = result.unwrap();
    assert_eq!(board.len(), 11);
    let pieces = board.into_iter().map(|(input, _)| input).collect::<Vec<_>>();
    let expected = vec![
        BoardInput::Piece(Piece::new(Ladybug, Black)),
        BoardInput::Piece(Piece::new(Mosquito, Black)),
        BoardInput::Stack(4),
        BoardInput::Piece(Piece::new(Queen, Black)),
        BoardInput::Piece(Piece::new(Grasshopper, Black)),
        BoardInput::Stack(2),
        BoardInput::Piece(Piece::new(Ant, White)),
        BoardInput::Piece(Piece::new(Queen, White)),
        BoardInput::Piece(Piece::new(Beetle, Black)),
        BoardInput::Stack(3),
        BoardInput::Piece(Piece::new(Pillbug, White)),
    ];

    assert_eq!(pieces, expected);

}

#[test]
pub fn test_board_location_relative_to_top_left() {
    use PieceColor::*;
    use PieceType::*;

    let board_string = concat!(
        ". l m . .\n", 
        " 2 l m . .\n", 
        "2 . . . .\n", 
    );

    let result = Parser::parse_board(board_string);
    assert!(
        result.is_ok(),
        "expected parse_board to give result, got {:?}",
        result
    );

    // Note: assuming "board order", (top to bottom, left to right)

    let board = result.unwrap();
    assert_eq!(board.len(), 6);
    let pieces = board.into_iter().map(|(_, loc)| loc).collect::<Vec<_>>();
    let top_left = HexLocation::new(0, 0);
    let expected = vec![
        top_left.apply(Direction::E),
        top_left.apply(Direction::E).apply(Direction::E),
        top_left.apply(Direction::SE),
        top_left.apply(Direction::SE).apply(Direction::E),
        top_left.apply(Direction::SE).apply(Direction::E).apply(Direction::E),
        top_left.apply(Direction::SE).apply(Direction::SW),
    ];

    assert_eq!(pieces, expected);

}

#[test]
pub fn test_simple_start_loc() {
    use PieceColor::*;
    use PieceType::*;

    let start_string = "start - [ 3 -2 ]";
    let pieces = vec![
        (Piece::new(Queen, White), HexLocation::new(0, 0)),
        (Piece::new(Grasshopper, Black), HexLocation::new(1, 0)),
        (Piece::new(Ant, White), HexLocation::new(0, 1)),
    ];

    let result = Parser::parse_start(start_string, &pieces);
    assert!(
        result.is_ok(),
        "expected parse_start to give result, got {:?}",
        result
    );

    // New locations should have been shifted by 3, -2 while preserving order
    let expected = vec![
        (Piece::new(Queen, White), HexLocation::new(3, -2)),
        (Piece::new(Grasshopper, Black), HexLocation::new(4, -2)),
        (Piece::new(Ant, White), HexLocation::new(3, -1)),
    ];

    assert_eq!(result.unwrap(), expected);
}

#[test]
pub fn test_parse_stacks() {
    use PieceColor::*;
    use PieceType::*;

    // Make sure that it fills in the stacks and leaves everything 
    // else alone
    let board = concat!(
        "2 3 a\n", 
        " 4 5 A\n"
    );

    let parse_string = concat!(
        "2-[m m]\n",
        "3 -[ m m m]\n",
        "4- [   m    m m  m]\n",
        "5 -[m m m  m m  ]\n"
    );

    let pieces = Parser::parse_board(board).expect("Couldn't parse board");

    let pieces = Parser::parse_stacks(parse_string, &pieces).expect("Couldn't parse stacks");

    let stack_2 = BoardInput::StackPieces([
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        None,
        None,
        None,
        None,
        None,
    ]);

    let stack_3 = BoardInput::StackPieces([
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        None,
        None,
        None,
        None,
    ]);

    let stack_4 = BoardInput::StackPieces([
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        None,
        None,
        None,
    ]);

    let stack_5 = BoardInput::StackPieces([
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        Some(Piece::new(Mosquito, Black)),
        None,
        None,
    ]);

    let white_ant = BoardInput::Piece(Piece::new(Ant, White));
    let black_ant = BoardInput::Piece(Piece::new(Ant, Black));

    let expected = vec![
        stack_2,
        stack_3,
        black_ant,
        stack_4,
        stack_5,
        white_ant,
    ];

    let pieces = pieces.into_iter().map(|(input, _)| input).collect::<Vec<_>>();
    for (i, piece) in pieces.iter().enumerate() {
        assert_eq!(piece, &expected[i]);
    }
}
