use crate::hex_grid::*;
use std::collections::HashSet;

/// Represents a HexGrid wrapper that can generate new positions
/// for a selected piece at a given height. It will create new boards according to the 
/// rules that govern that piece as if the game contained no Pillbug. 
///
/// For the pillbug, see the difference between pillbug_swaps() and pillbug_moves() TODO
///
/// The move generator is only guaranteed to generate moves correctly
/// for positions that follow the One Hive Rule
pub struct MoveGeneratorDebugger{
    grid : HexGrid,
    pinned : Vec<HexLocation>,
}


impl MoveGeneratorDebugger{
    pub fn new() -> MoveGeneratorDebugger{
        MoveGeneratorDebugger{
            grid : HexGrid::new(),
            pinned : Vec::new(),
        }
    }

    pub fn from_grid(grid : &HexGrid) -> MoveGeneratorDebugger{
        MoveGeneratorDebugger{
            grid: grid.clone(),
            pinned : grid.pinned(),
        }
    }

    pub fn spider_moves(&self, location : HexLocation, height : usize) -> Vec<HexGrid> {
        let stack = self.grid.peek(location);
        debug_assert!(stack.len() > height as usize);
        debug_assert!(stack[height as usize].piece == PieceType::Spider);

        if location == HexLocation::new(0, 0) {
            return Vec::new();
        }

        todo!()

    }
}


fn compare_positions(start_location: HexLocation, expected: &str, original_position: &HexGrid, test_positions: &Vec<HexGrid>) {
    let expected_locations = HexGrid::selector(expected);
    let mut original_position = original_position.clone();
    let piece = original_position.remove(start_location).expect("Expected piece at start location");
    let mut expected_positions = Vec::new();

    for location in expected_locations {
        let mut new_position = original_position.clone();
        new_position.add(piece, location);
        expected_positions.push(new_position);
    }

    assert_eq!(expected_positions.len(), test_positions.len());
    for position in test_positions {
        assert!(expected_positions.contains(position));
    }
}

#[test] 
pub fn test_spider_gate(){
    use PieceType::*; use PieceColor::*;
    // Testing with the "gate" structure that disallows free movement
    // between adjacent locations
    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . . a . . .\n",
        " . a S a . . .\n",
        ". . a a . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));
    let legal_moves = vec![];

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, height) = grid.find(Piece::new(PieceType::Spider, PieceColor::White)).unwrap();
    let spider_moves = generator.spider_moves(spider, height);

    assert_eq!(spider_moves, legal_moves);

    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . . a a . .\n",
        " . a . . a . .\n",
        ". . a S a . .\n",
        " . . a a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));
    let legal_moves = vec![];

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, height) = grid.find(Piece::new(PieceType::Spider, PieceColor::White)).unwrap();
    let spider_moves = generator.spider_moves(spider, height);

    assert_eq!(spider_moves, legal_moves);
}

#[test] 
pub fn test_spider_pinned(){
    use PieceType::*; use PieceColor::*;
    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . . a . . .\n",
        " . a S a . . .\n",
        ". . . . . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));
    let legal_moves = vec![];

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, height) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider, height);
    assert_eq!(spider_moves, legal_moves);

    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . a . . . .\n",
        " . a S . . . .\n",
        ". . . a a . .\n",
        " . . a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));
    let legal_moves = vec![];

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, height) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider, height);
    assert_eq!(spider_moves, legal_moves);
}


#[test] 
pub fn test_spider_door(){
    // Testing with the "door" structure that allows spiders extra mobility than typical
    use PieceType::*; use PieceColor::*;
    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a S . .\n",
        ". a . . . . .\n",
        " . a a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));

    let selector = concat!( 
        " . . * . . . .\n",
        ". . a a . . .\n",
        " . a * a S . .\n",
        ". a * . . . .\n",
        " . a a * . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
    );

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, height) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider, height);
    compare_positions(spider, selector, &grid, &spider_moves);

    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a . . S . .\n",
        " . a a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));

    let selector = concat!( 
        " . . . * . . .\n",
        ". . a a . . .\n",
        " . a * a . . .\n",
        ". a * . S . .\n",
        " . a a . . . .\n",
        ". . . * . . .\n\n",
        "start - [0 0]\n\n",
    );

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, height) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider, height);
    compare_positions(spider, selector, &grid, &spider_moves);
}

#[test] 
pub fn test_spider_typical_boards() {
    use PieceType::*; use PieceColor::*;
    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a . . S . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));
    let selector = concat!( 
        " . . . * . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a * . S . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    );


    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, height) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider, height);
    compare_positions(spider, selector, &grid, &spider_moves);
}
