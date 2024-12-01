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
    outside : HashSet<HexLocation>,
}


impl MoveGeneratorDebugger{
    pub fn new() -> MoveGeneratorDebugger{
        MoveGeneratorDebugger{
            grid : HexGrid::new(),
            pinned : Vec::new(),
            outside : HashSet::new(),
        }
    }

    pub fn from_grid(grid : &HexGrid) -> MoveGeneratorDebugger{
        MoveGeneratorDebugger{
            grid: grid.clone(),
            pinned : grid.pinned(),
            outside : grid.outside(),
        }
    }

    pub fn spider_dfs(&self, location: HexLocation, mut visited: Vec<HexLocation>, depth : usize, spider_removed : &HexGrid) -> Vec<HexLocation>  {
        if visited.contains(&location) {
            return vec![] 
        }
        visited.push(location);

        if depth == 3 {
            return vec![location]
        }


        let mut result = vec![];

        for slidable_location in spider_removed.slidable_locations(location).iter() {
            let found = self.spider_dfs(*slidable_location, visited.clone(), depth + 1, spider_removed);
            result.extend(found);
        }

        result
    }

    /// Returns a list of all possible moves for a spider at a given location
    /// if the spider is not covered by any other pieces.
    pub fn spider_moves(&self, location : HexLocation) -> Vec<HexGrid> {
        let stack = self.grid.peek(location);
        debug_assert!(stack.len() == 1 as usize);
        debug_assert!(stack[0].piece == PieceType::Spider);

        if self.pinned.contains(&location) {
            return vec![]
        }

        let mut spider_removed = self.grid.clone();
        spider_removed.remove(location);

        let new_locations = self.spider_dfs(location, vec![], 0, &spider_removed);
        let deduplicated = new_locations.iter().cloned().collect::<HashSet<HexLocation>>();

        let mut result = vec![];

        for new_location in deduplicated.iter() {
            let mut new_grid = self.grid.clone();
            new_grid.remove(location);
            new_grid.add(stack[0], *new_location);
            result.push(new_grid);
        }
        
        result
    }
}


fn compare_moves(start_location: HexLocation, expected: &str, original_position: &HexGrid, test_positions: &Vec<HexGrid>) {
    let expected_locations = HexGrid::selector(expected);
    let mut original_position = original_position.clone();
    let piece = original_position.remove(start_location).expect("Expected piece at start location");
    let mut expected_positions = Vec::new();

    for location in expected_locations {
        let mut new_position = original_position.clone();
        new_position.add(piece, location);
        expected_positions.push(new_position);
    }

    for position in test_positions {
        println!("test_position:\n{}\n", position.to_dsl());
    }
    for position in expected_positions.iter() {
        println!("expected_position:\n{}\n", position.to_dsl());
    }

    assert_eq!(expected_positions.len(), test_positions.len());
    for position in expected_positions {
        assert!(test_positions.contains(&position));
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
    let (spider, _) = grid.find(Piece::new(PieceType::Spider, PieceColor::White)).unwrap();
    let spider_moves = generator.spider_moves(spider);

    assert!(spider_moves.is_empty());
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
    let (spider, _) = grid.find(Piece::new(PieceType::Spider, PieceColor::White)).unwrap();
    let spider_moves = generator.spider_moves(spider);

    assert!(spider_moves.is_empty());
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

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider);
    assert!(spider_moves.is_empty());

    let grid = HexGrid::from_dsl(concat!( 
        " . . . . . . .\n",
        ". . a . . . .\n",
        " . a S . . . .\n",
        ". . . a a . .\n",
        " . . a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ));

    let generator = MoveGeneratorDebugger::from_grid(&grid);
    let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider);
    assert!(spider_moves.is_empty());
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
    let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider);
    compare_moves(spider, selector, &grid, &spider_moves);

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
    let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider);
    compare_moves(spider, selector, &grid, &spider_moves);
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
    let (spider, _) = grid.find(Piece::new(Spider, White)).unwrap();
    let spider_moves = generator.spider_moves(spider);
    compare_moves(spider, selector, &grid, &spider_moves);
}
