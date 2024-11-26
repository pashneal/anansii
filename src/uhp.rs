use crate::hex_grid::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum UHPError {
    #[error("Too many pieces changed at once")]
    AmbiguousState(Annotator),
    #[error("Inference is not possible because the annotator invariant was violated")]
    InvariantError,
}

pub type Result<T> = std::result::Result<T, UHPError>;

/// This struct represents UHP (Universal Hive Protocol) interface
/// https://github.com/jonthysell/Mzinga/wiki/UniversalHiveProtocol
pub struct UHP {}

type StackIds = Vec<Option<u8>>;

/// Responsible for annotation of moves in UHP format
/// given a board history fed into the annotator state by state
///
/// Invariant : The unambigious Annotator assumes that Hive pieces can only be placed or moved
/// but never removed from the board. Furthermore, it assumes that piece movements observe the
/// One Hive Rule. If this is the case, all moves will be annotated correctly.
///
/// TODO: cleanup, make more precise
/// Allowed moves:
///     - pass : no additions or removals made from the board
///     - placement : a single piece is added to top of the board which did not exist
///     - movement : a single piece is moved from the top of one location to top of another.
///     The resulting board has greater than 1 piece on the board's lowest level
///
/// If the state becomes ambiguous, some moves may not have correct unique identifiers
/// (such as bG1, bG2, etc) and the annotator will instead return moves with question marks
/// (bG?, bG?, etc) to indicate that the piece's identity is ambiguous.
#[derive(Debug, Clone)]
pub struct Annotator {
    /// An array of locations that we can currently identify
    /// the placement order of the pieces. This is used to
    /// determine whether pieces should be annotated
    /// bG1 bG2 ... etc
    ids: HashMap<HexLocation, StackIds>,
    /// Stores the number of pieces present
    piece_counts: HashMap<Piece, u8>,
    /// The previous state of the grid
    prev_grid: HexGrid,
    /// Whether each piece can be uniquely identified
    ambiguous: bool,
    /// Move strings (not UHP-compatible)
    moves: Vec<String>,
}

pub type Height = usize;
enum Diff {
    Added {
        loc: HexLocation,
        piece: Piece,
        height: Height,
    },
    Removed {
        loc: HexLocation,
        piece: Piece,
        height: Height,
    },
}

impl Annotator {
    pub fn new() -> Annotator {
        Annotator {
            ids: HashMap::new(),
            piece_counts: HashMap::new(),
            prev_grid: HexGrid::new(),
            ambiguous: false,
            moves: Vec::new(),
        }
    }

    /// Computes differences between the last state fed into this Annotator
    /// via next_state() and the given current_grid. Assumes
    /// any removal or addition at a height necessarily
    /// constitutes removal or addition of heights above that height
    fn get_differences(&self, current_grid: &HexGrid) -> Vec<Diff> {
        let mut diffs = Vec::new();
        // Look for added pieces
        for (new_stack, loc) in current_grid.pieces() {
            let old_stack = self.prev_grid.peek(loc);
            // Any piece higher the old stack's length is necessarily added
            for height in old_stack.len()..new_stack.len() {
                let piece = new_stack[height];
                diffs.push(Diff::Added { loc, piece, height });
            }

            for height in 0..old_stack.len().min(new_stack.len()) {
                if old_stack[height] != new_stack[height] {
                    // First difference detected, assume all others
                    // after this height were added
                    for i in height..new_stack.len() {
                        let piece = new_stack[i];
                        diffs.push(Diff::Added {
                            loc,
                            piece,
                            height: i,
                        })
                    }
                    break;
                }
            }
        }

        // Look for removed pieces
        for (old_stack, loc) in self.prev_grid.pieces() {
            let new_stack = current_grid.peek(loc);
            // Any piece missing from old stack is necessarily removed
            for height in new_stack.len()..old_stack.len() {
                let piece = old_stack[height];
                diffs.push(Diff::Removed { loc, piece, height })
            }

            for height in 0..old_stack.len().min(new_stack.len()) {
                if old_stack[height] != new_stack[height] {
                    // First difference detected, assume all others
                    // after this height were removed
                    for i in height..new_stack.len() {
                        let piece = old_stack[i];
                        diffs.push(Diff::Removed {
                            loc,
                            piece,
                            height: i,
                        })
                    }
                    break;
                }
            }
        }

        diffs
    }

    fn reference(
        hex_grid: &HexGrid,
        location: HexLocation,
        ids: &HashMap<HexLocation, StackIds>,
    ) -> String {
        let nw = location.apply(Direction::NW);
        let sw = location.apply(Direction::SW);
        let ne = location.apply(Direction::NE);
        let se = location.apply(Direction::SE);
        let e = location.apply(Direction::E);
        let w = location.apply(Direction::W);

        fn relative_direction(direction: Direction, uhp: &str) -> String {
            match direction {
                Direction::E => format!("-{}", uhp),
                Direction::W => format!("{}-", uhp),
                Direction::NW => format!(r"{}\", uhp),
                Direction::SW => format!(r"{}/", uhp),
                Direction::SE => format!(r"\{}", uhp),
                Direction::NE => format!(r"/{}", uhp),
            }
        }

        return if hex_grid.peek(nw).len() > 0 {
            let piece = hex_grid.peek(nw);
            let piece = piece.last().unwrap();
            let id = ids.get(&nw).unwrap().last().unwrap().unwrap();
            relative_direction(Direction::NW, &piece.to_uhp(id))
        } else if hex_grid.peek(sw).len() > 0 {
            let piece = hex_grid.peek(sw);
            let piece = piece.last().unwrap();
            let id = ids.get(&sw).unwrap().last().unwrap().unwrap();
            relative_direction(Direction::SW, &piece.to_uhp(id))
        } else if hex_grid.peek(ne).len() > 0 {
            let piece = hex_grid.peek(ne);
            let piece = piece.last().unwrap();
            let id = ids.get(&ne).unwrap().last().unwrap().unwrap();
            relative_direction(Direction::NE, &piece.to_uhp(id))
        } else if hex_grid.peek(se).len() > 0 {
            let piece = hex_grid.peek(se);
            let piece = piece.last().unwrap();
            let id = ids.get(&se).unwrap().last().unwrap().unwrap();
            relative_direction(Direction::SE, &piece.to_uhp(id))
        } else if hex_grid.peek(e).len() > 0 {
            let piece = hex_grid.peek(e);
            let piece = piece.last().unwrap();
            let id = ids.get(&e).unwrap().last().unwrap().unwrap();
            relative_direction(Direction::E, &piece.to_uhp(id))
        } else if hex_grid.peek(w).len() > 0 {
            let piece = hex_grid.peek(w);
            let piece = piece.last().unwrap();
            let id = ids.get(&w).unwrap().last().unwrap().unwrap();
            relative_direction(Direction::W, &piece.to_uhp(id))
        } else {
            panic!("No reference found, some invariant was violated!");
        };
    }

    fn piece_moved(&self, old: &Diff, new: &Diff, grid: &HexGrid) -> Annotator {
        if self.ambiguous {
            todo!()
        }

        let Diff::Removed {
            loc: old_loc,
            piece: old_piece,
            height: old_height,
        } = old
        else {
            panic!("Expected a removed piece");
        };

        let Diff::Added {
            loc: new_loc,
            piece: new_piece,
            height: new_height,
        } = new
        else {
            panic!("Expected an added piece");
        };

        let mut new_ids = self.ids.clone();
        let ids = new_ids
            .get_mut(&old_loc)
            .expect("Non ambiguous state should identify all pieces");
        // Should be the top of the stack
        debug_assert!(ids.len() == old_height + 1);

        let id = ids[*old_height];
        ids.remove(*old_height);

        let ids = new_ids.entry(*new_loc).or_insert(vec![]);

        // Should be the top of the stack
        debug_assert!(ids.len() == *new_height);
        ids.push(id);

        let anchor = Annotator::reference(grid, *new_loc, &new_ids);
        let piece_string = new_piece.to_uhp(id.expect("Expected an id"));
        let move_string = format!("{} {}", piece_string, anchor);

        let mut moves = self.moves.clone();
        moves.push(move_string);

        Annotator {
            ids: new_ids,
            piece_counts: self.piece_counts.clone(),
            prev_grid: grid.clone(),
            ambiguous: false,
            moves,
        }
    }

    fn piece_placed(&self, position: &Diff, grid: &HexGrid) -> Annotator {
        if self.ambiguous {
            todo!()
        }

        let mut new_piece_counts = self.piece_counts.clone();
        let Diff::Added { loc, piece, height } = position else {
            panic!("Expected an added piece in piece_placed()");
        };

        let mut new_ids = self.ids.clone();
        let ids = new_ids.entry(*loc).or_insert(vec![]);

        let new_id = new_piece_counts.entry(*piece).or_insert(0);
        *new_id += 1;

        ids.push(Some(*new_id));
        // Make sure the piece is added to the top of the stack
        assert!(ids.len() == height + 1);

        let piece_string = piece.to_uhp(*new_id);

        let move_string = if self.prev_grid.is_empty() {
            format!("{}", piece_string)
        } else {
            let anchor = Annotator::reference(&self.prev_grid, *loc, &new_ids);
            format!("{} {}", piece_string, anchor)
        };

        let mut moves = self.moves.clone();
        moves.push(move_string);

        Annotator {
            ids: new_ids,
            piece_counts: new_piece_counts,
            prev_grid: grid.clone(),
            ambiguous: false,
            moves,
        }
    }

    pub fn next_state(&self, current_grid: &HexGrid) -> Result<Annotator> {
        // No difference, passing moves

        let mut total_count = 0;
        for count in self.piece_counts.values() {
            total_count += count;
        }

        let mut diffs = self.get_differences(current_grid);

        // Happy paths:
        // 0 diffs
        // 1 diff and it's a piece placement
        // 2 diffs and it's removing the same piece from one location and adding it to another

        // 0 diffs happy path
        if diffs.len() == 0 {
            let mut moves = self.moves.clone();
            moves.push("pass".to_string());

            let annotator = Annotator {
                ids: self.ids.clone(),
                piece_counts: self.piece_counts.clone(),
                prev_grid: current_grid.clone(),
                ambiguous: false,
                moves,
            };

            return Ok(annotator);
        }

        // 1 diffs happy path
        if diffs.len() == 1 {
            let diff = &diffs[0];
            return match diff {
                Diff::Added { .. } => Ok(self.piece_placed(diff, &current_grid)),
                _ => Err(UHPError::InvariantError),
            };
        }

        // 2 diffs happy path
        if diffs.len() == 2 {
            let (added, removed) = match (&diffs[0], &diffs[1]) {
                (Diff::Added { piece: a, .. }, Diff::Removed { piece: b, .. }) if b == a => {
                    (&diffs[0], &diffs[1])
                }
                (Diff::Removed { piece: a, .. }, Diff::Added { piece: b, .. }) if b == a => {
                    (&diffs[1], &diffs[0])
                }
                (Diff::Removed { .. }, Diff::Removed { .. }) => {
                    return Err(UHPError::InvariantError)
                }
                _ => todo!("this should return an ambigous state instead of an error"),
            };

            return Ok(self.piece_moved(removed, added, &current_grid));
        }

        // If we have more than 2 diffs, we can't infer the state, but can return an
        // ambiguous state if the diffs are purely additions
        // If we have 0 diffs, we can't infer the state either, yet

        todo!("ambiguous state with only additions (thereby resulted from legal state transitions")
    }

    pub fn move_strings(&self) -> Vec<String> {
        self.moves.clone()
    }

    pub fn uhp_move_strings(&self) -> Vec<String> {
        self.move_strings()
            .iter()
            .map(|move_string| {
                move_string
                    .replace("P1", "P")
                    .replace("M1", "M")
                    .replace("L1", "L")
                    .replace("Q1", "Q")
            })
            .collect()
    }
}

#[test]
pub fn test_annotator_empty() {
    let mut annotator = Annotator::new();
    let grid = HexGrid::new();
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Empty -> empty should be a legal state transition"
    );
    annotator = result.unwrap();
    assert_eq!(annotator.move_strings(), vec![String::from("pass")]);
    assert_eq!(annotator.uhp_move_strings(), vec![String::from("pass")]);
}

#[test]
pub fn test_annotator_pass() {
    let mut annotator = Annotator::new();
    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . L . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();
    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . L . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(result.is_ok(), "Pass should be handled correctly");
    annotator = result.unwrap();

    assert_eq!(
        annotator.move_strings(),
        vec![String::from("wL1"), String::from("pass")]
    );
    assert_eq!(
        annotator.uhp_move_strings(),
        vec![String::from("wL"), String::from("pass")]
    );
}

#[test]
pub fn test_annotator_place() {
    let mut annotator = Annotator::new();
    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . B . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . B a .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . Q . .\n",
        " . . B a .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". a Q . .\n",
        " . . B . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single movement should be handled correctly"
    );
    annotator = result.unwrap();

    assert_eq!(
        annotator.move_strings(),
        vec![
            String::from("wB1"),
            String::from("bA1 wB1-"),
            String::from(r"wQ1 \wB1"),
            String::from("bA1 -wQ1")
        ]
    );
    assert_eq!(
        annotator.uhp_move_strings(),
        vec![
            String::from("wB1"),
            String::from("bA1 wB1-"),
            String::from(r"wQ \wB1"),
            String::from("bA1 -wQ")
        ]
    );
}

#[test]
pub fn test_annotator_climb() {
    let mut annotator = Annotator::new();
    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . B . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . B a .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . Q . .\n",
        " . . B a .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". a Q . .\n",
        " . . B . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Single movement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". a 2 . .\n",
        " . . . . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "2 - [Q B]\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Single movement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". 2 Q . .\n",
        " . . . . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "2 - [a B]\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Single movement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". a Q . .\n",
        " B . . . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Climb off should be handled correctly");

    assert_eq!(
        annotator.move_strings(),
        vec![
            String::from("wB1"),
            String::from("bA1 wB1-"),
            String::from(r"wQ1 \wB1"),
            String::from("bA1 -wQ1"),
            String::from("wB1 bA1-"),
            String::from("wB1 -wQ1"),
            String::from("wB1 /bA1")
        ]
    );
    assert_eq!(
        annotator.uhp_move_strings(),
        vec![
            String::from("wB1"),
            String::from("bA1 wB1-"),
            String::from(r"wQ \wB1"),
            String::from("bA1 -wQ"),
            String::from("wB1 bA1-"),
            String::from("wB1 -wQ"),
            String::from("wB1 /bA1")
        ]
    );
}

#[ignore]
#[test]
pub fn test_uhp_move_strings() {
    let mut annotator = Annotator::new();
    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . S . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . S . .\n",
        ". . L . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . S . .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . m .\n",
        " . . S . .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . m .\n",
        " . . S G .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . 2 G .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "2 - [ S m ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single movement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . 3 G .\n",
        ". . . p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single movement should be handled correctly"
    );

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . . 3 G .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single movement should be handled correctly"
    );

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . . 3 . .\n",
        ". . G . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Single movement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . . 3 G .\n",
        ". . G . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Second placement should be handled correctly"
    );
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . G 3 . .\n",
        ". . G . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(result.is_ok(), "Pieces should be fungible even if moved");
    annotator = result.unwrap();

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . G 4 . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "4 - [ S m L G]\n\n",
    ));
    let result = annotator.next_state(&grid);
    assert!(
        result.is_ok(),
        "Pieces should be fungible even if climbing up"
    );
    annotator = result.unwrap();

    let moves = annotator.move_strings();
    let possible_moves = vec![
        vec![String::from("wS1")],
        vec![String::from("wS1 /wL1")],
        vec![String::from(r"bP1 wS1\"), String::from("bP1 wL1-")],
        vec![String::from(r"bM1 wS1/")],
        vec![
            String::from(r"wG1 wS1-"),
            String::from(r"wG1 bP1/"),
            String::from(r"wG1 bM1\"),
        ],
        vec![
            String::from(r"bM1 -wG1"),
            String::from(r"bM1 \bP1"),
            String::from(r"bM1 wL1/"),
        ],
        vec![String::from(r"wL1 \bP1"), String::from(r"wL1 -wG1")],
        vec![
            String::from(r"bP1 wS1/"),
            String::from(r"bP1 bM1/"),
            String::from(r"bP1 bL1/"),
            String::from(r"bP1 \wG1"),
        ],
        vec![
            String::from(r"wG1 /wS1"),
            String::from(r"wG1 /bM1"),
            String::from(r"wG1 /bL1"),
        ],
        vec![
            String::from(r"wG2 wS1-"),
            String::from(r"wG2 bM1-"),
            String::from(r"wG2 bL1-"),
            String::from(r"wG2 bP1\"),
        ],
        vec![
            String::from(r"wG2 -wS1"),
            String::from(r"wG2 -bM1"),
            String::from(r"wG2 -bL1"),
            String::from(r"wG2 \wG1"),
        ],
        vec![String::from(r"wG1 wG2-"), String::from(r"wG1 /bP1")],
    ];
    assert_eq!(possible_moves.len() == moves.len(), true);

    for (expected, actual) in possible_moves.iter().zip(moves.iter()) {
        assert!(
            expected.contains(actual),
            "Expected {:?} to contain {:?}",
            expected,
            actual
        );
    }

    let uhp_moves = annotator.uhp_move_strings();
    let possible_uhp_moves = vec![
        vec![String::from("wS1")],
        vec![String::from("wS1 /wL")],
        vec![String::from(r"bP wS1\"), String::from("bP wL-")],
        vec![String::from(r"bM wS1/")],
        vec![
            String::from(r"wG1 wS1-"),
            String::from(r"wG1 bP/"),
            String::from(r"wG1 bM\"),
        ],
        vec![
            String::from(r"bM -wG1"),
            String::from(r"bM \bP"),
            String::from(r"bM wL/"),
        ],
        vec![String::from(r"wL \bP"), String::from(r"wL -wG1")],
        vec![
            String::from(r"bP wS1/"),
            String::from(r"bP bM/"),
            String::from(r"bP bL/"),
            String::from(r"bP \wG1"),
        ],
        vec![
            String::from(r"wG1 /wS1"),
            String::from(r"wG1 /bM"),
            String::from(r"wG1 /bL"),
        ],
        vec![
            String::from(r"wG2 wS1-"),
            String::from(r"wG2 bM-"),
            String::from(r"wG2 bL-"),
            String::from(r"wG2 bP\"),
        ],
        vec![
            String::from(r"wG2 -wS1"),
            String::from(r"wG2 -bM"),
            String::from(r"wG2 -bL"),
            String::from(r"wG2 \wG1"),
        ],
        vec![String::from(r"wG1 wG2-"), String::from(r"wG1 /bP")],
    ];
    assert_eq!(possible_uhp_moves.len() == uhp_moves.len(), true);

    for (expected, actual) in possible_uhp_moves.iter().zip(uhp_moves.iter()) {
        assert!(
            expected.contains(actual),
            "Expected {:?} to contain {:?}",
            expected,
            actual
        );
    }
}
