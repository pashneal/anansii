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
/// One Hive Rule. If this is the case, all legal moves will be annotated correctly as
/// UHP MoveStrings.
///
/// Unambiguous changes to the Annotator from state to state are as follows:
///
///     - pass : no additions or removals made from the board
///     - placement : a single piece is added to top of the board which did not previously exist
///     - movement : a single piece is moved from the top of one location to top of another.
///     The resulting board has greater than 1 piece on the board's lowest level
///
/// If the state becomes ambiguous, some moves may not have correct unique identifiers
/// (such as bG1, bG2, etc) and the annotator may instead return moves with question marks
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

#[derive(Debug)]
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
            // new_stack > old_stack
            for height in old_stack.len()..new_stack.len() {
                let piece = new_stack[height];
                diffs.push(Diff::Added { loc, piece, height });
            }

            // new_stack <= old_stack
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

    /// Computes the second part of a UHP MoveString, the anchor.
    /// location - the piece's new location,
    /// hex_grid - the newest state of the hexgrid
    /// ids - mappings from the newest state of the board to unique identifiers for each piece
    fn anchor_reference(
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

    /// Given a piece that was removed and a piece that was added,
    /// annotate the move in UHP format and return the new state of the annotator
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

        let anchor = Annotator::anchor_reference(grid, *new_loc, &new_ids);
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

    /// Given a piece that was placed, annotate the move in UHP format
    /// and return the new state of the annotator
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
            let anchor = Annotator::anchor_reference(&self.prev_grid, *loc, &new_ids);
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

    /// Add a new state to the annotator, representing a single legal move taken
    /// from the last state of the board to the current state of the board.
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

        println!("Unhappy path diffs: {:?}", diffs);
        println!("Unhappy position state: \n{}", current_grid.to_dsl());
        println!("Unhappy previous state: \n{}", self.prev_grid.to_dsl());
        todo!("ambiguous state with only additions (thereby resulted from legal state transitions")
    }

    /// Assuming an unambiguous state, find the piece, locataion and height
    /// uniquely described by the given piece string. examples "wQ1", "bM1", etc
    fn find(&self, piece_string: &str) -> Option<(Piece, HexLocation, Height)> {
        for (loc, stack) in &self.ids {
            for (height, piece_id) in stack.iter().enumerate() {
                if let Some(id) = piece_id {
                    let piece = self.prev_grid.peek(*loc)[height];
                    if piece.to_uhp(*id) == piece_string {
                        return Some((piece, *loc, height));
                    }
                }
            }
        }
        None
    }

    /// Add a new state to the annotator, representing a move string expect with identifiers
    /// appended to all pieces. (e.g. wQ1, bM1, etc)
    ///
    /// The move must represent a legal Hive move from the last state of the board
    /// to the current state.
    ///
    /// Returns the resulting state of the annotator after the move is applied
    pub fn next_standard_move(&self, move_string: &str) -> Result<Annotator> {
        // TODO: cleanup logic - this function is a little long
        let mut parts = move_string.split_whitespace();

        let piece_string = parts.next().expect("Expected a piece");
        let new_piece = Piece::from_uhp(piece_string).expect("Expected a valid piece");

        if move_string.len() <= 3 {
            let mut new_grid = HexGrid::new();
            new_grid.add(new_piece, HexLocation::new(0, 0));
            return self.next_state(&new_grid);
        } else if move_string == "pass" {
            return self.next_state(&self.prev_grid);
        }

        let anchor_piece_string = parts.next().expect("Expected an anchor position");

        let length = anchor_piece_string.len();

        // direction either at front, end or neither
        let first_last = (
            anchor_piece_string.chars().next().unwrap(),
            anchor_piece_string.chars().last().unwrap(),
        );

        let (direction, anchor_piece_string) = match first_last {
            ('-', _) => (Some(Direction::W), &anchor_piece_string[1..]),
            (_, '-') => (Some(Direction::E), &anchor_piece_string[..length - 1]),
            ('\\', _) => (Some(Direction::NW), &anchor_piece_string[1..]),
            ('/', _) => (Some(Direction::SW), &anchor_piece_string[1..]),
            (_, '/') => (Some(Direction::NE), &anchor_piece_string[..length - 1]),
            (_, '\\') => (Some(Direction::SE), &anchor_piece_string[..length - 1]),
            _ => (None, anchor_piece_string),
        };

        let (_, mut final_loc, _) = self
            .find(anchor_piece_string)
            .expect("Could not find anchor");
        if let Some(direction) = direction {
            final_loc = final_loc.apply(direction);
        }

        let mut new_grid = self.prev_grid.clone();

        match self.find(piece_string) {
            Some((piece, old_loc, height)) => {
                new_grid.remove(old_loc);
                debug_assert!(new_grid.peek(old_loc).len() == height);
                new_grid.add(piece, final_loc);
            }
            None => {
                new_grid.add(new_piece, final_loc);
            }
        }

        self.next_state(&new_grid)
    }

    /// Add a new state the annotator, representing a UHP move string with identifiers
    /// not appended to the mosquito, pillbug, ladybug and queen pieces. (e.g. wQ, bM, etc)
    ///
    /// The move must represent a legal Hive move from the last state of the board
    /// to the current state.
    ///
    /// Returns the resulting state of the annotator after the move is applied
    pub fn next_uhp_move(&self, move_string: &str) -> Result<Annotator> {
        let move_string = Annotator::uhp_to_standard(move_string);
        self.next_standard_move(&move_string)
    }

    /// Give a MoveString representation of the moves infered so far by this annotator.
    /// They are almost UHP compatible, expect for the fact that Queens, Mosquitos,
    /// Pillbugs and Ladybugs have ids appended to them. (e.g. wQ1, bM1, etc)
    pub fn standard_move_strings(&self) -> Vec<String> {
        self.moves.clone()
    }

    /// Convert a legal standard hive move to a UHP-compatible move string
    /// without unique identifiers appended to relevant pieces
    fn standard_to_uhp(move_string: &str) -> String {
        move_string
            .replace("P1", "P")
            .replace("M1", "M")
            .replace("L1", "L")
            .replace("Q1", "Q")
    }

    /// Convert a legal UHP-compatible move string to standard hive move string
    /// with unique identifiers appended to all the pieces
    fn uhp_to_standard(move_string: &str) -> String {
        move_string
            .replace("P", "P1")
            .replace("M", "M1")
            .replace("L", "L1")
            .replace("Q", "Q1")
    }

    /// Give a UHP-compatible MoveString representation of the moves
    /// infered so for by this annotator. If the game state started from
    /// an empty board and only legal moves were taken,
    /// then these strings will be correct according to the UHP.
    ///
    /// Queens, Mosquitos, Pillbugs and Ladybugs have no ids appended to them.
    /// (e.g. wQ, bM, etc)
    pub fn uhp_move_strings(&self) -> Vec<String> {
        self.standard_move_strings()
            .iter()
            .map(|move_string| Annotator::standard_to_uhp(move_string))
            .collect()
    }

    pub fn grid(&self) -> &HexGrid {
        &self.prev_grid
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
    assert_eq!(
        annotator.standard_move_strings(),
        vec![String::from("pass")]
    );
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
        annotator.standard_move_strings(),
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
        annotator.standard_move_strings(),
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

    let standard_moves = annotator.standard_move_strings();
    let possible_standard_moves = vec![
        vec![String::from("wB1")],
        vec![String::from("bA1 wB1-")],
        vec![String::from(r"wQ1 \wB1")],
        vec![String::from("bA1 -wQ1")],
        vec![String::from("wB1 bA1-"), String::from("wB1 wQ1")],
        vec![String::from("wB1 -wQ1"), String::from("wB1 bA1")],
        vec![String::from("wB1 /bA1")],
    ];
    assert!(possible_standard_moves.len() == standard_moves.len());
    for (expected, actual) in possible_standard_moves.iter().zip(standard_moves.iter()) {
        assert!(
            expected.contains(actual),
            "Expected {:?} to contain {:?}",
            expected,
            actual
        );
    }

    let uhp_moves = annotator.uhp_move_strings();
    let possible_uhp_moves = vec![
        String::from("wB1"),
        String::from("bA1 wB1-"),
        String::from(r"wQ \wB1"),
        String::from("bA1 -wQ"),
        String::from("wB1 bA1-"),
        String::from("wB1 -wQ"),
        String::from("wB1 /bA1"),
    ];
    assert!(possible_uhp_moves.len() == uhp_moves.len());
    for (expected, actual) in possible_uhp_moves.iter().zip(uhp_moves.iter()) {
        assert_eq!(expected, actual);
    }
}

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
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . S . .\n",
        ". . L . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . S . .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . m .\n",
        " . . S . .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . m .\n",
        " . . S G .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . 2 G .\n",
        ". . L p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "2 - [ S m ]\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single climb up should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . . .\n",
        " . . 3 G .\n",
        ". . . p .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single climb up should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . . 3 G .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single movement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . . 3 . .\n",
        ". . G . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Single movement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . . 3 G .\n",
        ". . G . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n",
    ));

    let result = annotator.next_state(&grid);
    annotator = result.expect("Second placement should be handled correctly");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . G 3 . .\n",
        ". . G . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "3 - [ S m L ]\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Pieces should be fungible even if moved");

    let grid = HexGrid::from_dsl(concat!(
        " . . . . .\n",
        ". . . p .\n",
        " . G 4 . .\n",
        ". . . . .\n",
        " . . . . .\n\n",
        "start - [ 0 0 ]\n\n",
        "4 - [ S m L G]\n",
    ));
    let result = annotator.next_state(&grid);
    annotator = result.expect("Pieces should be fungible even if climbing up");

    let moves = annotator.standard_move_strings();
    let possible_moves = vec![
        vec![String::from("wS1")],
        vec![String::from("wL1 /wS1")],
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
            String::from(r"bM1 wS1"),
        ],
        vec![
            String::from(r"wL1 \bP1"),
            String::from(r"wL1 -wG1"),
            String::from(r"wL1 bM1"),
        ],
        vec![
            String::from(r"bP1 wS1/"),
            String::from(r"bP1 bM1/"),
            String::from(r"bP1 wL1/"),
            String::from(r"bP1 \wG1"),
        ],
        vec![
            String::from(r"wG1 /wS1"),
            String::from(r"wG1 /bM1"),
            String::from(r"wG1 /wL1"),
        ],
        vec![
            String::from(r"wG2 wS1-"),
            String::from(r"wG2 bM1-"),
            String::from(r"wG2 wL1-"),
            String::from(r"wG2 bP1\"),
        ],
        vec![
            String::from(r"wG2 -wS1"),
            String::from(r"wG2 -bM1"),
            String::from(r"wG2 -wL1"),
            String::from(r"wG2 \wG1"),
        ],
        vec![
            String::from(r"wG1 wG2-"),
            String::from(r"wG1 /bP1"),
            String::from(r"wG1 wL1"),
        ],
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
        vec![String::from("wL /wS1")],
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
            String::from(r"bP wL/"),
            String::from(r"bP \wG1"),
        ],
        vec![
            String::from(r"wG1 /wS1"),
            String::from(r"wG1 /bM"),
            String::from(r"wG1 /wL"),
        ],
        vec![
            String::from(r"wG2 wS1-"),
            String::from(r"wG2 bM-"),
            String::from(r"wG2 wL-"),
            String::from(r"wG2 bP\"),
        ],
        vec![
            String::from(r"wG2 -wS1"),
            String::from(r"wG2 -bM"),
            String::from(r"wG2 -wL"),
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

#[test]
pub fn test_annotator_climb_across() {
    //TODO: climbing across distinct stacks
    let mut annotator = Annotator::new();
    let positions = vec![
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". . . . .\n",
            " . . S . .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". . . . .\n",
            " . . S b .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". . . . .\n",
            " . b S b .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". . . . .\n",
            " b b S b .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". . . . .\n",
            " . 2 S b .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
            "2 - [ b b ]\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". . . . .\n",
            " . 2 2 . .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
            "2 - [ b b ]\n",
            "2 - [ S b ]\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". . . . .\n",
            " . 3 S . .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
            "3 - [ b b b]\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . .\n",
            ". b . . .\n",
            " . 2 S . .\n",
            ". . . . .\n",
            " . . . . .\n\n",
            "start - [ 0 0 ]\n\n",
            "2 - [ b b ]\n",
        )),
    ];

    for grid in positions {
        let result = annotator.next_state(&grid);
        assert!(
            result.is_ok(),
            "Move should be legal, but got error {:?}",
            result
        );
        annotator = result.unwrap();
    }
}

#[test]
pub fn test_annotator_standard_move_string_interpretation() {
    let legal_moves = vec![
        String::from(r"wL1"),
        String::from(r"bL1 wL1\"),
        String::from(r"wA1 \wL1"),
        String::from(r"bM1 bL1\"),
        String::from(r"wQ1 /wA1"),
        String::from(r"bA1 /bL1"),
        String::from(r"wP1 \wQ1"),
        String::from(r"bQ1 bM1/"),
        String::from(r"wA1 bQ1-"),
        String::from(r"bM1 wA1-"),
    ];

    let positions = vec![
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . L . . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . L . . .\n",
            ". . . l . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . . .\n",
            " . . L . . .\n",
            ". . . l . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . A . . . .\n",
            " . . L . . . .\n",
            ". . . l . . .\n",
            " . . . m . . .\n",
            ". . . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . A . . . .\n",
            " . Q L . . . .\n",
            ". . . l . . .\n",
            " . . . m . . .\n",
            ". . . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". . A . . . .\n",
            " . Q L . . . .\n",
            ". . . l . . .\n",
            " . . a m . . .\n",
            ". . . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". P A . . . .\n",
            " . Q L . . . .\n",
            ". . . l . . .\n",
            " . . a m . . .\n",
            ". . . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". P A . . . .\n",
            " . Q L . . . .\n",
            ". . . l q . .\n",
            " . . a m . . .\n",
            ". . . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". P . . . . .\n",
            " . Q L . . . .\n",
            ". . . l q A .\n",
            " . . a m . . .\n",
            ". . . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . . .\n",
            ". P . . . . .\n",
            " . Q L . . . .\n",
            ". . . l q A m\n",
            " . . a . . . .\n",
            ". . . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
    ];

    let mut annotator = Annotator::new();
    for (move_string, grid) in legal_moves.iter().zip(positions.iter()) {
        let result = annotator.next_standard_move(move_string);
        assert!(
            result.is_ok(),
            "Move string {:?} should be legal, but got error {:?}",
            move_string,
            result
        );
        annotator = result.unwrap();
        assert!(
            annotator.grid() == grid,
            "Grids should be equal \nannotator:\n{}\ngrid:\n{}",
            annotator.grid().to_dsl(),
            grid.to_dsl()
        );
    }
}

#[test]
pub fn test_annotator_uhp_move_string_interpretation_with_climbing() {
    let legal_moves = vec![
        String::from(r"wL"),
        String::from(r"bP wL-"),
        String::from(r"wA1 \wL"),
        String::from(r"bB1 bP/"),
        String::from(r"wQ /wA1"),
        String::from(r"bQ bB1\"),
        String::from(r"wB1 wQ\"),
        String::from(r"bB1 bP"),  // Move atop the hive absolute notation
        String::from(r"wB1 wQ-"), // Move atop the hive with relative notation
        String::from(r"bB1 wB1"), // absolute again
        String::from(r"wA1 bQ-"),
        String::from(r"bB1 \wB1"),
    ];

    let positions = [
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . L . . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . . L p . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . . .\n",
            " . . L p . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . b .\n",
            " . . L p . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . b .\n",
            " . Q L p . .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . b .\n",
            " . Q L p q .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . b .\n",
            " . Q L p q .\n",
            ". . B . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . . .\n",
            " . Q L 2 q .\n",
            ". . B . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
            "2 - [ p b ]\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . . .\n",
            " . Q 2 2 q .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
            "2 - [ L B ]\n",
            "2 - [ p b ]\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . A . . .\n",
            " . Q 3 p q .\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
            "3 - [ L B b ]\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . . . . .\n",
            " . Q 3 p q A\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
            "3 - [ L B b ]\n",
        )),
        HexGrid::from_dsl(concat!(
            " . . . . . .\n",
            ". . b . . .\n",
            " . Q 2 p q A\n",
            ". . . . . .\n",
            " . . . . . .\n",
            ". . . . . .\n\n",
            "start - [-1 -2]\n\n",
            "2 - [ L B ]\n",
        )),
    ];

    let mut annotator = Annotator::new();
    for (move_string, grid) in legal_moves.iter().zip(positions.iter()) {
        let result = annotator.next_uhp_move(move_string);
        assert!(
            result.is_ok(),
            "Move string {:?} should be legal, but got error {:?}",
            move_string,
            result
        );
        annotator = result.unwrap();
        assert!(
            annotator.grid() == grid,
            "Grids should be equal \nannotator:\n{}\ngrid:\n{}",
            annotator.grid().to_dsl(),
            grid.to_dsl()
        );
    }
}

// TODO: prefer to return absolute move notation for climbing atop the hive
