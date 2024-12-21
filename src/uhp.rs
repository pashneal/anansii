use crate::constants::*;
use crate::game::*;
use crate::hex_grid::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum UHPError {
    #[error("Inference is not possible because the annotator invariant was violated")]
    InvariantError,
    #[error("Could not undo move")]
    UndoError,
    #[error("IllegalMove")]
    IllegalMove,
}

pub type Result<T> = std::result::Result<T, UHPError>;
pub type CommandResult = std::result::Result<String, String>;
type StackIds = Vec<Option<u8>>;

/// Responsible for annotation of moves in UHP format
/// given a board history fed into the annotator state by state
///
/// Invariant : The Annotator assumes that Hive pieces can only be placed or moved
/// but never removed from the board. Furthermore, it assumes that piece movements observe the
/// One Hive Rule. If this is the case, all legal moves will be annotated correctly as
/// UHP MoveStrings.
///
/// valid changes to the Annotator from state to state are as follows:
///
/// - pass : no additions or removals made from the board
/// - placement : a single piece is added to top of the board which did not previously exist
/// - movement : a single piece is moved from the top of one location to top of another. The resulting board must have greater than 1 piece on the board's lowest level
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
    /// Move strings (not necessarily UHP-compatible)
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
            moves: Vec::new(),
        }
    }

    /// Computes differences between the last state fed into this Annotator
    /// via next_state() and the given current_grid.
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
    /// destination - the piece's new location after moving
    /// hex_grid - the newest state of the hexgrid
    /// ids - mappings from the newest state of the board to unique identifiers for each piece
    fn anchor_reference(
        hex_grid: &HexGrid,
        destination: HexLocation,
        ids: &HashMap<HexLocation, StackIds>,
    ) -> String {
        let stack = hex_grid.peek(destination);
        debug_assert!(!stack.is_empty(), "There should be a piece here!");
        if stack.len() > 1 {
            // If the piece climbs atop the hive, use the piece below it as the anchor
            let piece_below = stack[stack.len() - 2];
            let id_below = ids
                .get(&destination)
                .unwrap()
                .get(stack.len() - 2)
                .unwrap()
                .unwrap();
            return piece_below.to_uhp(id_below);
        }

        let nw = destination.apply(Direction::NW);
        let sw = destination.apply(Direction::SW);
        let ne = destination.apply(Direction::NE);
        let se = destination.apply(Direction::SE);
        let e = destination.apply(Direction::E);
        let w = destination.apply(Direction::W);

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

        return if !hex_grid.peek(nw).is_empty() {
            let piece = hex_grid.peek(nw);
            let piece = piece.last().unwrap();
            let id = ids.get(&nw).unwrap().last().unwrap().unwrap();

            relative_direction(Direction::NW, &piece.to_uhp(id))
        } else if !hex_grid.peek(sw).is_empty() {
            let piece = hex_grid.peek(sw);
            let piece = piece.last().unwrap();
            let id = ids.get(&sw).unwrap().last().unwrap().unwrap();

            relative_direction(Direction::SW, &piece.to_uhp(id))
        } else if !hex_grid.peek(ne).is_empty() {
            let piece = hex_grid.peek(ne);
            let piece = piece.last().unwrap();
            let id = ids.get(&ne).unwrap().last().unwrap().unwrap();

            relative_direction(Direction::NE, &piece.to_uhp(id))
        } else if !hex_grid.peek(se).is_empty() {
            let piece = hex_grid.peek(se);
            let piece = piece.last().unwrap();
            let id = ids.get(&se).unwrap().last().unwrap().unwrap();

            relative_direction(Direction::SE, &piece.to_uhp(id))
        } else if !hex_grid.peek(e).is_empty() {
            let piece = hex_grid.peek(e);
            let piece = piece.last().unwrap();
            let id = ids.get(&e).unwrap().last().unwrap().unwrap();

            relative_direction(Direction::E, &piece.to_uhp(id))
        } else if !hex_grid.peek(w).is_empty() {
            let piece = hex_grid.peek(w);
            let piece = piece.last().unwrap();
            let id = ids.get(&w).unwrap().last().unwrap().unwrap();

            relative_direction(Direction::W, &piece.to_uhp(id))
        } else {
            panic!("No reference found, some invariant was violated!");
        };
    }

    /// Given a pair of differences representing a piece being removed and added
    /// to a destination on the resulting *grid*, return a new annotator
    /// that accounts for this move.
    fn piece_moved(&self, removed: &Diff, added: &Diff, grid: &HexGrid) -> Annotator {
        let Diff::Removed {
            loc: old_loc,
            piece: _,
            height: old_height,
        } = removed
        else {
            panic!("Expected a removed piece");
        };

        let Diff::Added {
            loc: new_loc,
            piece: new_piece,
            height: new_height,
        } = added
        else {
            panic!("Expected an added piece");
        };

        let mut new_ids = self.ids.clone();
        let ids = new_ids
            .get_mut(old_loc)
            .expect("valid state should identify all pieces");
        // Should be the top of the stack
        debug_assert!(ids.len() == old_height + 1);

        let id = ids[*old_height];
        ids.remove(*old_height);

        let ids = new_ids.entry(*new_loc).or_default();

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
            moves,
        }
    }

    /// Given a Diff representing a single piece that was placed from a player's
    /// hand, and return the new state of the annotator after this move was made
    fn piece_placed(&self, position: &Diff, grid: &HexGrid) -> Annotator {
        let mut new_piece_counts = self.piece_counts.clone();
        let Diff::Added { loc, piece, height } = position else {
            panic!("Expected an added piece in piece_placed()");
        };

        let mut new_ids = self.ids.clone();
        let ids = new_ids.entry(*loc).or_default();

        let new_id = new_piece_counts.entry(*piece).or_insert(0);
        *new_id += 1;

        ids.push(Some(*new_id));
        // Make sure the piece is added to the top of the stack
        assert!(ids.len() == height + 1);

        let piece_string = piece.to_uhp(*new_id);

        let move_string = if self.prev_grid.is_empty() {
            piece_string.to_string()
        } else {
            let anchor = Annotator::anchor_reference(grid, *loc, &new_ids);
            format!("{} {}", piece_string, anchor)
        };

        let mut moves = self.moves.clone();
        moves.push(move_string);

        Annotator {
            ids: new_ids,
            piece_counts: new_piece_counts,
            prev_grid: grid.clone(),
            moves,
        }
    }

    /// Add a new state to the annotator, representing a single legal move taken
    /// from the last state of the board to the current state of the board.
    pub fn next_state(&self, current_grid: &HexGrid) -> Result<Annotator> {
        let mut total_count = 0;
        for count in self.piece_counts.values() {
            total_count += count;
        }

        let diffs = self.get_differences(current_grid);

        // Happy paths:
        // 0 diffs
        // 1 diff and it's a piece placement
        // 2 diffs and it's removing the same piece from one location and adding it to another

        // 0 diffs happy path
        if diffs.is_empty() {
            let mut moves = self.moves.clone();
            moves.push("pass".to_string());

            let annotator = Annotator {
                ids: self.ids.clone(),
                piece_counts: self.piece_counts.clone(),
                prev_grid: current_grid.clone(),
                moves,
            };

            return Ok(annotator);
        }

        // 1 diffs happy path
        if diffs.len() == 1 {
            let diff = &diffs[0];
            return match diff {
                Diff::Added { .. } => Ok(self.piece_placed(diff, current_grid)),
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
                _ => Err(UHPError::IllegalMove)?,
            };

            return Ok(self.piece_moved(removed, added, current_grid));
        }

        // If we have more than 2 diffs, we can't infer the state
        println!("Unhappy path diffs: {:?}", diffs);
        println!("Unhappy position state: \n{}", current_grid.to_dsl());
        println!("Unhappy previous state: \n{}", self.prev_grid.to_dsl());
        Err(UHPError::InvariantError)
    }

    /// Returns the destination location of the last move recorded by the annotator.
    pub fn last_move(&self) -> Option<HexLocation> {
        if self.moves.is_empty() {
            return None;
        }

        let last_move = self.moves.last()?;
        if last_move == "pass" {
            return None;
        }

        let mut parts = last_move.split_whitespace();
        let piece = parts.next()?;
        let (_, location, _) = self.find(piece).expect("Expected a piece");

        Some(location)
    }

    /// Assuming an valid annotator, find the piece, location and height
    /// uniquely described by the given piece string.
    ///
    /// examples of piece strings are "wQ1", "bM1", etc
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

    /// Add a new state to the annotator, representing a "standard" move string with unique
    /// identifiers appended to all pieces. (e.g. wQ1, bM1, etc).
    ///
    /// Preserves the input move's text verbatim to be accessed later by the
    /// standard_move_strings() or uhp_move_strings() functions.
    ///
    /// The move must represent a legal Hive move from the last state of the annotator.
    ///
    /// Returns the resulting state of the annotator after the move is applied.
    pub fn next_standard_move(&self, move_string: &str) -> Result<Annotator> {
        debug_assert!(move_string.trim() == move_string);

        if move_string == "pass" {
            return self.next_state(&self.prev_grid);
        }

        let mut parts = move_string.split_whitespace();

        let piece_string = parts.next().expect("Expected a piece");
        let new_piece = Piece::from_uhp(piece_string).expect("Expected a valid piece");

        if move_string.len() <= 3 {
            let mut new_grid = HexGrid::new();
            new_grid.add(new_piece, HexLocation::new(0, 0));
            return self.next_state(&new_grid);
        }

        let anchor_piece_string = parts.next().expect("Expected an anchor position");

        let length = anchor_piece_string.len();

        let first_last = (
            anchor_piece_string.chars().next().unwrap(),
            anchor_piece_string.chars().last().unwrap(),
        );

        // direction either at front, end or neither
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

        // Account for either a existing piece being moved or one being placed
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

        let result = self.next_state(&new_grid);

        // Replace last move with verbatim move string so annotator
        // uhp move strings are predicable given string input
        result.map(|annotator| {
            let mut moves = annotator.moves.clone();
            moves.pop();
            moves.push(move_string.to_string());
            Annotator { moves, ..annotator }
        })
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

    /// Convert a legal "standard" hive move to a UHP-compatible move string
    /// without unique identifiers appended to relevant pieces
    fn standard_to_uhp(move_string: &str) -> String {
        move_string
            .replace("P1", "P")
            .replace("M1", "M")
            .replace("L1", "L")
            .replace("Q1", "Q")
    }

    /// Convert a legal UHP-compatible move string to "standard" hive move string
    /// with unique identifiers appended to all the pieces
    fn uhp_to_standard(move_string: &str) -> String {
        move_string
            .replace('P', "P1")
            .replace('M', "M1")
            .replace('L', "L1")
            .replace('Q', "Q1")
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

    pub fn annotate(&self, position: &HexGrid) -> Result<String> {
        let next = self.next_state(position)?;
        next.uhp_move_strings()
            .last()
            .cloned()
            .ok_or(UHPError::InvariantError)
    }

    pub fn position(&self) -> &HexGrid {
        &self.prev_grid
    }
}

#[derive(Copy, Clone, Debug)]
pub enum GameType {
    Standard,
    M,
    L,
    P,
    ML,
    MP,
    LP,
    MLP,
}

impl GameType {
    pub fn to_str(&self) -> &str {
        match self {
            GameType::Standard => "Base",
            GameType::M => "Base+M",
            GameType::L => "Base+L",
            GameType::P => "Base+P",
            GameType::ML => "Base+ML",
            GameType::MP => "Base+MP",
            GameType::LP => "Base+LP",
            GameType::MLP => "Base+MLP",
        }
    }
}

pub struct UHPInterface {
    annotations: Vec<Annotator>,
    game_type: GameType,
    game: GameDebugger,
    player_to_move: PieceColor,
}

impl UHPInterface {
    pub fn new() -> UHPInterface {
        UHPInterface {
            annotations: vec![Annotator::new()],
            game_type: GameType::Standard,
            game: GameDebugger::from_moves(&[]).unwrap(),
            player_to_move: PieceColor::White,
        }
    }

    fn info(&self) -> CommandResult {
        Ok("id ".to_string() + ENGINE_NAME + " v" + VERSION + "\n" + "Mosquito;Ladybug;Pillbug")
    }

    fn unknown(&self) -> CommandResult {
        Err("Unknown command, cannot parse".to_string())
    }

    /// Parse a GameTypeString (see Universal Hive Protocol wiki)
    /// and set the game type accordingly
    fn set_game_type(&mut self, input: &str) -> CommandResult {
        self.game_type = match input {
            "Base" => GameType::Standard,
            "Base+M" => GameType::M,
            "Base+L" => GameType::L,
            "Base+P" => GameType::P,
            "Base+ML" | "Base+LM" => GameType::ML,
            "Base+MP" | "Base+PM" => GameType::MP,
            "Base+LP" | "Base+PL" => GameType::LP,
            "Base+MLP" | "Base+MPL" | "Base+LMP" | "Base+LPM" | "Base+PML" | "Base+PLM" => {
                GameType::MLP
            }
            _ => return Err("Unable to interpret GameTypeString".to_string()),
        };

        // Also update underlying move generator
        self.game = GameDebugger::from_moves_custom(
            &self.annotations.last().unwrap().uhp_move_strings(),
            self.game_type,
        )
        .unwrap();

        Ok("".to_string())
    }

    /// Parse a newgame command,
    /// commands must be of the form:
    ///
    /// newgame
    /// newgame GameTypeString
    /// newgame GameString
    ///
    /// See the Universal Hive Protocol wiki for more information
    fn new_game(&mut self, input: &str) -> CommandResult {
        self.annotations = vec![Annotator::new()];
        self.player_to_move = PieceColor::White;
        if input == "newgame" {
            self.set_game_type("Base")?;
            return Ok(self.game_string());
        }

        if input.len() < 10 {
            return Err("Invalid game type".to_string());
        }

        if !input.contains(';') {
            self.set_game_type(&input[8..])?;
        } else {
            let rest = input[8..].to_string();
            let mut delimited = rest.split(';');

            let base = delimited
                .next()
                .ok_or_else(|| String::from("Invalid GameString"))?;
            self.set_game_type(base)?;

            let game_state = delimited
                .next()
                .ok_or_else(|| String::from("Invalid GameString"))?;

            let turn_string = delimited
                .next()
                .ok_or_else(|| String::from("Invalid GameString"))?;

            if turn_string.len() < 8 {
                return Err("Expected TurnString at position 2 of GameString".to_string());
            }

            let num = turn_string[6..turn_string.len() - 1].to_string();
            let _ = num
                .parse::<u8>()
                .map_err(|_| "Expected number at position 2 of GameString")?;

            for move_string in delimited {
                self.make_move(move_string)?;
            }

            debug_assert!(
                game_state == self.game_result(),
                "Game did not end as expected"
            );
        }

        Ok(self.game_string())
    }

    fn game_result(&self) -> &str {
        let result = self.game.game_result();

        match (self.annotations.len(), result) {
            (1, None) => "NotStarted",
            (_, None) => "InProgress",
            (_, Some(GameResult::Draw)) => "Draw",
            (_, Some(GameResult::WhiteWins)) => "WhiteWins",
            (_, Some(GameResult::BlackWins)) => "BlackWins",
        }
    }
    /// Returns the current GameString according to the Universal Hive Protocol
    /// wiki
    fn game_string(&self) -> String {
        let turn_number = ((self.annotations.len() - 1) / 2) + 1;
        let moves = self
            .annotations
            .last()
            .unwrap()
            .uhp_move_strings()
            .join(";");
        let game_type = self.game_type.to_str();
        let color = self.player_to_move.to_str();
        let game_result = self.game_result();
        if game_result == "NotStarted" {
            return format!("{};{};{}[{}]", game_type, game_result, color, turn_number);
        }
        format!(
            "{};{};{}[{}];{}",
            game_type, game_result, color, turn_number, moves
        )
    }

    /// Parses a move string in the UHP protocol form and stores
    /// it directly for construction of the GameString. Checks the that
    /// move is legal assuming self.game_type is updated
    fn make_move(&mut self, move_string: &str) -> CommandResult {
        let annotator = self.annotations.last().unwrap();

        let annotator = annotator
            .next_uhp_move(move_string)
            .map_err(|e| e.to_string())?;

        self.game
            .make_move(move_string)
            .map_err(|e| e.to_string())?;
        self.annotations.push(annotator);
        self.player_to_move = self.player_to_move.opposite();
        Ok(self.game_string())
    }

    /// Parses a play command in the UHP protocol form and stores
    /// the move string directly for access in the GameString
    fn play(&mut self, input: &str) -> CommandResult {
        if input.len() < 7 {
            return Err("Invalid move string for play command".to_string());
        }

        self.make_move(input[5..].trim())
    }

    /// Returns a list of all valid moves for the current player
    /// in compliance with the UHP protocol
    fn valid_moves(&mut self) -> CommandResult {
        let positions = self.game.legal_positions();
        let annotator = self.annotations.last().unwrap();

        let mut moves = vec![];
        for position in positions {
            let move_string = annotator
                .annotate(&position)
                .map_err(|_| "Cannot create move string")?;
            moves.push(move_string);
        }

        Ok(moves.join(";"))
    }

    fn pass(&mut self) -> CommandResult {
        self.make_move("pass")
    }

    fn best_move(&mut self, _input: &str) -> CommandResult {
        todo!()
    }

    /// Undoes a single move and updates the game state if possible
    fn undo_one(&mut self) -> CommandResult {
        if self.annotations.len() == 1 {
            return Err("Cannot undo past the first move".to_string());
        }
        self.annotations.pop();
        self.player_to_move = self.player_to_move.opposite();
        self.game.undo_move().map_err(|e| e.to_string())?;
        Ok(self.game_string())
    }

    /// Undo command,
    /// Must be one of the following forms
    ///
    /// undo
    /// undo number
    ///
    /// See the Universal Hive Protocol wiki for more information
    fn undo(&mut self, input: &str) -> CommandResult {
        if input == "undo" {
            return self.undo_one();
        }

        let num = input[5..]
            .trim()
            .parse::<u8>()
            .map_err(|_| "Invalid number for undo command".to_string())?;
        if num as usize >= self.annotations.len() {
            return Err("Cannot undo past the first move".to_string());
        }
        if num < 1 {
            return Err("Invalid number for undo command".to_string());
        }

        let mut game_string = Ok("".to_string());
        for _ in 0..num {
            game_string = self.undo_one();
        }
        game_string
    }

    fn options(&mut self, _input: &str) -> CommandResult {
        todo!()
    }

    pub fn current_position(&self) -> &HexGrid {
        self.annotations
            .last()
            .expect("There should always be a current position")
            .position()
    }

    /// Parses commands according to the Universal Hive Protocol
    /// and returns the appropriate response
    ///
    /// All responses end with "ok\n"
    ///
    /// If the command encounters an error, the string returned will
    /// begin with "err" in accordance with the UHP
    pub fn command(&mut self, input: &str) -> String {
        let response = match input.trim() {
            "info" => self.info(),
            "validmoves" => self.valid_moves(),
            "pass" => self.pass(),
            a if a.starts_with("bestmove") => self.best_move(a),
            a if a.starts_with("newgame") => self.new_game(a),
            a if a.starts_with("play") => self.play(a),
            a if a.starts_with("undo") => self.undo(a),
            a if a.starts_with("options") => self.options(a),
            _ => self.unknown(),
        };
        let response = match response {
            Ok(response) => response,
            Err(response) => "err ".to_string() + &response,
        };

        debug_assert!(
            if !response.is_empty() {
                !response.ends_with('\n')
            } else {
                true
            },
            "Non-empty response should not end with a newline"
        );
        if response.is_empty() {
            "ok\n".to_string()
        } else {
            response + "\nok\n"
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Annotator;
    use super::UHPInterface;
    use super::*;

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
        let possible_standard_moves = [
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
        let possible_uhp_moves = [
            vec![String::from("wB1")],
            vec![String::from("bA1 wB1-")],
            vec![String::from(r"wQ \wB1")],
            vec![String::from("bA1 -wQ")],
            vec![String::from("wB1 bA1-"), String::from("wB1 wQ")],
            vec![String::from("wB1 -wQ"), String::from("wB1 bA1")],
            vec![String::from("wB1 /bA1")],
        ];
        assert!(possible_uhp_moves.len() == uhp_moves.len());
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
        assert!(possible_moves.len() == moves.len());

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
                String::from(r"bM wS1"),
            ],
            vec![
                String::from(r"wL \bP"),
                String::from(r"wL -wG1"),
                String::from(r"wL bM"),
            ],
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
            vec![
                String::from(r"wG1 wG2-"),
                String::from(r"wG1 /bP"),
                String::from(r"wG1 wL"),
            ],
        ];
        assert!(possible_uhp_moves.len() == uhp_moves.len());

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
                "Move input {:?} should be legal, but got error {:?}",
                move_string,
                result
            );
            annotator = result.unwrap();
            assert!(
                annotator.position() == grid,
                "Grids should be equal \nannotator:\n{}\ngrid:\n{}",
                annotator.position().to_dsl(),
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
                "Move input {:?} should be legal, but got error {:?}",
                move_string,
                result
            );
            annotator = result.unwrap();
            assert!(
                annotator.position() == grid,
                "Grids should be equal \nannotator:\n{}\ngrid:\n{}",
                annotator.position().to_dsl(),
                grid.to_dsl()
            );
        }
    }

    #[test]
    pub fn test_uhp_interface_newgame() {
        let mut uhp = UHPInterface::new();
        let result = uhp.command("newgame");
        assert_eq!(result, "Base;NotStarted;White[1]\nok\n")
    }

    #[test]
    pub fn test_uhp_interface_newgame_game_type_strings() {
        let mut uhp = UHPInterface::new();
        let result = uhp.command("newgame Base+M");
        assert_eq!(result, "Base+M;NotStarted;White[1]\nok\n");
        let result = uhp.command("newgame Base+ML");
        assert!(
            result == "Base+ML;NotStarted;White[1]\nok\n"
                || result == "Base+LM;NotStarted;White[1]\nok\n"
        );

        let result = uhp.command("newgame Base+LM");
        assert!(
            result == "Base+LM;NotStarted;White[1]\nok\n"
                || result == "Base+ML;NotStarted;White[1]\nok\n"
        );
        let result = uhp.command("newgame Base+MLP");
        assert!(
            result == "Base+MLP;NotStarted;White[1]\nok\n"
                || result == "Base+MPL;NotStarted;White[1]\nok\n"
                || result == "Base+PLM;NotStarted;White[1]\nok\n"
                || result == "Base+PML;NotStarted;White[1]\nok\n"
                || result == "Base+LMP;NotStarted;White[1]\nok\n"
                || result == "Base+LPM;NotStarted;White[1]\nok\n"
        );
    }

    #[test]
    pub fn test_uhp_interface_some_moves() {
        let moves = [
            r"wL", r"bP wL-", r"wA1 \wL", r"bB1 bP/", r"wQ /wA1", r"bQ bB1\",
        ];
        let final_position = HexGrid::from_dsl(concat!(
            ". . . . . .\n",
            " . A . b . .\n",
            ". Q L p q .\n",
            " . . . . . .\n\n",
            "start - [ -1 -2 ]\n\n"
        ));

        let moves = moves.join(";");
        let newgame = format!("newgame Base+LP;InProgress;White[4];{}", moves);

        let mut uhp = UHPInterface::new();
        let output = uhp.command(&newgame);

        println!("OUTPUT: {}", output);
        assert!(
            output == format!("Base+LP;InProgress;White[4];{}\nok\n", moves)
                || output == format!("Base+PL;InProgress;White[4];{}\nok\n", moves)
        );
        println!("{}", uhp.current_position().to_dsl());
        println!("{}", final_position.to_dsl());
        assert!(*uhp.current_position() == final_position);
    }

    #[test]
    pub fn test_uhp_interface_play() {
        let moves = [
            r"wL", r"bP wL-", r"wA1 \wL", r"bB1 bP/", r"wQ /wA1", r"bQ bB1\",
        ];
        let final_position = HexGrid::from_dsl(concat!(
            ". . . . . .\n",
            " . A . b . .\n",
            ". Q L p q .\n",
            " . . . . . .\n\n",
            "start - [ -1 -2 ]\n\n"
        ));

        let mut uhp = UHPInterface::new();
        uhp.command("newgame Base+PL");

        for (i, move_string) in moves.iter().enumerate() {
            let turn_number = (i + 1) / 2 + 1;
            let color = match (i + 1) % 2 {
                0 => "White",
                _ => "Black",
            };
            let output = uhp.command(&format!("play {}", move_string));
            let moves = moves
                .iter()
                .take(i + 1)
                .cloned()
                .collect::<Vec<&str>>()
                .join(";");

            println!("GOT TO THIS OUTPUT {}", output);
            assert!(
                output
                    == format!(
                        "Base+PL;InProgress;{}[{}];{}\nok\n",
                        color, turn_number, moves
                    )
                    || output
                        == format!(
                            "Base+LP;InProgress;{}[{}];{}\nok\n",
                            color, turn_number, moves
                        )
            );
        }

        assert!(*uhp.current_position() == final_position);
    }

    #[test]
    pub fn test_uhp_interface_undo() {
        let moves = [
            r"wL", r"bP wL-", r"wA1 \wL", r"bB1 bP/", r"wQ /wA1", r"bQ bB1\",
        ];

        let mut uhp = UHPInterface::new();
        uhp.command("newgame Base+PML;NotStarted;White[1]");
        uhp.command(&format!("play {}", moves[0]));
        uhp.command(&format!("play {}", moves[1]));
        uhp.command(&format!("play {}", moves[2]));
        uhp.command(&format!("play {}", moves[3]));
        let output = uhp.command("undo");
        assert_eq!(
            &output[8..],
            ";InProgress;Black[2];wL;bP wL-;wA1 \\wL\nok\n"
        );

        let output = uhp.command("undo");
        assert_eq!(&output[8..], ";InProgress;White[2];wL;bP wL-\nok\n");

        uhp.command(&format!("play {}", moves[2]));
        let output = uhp.command("undo 2");
        assert_eq!(&output[8..], ";InProgress;Black[1];wL\nok\n");
    }

    #[test]
    pub fn test_game_states_output() {
        let draw_before = r"Base;InProgress;Black[6];wA1;bA1 wA1-;wQ -wA1;bQ bA1-;wQ \wA1;bQ bA1/;wQ -wA1;bQ bA1-;wQ /wA1;bQ bA1\;wQ -wA1";
        let draw_last_move = r"bQ bA1-";
        let draw_complete = r"Base;Draw;White[7];wA1;bA1 wA1-;wQ -wA1;bQ bA1-;wQ \wA1;bQ bA1/;wQ -wA1;bQ bA1-;wQ /wA1;bQ bA1\;wQ -wA1;bQ bA1-";

        let black_wins_before = r"Base+LMP;InProgress;White[8];wP;bL wP-;wB1 \wP;bQ bL/;wA1 /wB1;bA1 \bQ;wQ wA1\;bA2 bQ/;wB1 wP;bA1 /wA1;wB1 wP\;bA2 bA1\;wA2 \wP;bA3 bQ\";
        let black_wins_last_move = r"wA2 wQ\";
        let black_wins_complete = r"Base+LMP;BlackWins;Black[8];wP;bL wP-;wB1 \wP;bQ bL/;wA1 /wB1;bA1 \bQ;wQ wA1\;bA2 bQ/;wB1 wP;bA1 /wA1;wB1 wP\;bA2 bA1\;wA2 \wP;bA3 bQ\;wA2 wQ\";

        let white_wins_before = r"Base+PL;InProgress;White[7];wP;bL wP-;wB1 \wP;bQ bL/;wA1 /wB1;bA1 \bQ;wQ wA1\;bB1 bQ/;wB1 wP;bG1 bB1\;wA1 bQ\;bG2 bG1/";
        let white_wins_last_move = r"wB1 \bL";
        let white_wins_complete = r"Base+PL;WhiteWins;Black[7];wP;bL wP-;wB1 \wP;bQ bL/;wA1 /wB1;bA1 \bQ;wQ wA1\;bB1 bQ/;wB1 wP;bG1 bB1\;wA1 bQ\;bG2 bG1/;wB1 \bL";

        let mut uhp = UHPInterface::new();

        uhp.command(&format!("newgame {}", draw_before));
        let output = uhp.command(&format!("play {}", draw_last_move));
        println!("{}", output);
        println!("{}", format!("{}\nok\n", draw_complete));
        assert!(output == format!("{}\nok\n", draw_complete));

        uhp.command(&format!("newgame {}", black_wins_before));
        let output = uhp.command(&format!("play {}", black_wins_last_move));
        let output = output.to_string();
        let black_wins_complete = black_wins_complete.to_string();
        println!("{}", &output[8..]);
        println!("{}", &format!("{}\nok\n", black_wins_complete)[8..]);
        assert!(output[8..] == format!("{}\nok\n", black_wins_complete)[8..]);

        uhp.command(&format!("newgame {}", white_wins_before));
        let output = uhp.command(&format!("play {}", white_wins_last_move));
        println!("{}", output);
        println!("{}", format!("{}\nok\n", white_wins_complete));
        assert!(output[7..] == format!("{}\nok\n", white_wins_complete)[7..]);
    }

    #[test]
    pub fn test_game_states_input() {
        let mut uhp = UHPInterface::new();
        let draw_game_string = r"Base;Draw;White[7];wA1;bA1 wA1-;wQ -wA1;bQ bA1-;wQ \wA1;bQ bA1/;wQ -wA1;bQ bA1-;wQ /wA1;bQ bA1\;wQ -wA1;bQ bA1-";
        let output = uhp.command(&format!("newgame {}", draw_game_string));
        println!("{}", output);
        println!("{}", format!("{}\nok\n", draw_game_string));
        assert!(output == format!("{}\nok\n", draw_game_string));

        let black_wins = r"Base+LMP;BlackWins;Black[8];wP;bL wP-;wB1 \wP;bQ bL/;wA1 /wB1;bA1 \bQ;wQ wA1\;bA2 bQ/;wB1 wP;bA1 /wA1;wB1 wP\;bA2 bA1\;wA2 \wP;bA3 bQ\;wA2 wQ\";
        let output = uhp.command(&format!("newgame {}", black_wins));
        println!("{}", output);
        println!("{}", format!("{}\nok\n", black_wins));
        assert!(output[8..] == format!("{}\nok\n", black_wins)[8..]);

        let white_wins = r"Base+PL;WhiteWins;Black[7];wP;bL wP-;wB1 \wP;bQ bL/;wA1 /wB1;bA1 \bQ;wQ wA1\;bB1 bQ/;wB1 wP;bG1 bB1\;wA1 bQ\;bG2 bG1/;wB1 \bL";
        let output = uhp.command(&format!("newgame {}", white_wins));
        println!("{}", output);
        println!("{}", format!("{}\nok\n", white_wins));
        assert!(output[7..] == format!("{}\nok\n", white_wins)[7..]);
    }

    #[ignore = "be sure to manually test using nokamute uhp test suite"]
    #[test]
    pub fn test_valid_moves() {
        //Note: we use nokamute for testing!
    }
}
