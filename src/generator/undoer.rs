use super::*;
use crate::generator::mini::*;
use crate::bitgrid::mini::*;
use crate::bitgrid::gates::*;
use crate::bitgrid::board::AxialBitboard;
use crate::generator::*;
use crate::hex_grid::*;
use crate::uhp::{GameType, Annotator};
use std::hint::black_box;
use rand::{rngs::ChaCha8Rng, SeedableRng};


use rand::{self, RngExt};


// Undoer does the following, make a bunch of random legal moves, 
// undoes them, and checking the move generator after each undo 
// or move. And keep going for a bunch of iterations. 
//
// TODO: we may want to rewind, then fast forward, then rewind 
// randomly so that we can exercise the MiniGenerator 
// internal state in a more representative way. For 
// now, we just bias in the forward direction.
//
// This should give a nice sample that is a representative workload for our 
// use case - move generation in the context of backtracking search.
//
//
// To isolate the performance of just the single piece type under test
// we first construct a list of legal moves and "undo"s by 
// using the move generator and not timing it, *then* with 
// a fresh copy we time just the piece function (for example queen_moves()) 
// with all the cached positions. 
pub struct Undoer{
    generator: MiniGenerator,
} 

impl Undoer {
    pub fn new() -> Self {
        let hex_grid = HexGrid::new();
        let generator = MiniGenerator::from_hex_grid(
            &hex_grid,
            GameType::MLP,
            None,
        );
        Undoer { generator }
    }

    /// Randomly generates a sequence of a legal changes 
    /// that mutates the empty start position only in Hive-legal ways
    /// according to the MiniGenerator.
    ///
    /// length - the number of changes to generate 
    pub fn untimed_run(&mut self, length: usize, seed: u64) -> Vec<Change> {
        let mut moves = Vec::new();
        let mut rng = ChaCha8Rng::seed_from_u64(seed);

        for i in 0..=length {

            let color = match i % 2 {
                0 => PieceColor::White,
                _ => PieceColor::Black,
            }; 

            let positions = self.generator.generate_positions_for(color);
            let mut vec_positions = positions.into_iter().collect::<Vec<_>>();
            // TODO: could make this bench faster by sorting
            // by a more realistic key
            vec_positions.sort_by_key(|x| x.to_hex_grid().to_dsl());

            let r = rng.random::<i32>().abs() as usize % vec_positions.len(); 
            let random_position = &vec_positions[r];

            let change = Differ::single_diff(
                &self.generator.current_grid(), 
                random_position
            ).expect("could not infer change");

            moves.push(change);

            self.generator.apply(change);
        }

        for change in moves.clone().iter().rev() {
            self.generator.undo(*change)
        }

        moves
    }

    pub fn warmup_gates(&mut self) {
        gated_neighbors(
            AxialBitboard(0x0000000000), 
            MiniBitGridLocation {board_index: 0, mask: 0x0000020000}
        );
    }

    // Given a piece, and a set of moves, track the presence
    // of a certain piece and return a location of the piece
    // on the board (if any of the piece are present).
    pub fn track_piece(&mut self, moves: Vec<Change>, piece: Piece) -> Vec<(Change, Option<MiniBitGridLocation>)>{
        let mut tracked = Vec::new();
        for change in moves.iter() {
            self.generator.apply(*change);
            let found_piece = self.generator.find_piece(piece);
            tracked.push((change.clone(), found_piece))
        }
        for change in moves.iter().rev() {
            self.generator.undo(*change);
        }
        return tracked;
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by anticipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn queen_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().queen_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by anticipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn beetle_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().beetle_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by anticipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn pillbug_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().pillbug_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by anticipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn grasshopper_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().grasshopper_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by anticipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn ant_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().ant_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by ladybugicipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn ladybug_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().ladybug_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by mosquitoicipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn mosquito_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().mosquito_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }

    // NOTE: Be sure to initialize with a new MiniGenerator/Undoer so that 
    // the implementor can't cheat the benchmark by anticipating
    // the moves. Minor risk but adds more validity to the benchmark.
    pub fn spider_run(&mut self, tracked_changes: Vec<(Change, Option<MiniBitGridLocation>)>) {
        for (change, piece_location) in tracked_changes.iter() {
            self.generator.apply(*change);
            if let Some(location) = piece_location {
                black_box(self.generator.current_grid().spider_moves(*location));
            }
        }

        for (change, _) in tracked_changes.iter().rev() {
            self.generator.undo(*change);
        }
    }
}


#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    pub fn test_sanity_check() {
        let mut undoer = Undoer::new();
        let moves = undoer.untimed_run(10, 46);
        let tracked = undoer.track_piece(
            moves.clone(), 
            Piece::new(PieceType::Queen, PieceColor::White)
        );
        undoer.queen_run(tracked);
        assert!(true);
    }
}
    
