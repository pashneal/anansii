#[cfg(test)]

mod tests {
    use super::*;
    use crate::generator::mini::*;
    use crate::generator::*;
    use crate::hex_grid::*;
    use crate::uhp::{GameType, Annotator};

    use criterion::{criterion_group, criterion_main, Criterion};
    use rand::{self, RngExt};


    // Undoer does the following, make 20 random legal moves, 
    // undo 20, checking the move generator after each undo 
    // or move. And keep going for 100 iterations.
    //
    // This should give a nice sample that is a representative workload for our use case 
    // - move generation in the context of backtracking search.
    //
    //
    // To isolate the performance of just the single piece type under test
    // we first construct a list of legal moves and undos by 
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
        fn run(&mut self, length: usize) -> Vec<Change> {
            let mut moves = Vec::new();
            for i in 0..=length {

                let color = match i % 2 {
                    0 => PieceColor::White,
                    _ => PieceColor::Black,
                }; 

                let positions = self.generator.generate_positions_for(color);
                let vec_positions = positions.into_iter().collect::<Vec<_>>();

                let mut rng = rand::rng();

                let random_position = &vec_positions[
                    rng.random_range(0..vec_positions.len())
                ];

                let change = Differ::single_diff(
                    &self.generator.current_grid(), 
                    random_position
                ).expect("could not infer change");

                moves.push(change);

                self.generator.apply(change);

            }
            moves

        }
    }



    // TODO: the second set of perf benchmarks we'll need to create
    // are the eval benchmarks. I imagine eval of the board will
    // become the bottleneck after move generation is optimized.

        
}
