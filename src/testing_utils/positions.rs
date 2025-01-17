use crate::generator::debug::*;
use crate::hex_grid::{HexGrid, HexLocation};
use crate::piece::*;
use crate::uhp::GameType;

const SPIDER_MOVES: [&'static str; 8] = [
    concat!(
        " . . . . . . .\n",
        ". . . a . . .\n",
        " . a S a . . .\n",
        ". . a a . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . . a a . .\n",
        " . a . . a . .\n",
        ". . a S a . .\n",
        " . . a a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . . a . . .\n",
        " . a S a . . .\n",
        ". . . . . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a . . . .\n",
        " . a S . . . .\n",
        ". . . a a . .\n",
        " . . a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a S . .\n",
        ". a . . . . .\n",
        " . a a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a . . S . .\n",
        " . a a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a a a . .\n",
        " . a S . a . .\n",
        ". . a . a . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a . . S . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
];

pub const GRASSHOPPER_MOVES: [&str; 2] = [
    concat!(
        ". a a a . . .\n",
        " . . . a . . .\n",
        ". . a a . . .\n",
        " . a G a a a .\n",
        ". . . . . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        ". . . a . . .\n",
        " . . a a . . .\n",
        ". . a . . . .\n",
        " . a G a a a .\n",
        ". . . a . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
];

pub const QUEEN_MOVES: [&str; 5] = [
    concat!(
        " . . . . . . .\n",
        ". . a . . . .\n",
        " . a . a . . .\n",
        ". a . . Q . .\n",
        " . a a a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a . . Q . .\n",
        " . a a a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a . . Q . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . .\n",
        ". . a a . . .\n",
        " . a . a . . .\n",
        ". a . Q . . .\n",
        " . a a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        ". . . . . . .\n",
        " a . . a a . .\n",
        ". a . Q . a .\n",
        " a . . a a . .\n",
        ". a a a . . .\n\n",
        "start - [0 0]\n\n"
    ),
];

pub const ANT_MOVES: [&str; 2] = [
    concat!(
        " . . . . . . . . .\n",
        ". . . g g g . . .\n",
        " . . g . . g g . .\n",
        ". . . . . g . g .\n",
        " . . g g g . g . .\n",
        ". . . . A . . . .\n",
        " . . . . . . . . .\n",
        ". . . . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        " . . . . . . . . .\n",
        ". . . g g g . . .\n",
        " . . g . . g g . .\n",
        ". . . . . g . g .\n",
        " . . g A g . g . .\n",
        ". . . . . . . . .\n",
        " . . . . . . . . .\n",
        ". . . . . . . . .\n\n",
        "start - [0 0]\n\n"
    ),
];

pub const BEETLE_MOVES: [&str; 8] = [
    concat!(
        ". . . . . . .\n",
        " a . . a a . .\n",
        ". a . B . a .\n",
        " a . . a a . .\n",
        ". a a a . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        ". . . . . . .\n",
        " a . . a a . .\n",
        ". a . B a a .\n",
        " a . . a a . .\n",
        ". a a a . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        ". . . . . . .\n",
        " a . . a . . .\n",
        ". a . . 2 . .\n",
        " a . . a a . .\n",
        ". a a a . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a B]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . 3 a . .\n",
        ". . . a 2 a .\n",
        " . . . 3 . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "3 - [a b b]\n",
        "2 - [a B]\n",
        "3 - [a b b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . 3 a . .\n",
        ". . . 2 B a .\n",
        " . . . 4 . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "3 - [a b b]\n",
        "2 - [a b]\n",
        "4 - [a b b b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . 2 . . .\n",
        ". . . a 2 2 .\n",
        " . . . 4 a . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "2 - [a B]\n",
        "2 - [a b]\n",
        "4 - [a b b b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . a . . .\n",
        ". . . . B . .\n",
        " . . . a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . . a . . .\n",
        ". . . 2 . . .\n",
        " . . . a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a B]\n"
    ),
];

pub const LADYBUG_MOVES: [&str; 3] = [
    concat!(
        ". . . . . . .\n",
        " . . 2 2 . . .\n",
        ". . a a L . .\n",
        " . . 2 a . . .\n",
        ". . . . . . .\n\n",
        " . . . . . a .\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
    ),
    concat!(
        ". . . . . a .\n",
        " . . . 2 a . .\n",
        ". . . . L 2 .\n",
        " . . . 2 a . .\n",
        ". . . . a 2 .\n",
        " . . . . . a .\n\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
        "2 - [a b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . 2 . . .\n",
        ". . a . L . .\n",
        " . . 2 a . . .\n",
        ". . . . . . .\n\n",
        " . . . . . a .\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
    ),
];

pub const PILLBUG_MOVES: [&str; 2] = [
    concat!(
        ". . . . . . .\n",
        " a . . a a . .\n",
        ". a . P . a .\n",
        " a . . a a . .\n",
        ". a a a . . .\n\n",
        "start - [0 0]\n\n"
    ),
    concat!(
        ". . . . . . .\n",
        " a . . a a . .\n",
        ". a . P . a .\n",
        " a . . . a . .\n",
        ". a a a a . .\n\n",
        "start - [0 0]\n\n"
    ),
];

pub const PILLBUG_SWAPS: [&str; 11] = [
    concat!(
        ". . . . . . .\n",
        " . . . 2 . . .\n",
        ". . 2 P . . .\n",
        " . . l . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [q b]\n",
        "2 - [m b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . 2 . . .\n",
        ". . 2 P l . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [q b]\n",
        "2 - [m b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . 2 . . .\n",
        ". . 2 P . . .\n",
        " . . . l . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [q b]\n",
        "2 - [m b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . b q . . .\n",
        ". . 2 P a a .\n",
        " . . l . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [m b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . . q . . .\n",
        ". . 2 P a a .\n",
        " . . l b . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [m b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . b q . . .\n",
        ". . 2 P a a .\n",
        " . . . l . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [m b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . b q . . .\n",
        ". . 2 P a a .\n",
        " . . l a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [m b]\n"
    ),
    concat!(
        ". . . . . . .\n",
        " . . b 3 . . .\n",
        ". . 2 P a . .\n",
        " . . . a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "3 - [m b b]\n",
        "2 - [m b]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . b 3 . . .\n",
        ". . 2 P . . .\n",
        " . . a a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "3 - [m b b]\n",
        "2 - [m b]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . b 3 . . .\n",
        ". . 2 P a . .\n",
        " . . a . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "3 - [m b b]\n",
        "2 - [m b]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . b . . . .\n",
        ". . 2 P a . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [m b]\n",
    ),
];

pub const PLACEMENTS: [&str; 3] = [
    concat!(
        ". . . . . . .\n",
        " . A . A . . .\n",
        ". 2 b a A . .\n",
        " . b . 2 . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [A b]\n",
        "2 - [m B]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . . . . . .\n",
        ". . . A . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
    ),
];

pub const MOSQUITO_MOVES: [&str; 4] = [
    concat!(
        ". . . . . . .\n",
        " . . . . . . .\n",
        ". a m M . . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . q g . . .\n",
        ". a b M 2 . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a S]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . q . . . .\n",
        ". a b 2 2 . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a M]\n",
        "2 - [a S]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . . g . . .\n",
        ". a b M 2 . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a B]\n",
    ),
];

pub mod test_suite {
    use super::*;
    use PieceColor::*;
    use PieceType::*;

    fn dsl_to_locations(dsls: &[&'static str], target: Piece) -> Vec<HexLocation> {
        dsl_to_hex_grids(dsls)
            .iter()
            .map(|x| x.find(target))
            .map(|x| x.expect(&format!("Could not find target piece {:?}", target)))
            .map(|x| x.0)
            .collect::<Vec<_>>()
    }

    fn dsl_to_hex_grids(dsls: &[&'static str]) -> Vec<HexGrid> {
        dsls.iter()
            .map(|x| HexGrid::from_dsl(x))
            .collect::<Vec<_>>()
    }

    // this is the most garbage function definition I've ever written :D,
    // just wanted to play around with generics and closures
    // ... well maybe it's not all that bad, look how clean the final interface ends up being!
    fn move_test<I: IntoPieces, M: MoveGenerator<I>, Fa, Fb>(
        target: Piece,
        dsls: &[&'static str],
        funcs: (Fa, Fb),
    ) -> std::result::Result<(), ()>
    where
        Fa: FnMut(&mut M, HexLocation) -> Vec<I>,
        Fb: FnMut(&mut ReferenceGenerator, HexLocation) -> Vec<HexGrid>,
    {
        let locations = dsl_to_locations(dsls, target);
        let hex_grids = dsl_to_hex_grids(dsls);

        let (mut gen_func, mut ref_func) = funcs;

        for (location, hex_grid) in locations.iter().zip(hex_grids.iter()) {
            let mut reference_generator = ReferenceGenerator::from_default(&hex_grid);
            let mut generator = M::from_default(&hex_grid);

            println!("generating positions from:\n{}\n...", hex_grid.to_dsl());

            let actual_result = gen_func(&mut generator, *location);
            let expected_result = ref_func(&mut reference_generator, *location);

            let actual_result = actual_result
                .into_iter()
                .map(|x| x.to_hex_grid())
                .collect::<Vec<_>>();


            for position in expected_result.iter() {
                if !actual_result.contains(&position) {
                    println!("----------");
                    println!("expected position missing:\n{}\n", position.to_dsl());
                    println!("----------");
                }
            }

            for position in actual_result.iter() {
                if !expected_result.contains(&position) {
                    println!("----------");
                    println!("unexpected position:\n{}\n", position.to_dsl());
                    println!("----------");
                }
            }

            if !expected_result.iter().all(|x| actual_result.contains(x)) {
                return Err(());
            }
            if !actual_result.iter().all(|x| expected_result.contains(x)) {
                return Err(());
            }

            println!("...success\n");
        }
        return Ok(());
    }

    pub fn test_spider_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Spider, White),
            &SPIDER_MOVES[..],
            (M::spider_moves, ReferenceGenerator::spider_moves),
        )
    }

    pub fn test_grasshopper_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Grasshopper, White),
            &GRASSHOPPER_MOVES[..],
            (M::grasshopper_moves, ReferenceGenerator::grasshopper_moves),
        )
    }

    pub fn test_queen_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Queen, White),
            &QUEEN_MOVES[..],
            (M::queen_moves, ReferenceGenerator::queen_moves),
        )
    }

    pub fn test_ant_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Ant, White),
            &ANT_MOVES[..],
            (M::ant_moves, ReferenceGenerator::ant_moves),
        )
    }

    pub fn test_beetle_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Beetle, White),
            &BEETLE_MOVES[..],
            (M::beetle_moves, ReferenceGenerator::beetle_moves),
        )
    }

    pub fn test_ladybug_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Ladybug, White),
            &LADYBUG_MOVES[..],
            (M::ladybug_moves, ReferenceGenerator::ladybug_moves),
        )
    }

    pub fn test_pillbug_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Pillbug, White),
            &PILLBUG_MOVES[..],
            (M::pillbug_moves, ReferenceGenerator::pillbug_moves),
        )
    }

    pub fn test_mosquito_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_test(
            Piece::new(Mosquito, White),
            &MOSQUITO_MOVES[..],
            (M::mosquito_moves, ReferenceGenerator::mosquito_moves),
        )
    }
}
