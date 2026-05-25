use crate::generator::debug::*;
use crate::generator::generator::*;
use crate::hex_grid::{HexGrid, HexLocation, Shiftable};
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

pub const BEETLE_MOVES: [&str; 9] = [
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
        " . . . 2 a . .\n",
        ". . . a B a .\n",
        " . . . 3 . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "3 - [a b b]\n"
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
    concat!(
        ". . . . . . .\n",
        " . . 2 2 . . .\n",
        ". . a a . . .\n",
        " . . 2 2 . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
        "2 - [a B]\n",
    ),
];

pub const LADYBUG_MOVES: [&str; 4] = [
    concat!(
        ". . . . . . .\n",
        " . . 2 2 . . .\n",
        ". . a a L . .\n",
        " . . 2 2 . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
        "2 - [a b]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . 2 2 . . .\n",
        ". . a a L . .\n",
        " . . 2 a . . .\n",
        ". . . . . . .\n\n",
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

pub const PILLBUG_SWAPS: [&str; 12] = [
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
    // Technically, this case cannot appear in a legal game,
    // but we want to be sure that movement for a pillbug on top of the 
    // hive is well defined + tested - for ease of implementation of the
    // mosquito, which can copy the pillbug's swapping movement
    concat!(
        ". . . . . . .\n",
        " . . . a . . .\n",
        ". . a 2 a . .\n",
        " . . l a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [m P]\n"
    )
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

pub const MOSQUITO_MOVES: [&str; 5] = [
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
    concat!(
        ". . . . . . .\n",
        " . . . g . . .\n",
        ". a b 2 p . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a M]\n",
    ),
];

pub const MOSQUITO_SWAPS: [&str; 5] = [
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
    concat!(
        ". . . . . . .\n",
        " . . . g . . .\n",
        ". a b 2 p . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a M]\n",
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

    // -dsls must contain exactly one piece match the target
    fn swap_parity_test<I: IntoPieces, S: SwapGenerator<I>, Fa, Fb>(
        target: Piece,
        dsls: &[&'static str],
        funcs: (Fa, Fb),
    ) -> std::result::Result<(), ()>
    where
        Fa: FnMut(&mut S, HexLocation, Option<HexLocation>) -> Vec<I>,
        Fb: FnMut(&mut PositionGeneratorDebugger, HexLocation, Option<HexLocation>) -> Vec<HexGrid>,
    {
        // the methodology for this is just that we need to generate
        // good "immobilized" positions. For now, since it's underspecified 
        // whether an immobilized location *must* contain a piece
        // we'll stick to the strict spec of SwapGenerator and generate
        // the cartesian product of:
        // - dsls
        // - all 6 locations adjacent to the target piece + 
        //      the location of the target piece + 
        //      no location

        let locations = dsl_to_locations(dsls, target);
        let hex_grids = dsl_to_hex_grids(dsls);

        let (mut gen_func, mut ref_func) = funcs;

        let translation_funcs = [
            HexLocation::shift_east,
            HexLocation::shift_northeast,
            HexLocation::shift_northwest,
            HexLocation::shift_west,
            HexLocation::shift_southwest,
            HexLocation::shift_southeast,
            |x: &HexLocation| *x, 

        ];


        let immobilized_locations = locations
            .iter()
            .flat_map(|location| {
                translation_funcs
                    .iter()
                    .map(|func| func(location))
                    .map(|loc| Some(loc))
            })
            .collect::<Vec<_>>();

        let immobilized_locations = immobilized_locations
            .into_iter()
            .chain(std::iter::once(None))
            .collect::<Vec<_>>();


        let triplets = locations
            .iter()
            .zip(hex_grids.iter())
            .flat_map(|(location, hex_grid)| {
                immobilized_locations
                    .iter()
                    .map(move |immobilized_location| (location, hex_grid, immobilized_location))
            })
            .collect::<Vec<_>>();
        for (location, hex_grid, immobilized_location) in triplets {
            let mut reference_generator = PositionGeneratorDebugger::from_default(&hex_grid);
            let mut generator = S::from_default(&hex_grid);

            println!("generating swaps from:\n{}\n...", hex_grid.to_dsl());
            println!(
                "immobilized location: {:?}\n", immobilized_location
            );

            let actual_result = gen_func(&mut generator, *location, *immobilized_location);
            let expected_result = ref_func(&mut reference_generator, *location, *immobilized_location);

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

        Ok(())
    }

    // this is the most garbage function definition I've ever written :D,
    // just wanted to play around with generics and closures
    // ... well maybe it's not all that bad, look how clean the final interface ends up being!
    // -dsls must contain exactly one piece matching the target
    fn move_parity_test<I: IntoPieces, M: MoveGenerator<I>, Fa, Fb>(
        target: Piece,
        dsls: &[&'static str],
        funcs: (Fa, Fb),
    ) -> std::result::Result<(), ()>
    where
        Fa: FnMut(&mut M, HexLocation) -> Vec<I>,
        Fb: FnMut(&mut PositionGeneratorDebugger, HexLocation) -> Vec<HexGrid>,
    {
        let locations = dsl_to_locations(dsls, target);
        let hex_grids = dsl_to_hex_grids(dsls);

        let (mut gen_func, mut ref_func) = funcs;

        for (location, hex_grid) in locations.iter().zip(hex_grids.iter()) {
            let mut reference_generator = PositionGeneratorDebugger::from_default(&hex_grid);
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
                    println!("unexpected position found:\n{}\n", position.to_dsl());
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
        move_parity_test(
            Piece::new(Spider, White),
            &SPIDER_MOVES[..],
            (M::spider_moves, PositionGeneratorDebugger::spider_moves),
        )
    }

    pub fn test_grasshopper_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Grasshopper, White),
            &GRASSHOPPER_MOVES[..],
            (M::grasshopper_moves, PositionGeneratorDebugger::grasshopper_moves),
        )
    }

    pub fn test_queen_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Queen, White),
            &QUEEN_MOVES[..],
            (M::queen_moves, PositionGeneratorDebugger::queen_moves),
        )
    }

    pub fn test_ant_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Ant, White),
            &ANT_MOVES[..],
            (M::ant_moves, PositionGeneratorDebugger::ant_moves),
        )
    }

    pub fn test_beetle_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Beetle, White),
            &BEETLE_MOVES[..],
            (M::beetle_moves, PositionGeneratorDebugger::beetle_moves),
        )
    }

    pub fn test_ladybug_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Ladybug, White),
            &LADYBUG_MOVES[..],
            (M::ladybug_moves, PositionGeneratorDebugger::ladybug_moves),
        )
    }

    pub fn test_pillbug_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Pillbug, White),
            &PILLBUG_MOVES[..],
            (M::pillbug_moves, PositionGeneratorDebugger::pillbug_moves),
        )
    }

    pub fn test_mosquito_moves<I: IntoPieces, M: MoveGenerator<I>>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Mosquito, White),
            &MOSQUITO_MOVES[..],
            (M::mosquito_moves, PositionGeneratorDebugger::mosquito_moves),
        )
    }

    pub fn test_pillbug_swaps<I: IntoPieces, S: SwapGenerator<I>>() -> Result<(), ()> {
        swap_parity_test(
            Piece::new(Pillbug, White),
            &PILLBUG_SWAPS[..],
            (S::pillbug_swaps, PositionGeneratorDebugger::pillbug_swaps),
        )
    }

    pub fn test_mosquito_swaps<I: IntoPieces, S: SwapGenerator<I>>() -> Result<(), ()>{
        swap_parity_test(
            Piece::new(Mosquito, White),
            &MOSQUITO_SWAPS[..],
            (S::pillbug_swaps, PositionGeneratorDebugger::pillbug_swaps),
        )
    }

    // TODO: placement generator tests
    

    // TODO: tournament rules integration test suite with generate_positions_for
}
