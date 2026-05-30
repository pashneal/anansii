use crate::generator::debug::*;
use crate::generator::generator::*;
use crate::hex_grid::{HexGrid, HexLocation, Shiftable, IntoWrappingHexes, Isomorphic};
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

pub const MOSQUITO_MOVES: [&str; 6] = [
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
        "2 - [a B]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . q . . . .\n",
        ". a b 2 2 . .\n",
        " . . . l . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a M]\n",
        "2 - [a B]\n",
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
        " . . . a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a M]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . l M g . . .\n",
        ". a b a p . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
    ),
];

pub const MOSQUITO_SWAPS: [&str; 6] = [
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
        "2 - [a b]\n",
    ),
    concat!(
        ". . . . . . .\n",
        " . . q . . . .\n",
        ". a b 2 2 . .\n",
        " . . . . . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [a M]\n",
        "2 - [a b]\n",
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
    concat!(
        ". . . . . . .\n",
        " . . . a . . .\n",
        ". . a 2 a . .\n",
        " . . l a . . .\n",
        ". . . . . . .\n\n",
        "start - [0 0]\n\n",
        "2 - [m M]\n"
    )
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

    fn populate_hex_grid_with_hex_locations(
        hex_grid: &HexGrid,
        locations: Vec<HexLocation>,
        piece: Piece,
    ) -> Vec<HexGrid> {
        let mut hex_grids = Vec::new();
        for location in locations {
            let mut new_hex_grid = hex_grid.clone();
            new_hex_grid.add(piece, location);
            hex_grids.push(new_hex_grid);
        }
        hex_grids
    }


    fn placement_parity_test<I: IntoPieces, P: PlacementGenerator<I>, Fa, Fb>(
        dsls: &[&'static str],
        funcs: (Fa, Fb),
    ) -> std::result::Result<(), ()>
    where
        Fa: FnMut(&P, PieceColor) -> Vec<I::PieceLocation>,
        Fb: FnMut(&PositionGeneratorDebugger, PieceColor) -> Vec<HexLocation>,
    {

        let colors = [White, Black];
        let hex_grids = dsl_to_hex_grids(dsls);
        let (mut gen_func, mut ref_func) = funcs;

        for hex_grid in hex_grids.iter() {
            let mut reference_generator = PositionGeneratorDebugger::from_default(&hex_grid);
            let mut generator = P::from_default(&hex_grid);

            println!("generating placements from:\n{}\n...", hex_grid.to_dsl());

            for color in colors.iter() {
                let actual_locations = gen_func(&mut generator, *color);
                let expected_locations = ref_func(&mut reference_generator, *color);

                println!("actual locations:\n{:?}\n", actual_locations);
                let actual_grid = generator.current_grid();
                let actual_hex_locations = actual_grid.into_hexes(actual_locations).unwrap();
                println!("actual hex locations:\n{:?}\n", actual_hex_locations);
                let actual_hex_grid = actual_grid.to_hex_grid();
                

                let actuals = populate_hex_grid_with_hex_locations(
                    &actual_hex_grid, 
                    actual_hex_locations, 
                    Piece::new(WildCard, Black)
                );

                let expecteds = populate_hex_grid_with_hex_locations(
                    &hex_grid, 
                    expected_locations.clone(), 
                    Piece::new(WildCard, Black)
                );


                let mut successful = true;
                for expected in expecteds.iter() {
                    let matches_one_reference = actuals.iter().any(|actual| {
                        actual.is_equivalent(expected)
                    });
                    if !matches_one_reference {
                        println!("----------");
                        println!("expected position missing:\n{}\n", expected.to_dsl());
                        println!("----------");
                        successful = false;
                    }
                }

                for actual in actuals.iter() {
                    let matches_no_references = expecteds.iter().all(|expected| {
                        !expected.is_equivalent(actual)
                    });
                    if matches_no_references {
                        println!("----------");
                        println!("unexpected position found:\n{}\n", actual.to_dsl());
                        println!("----------");
                        successful = false;
                    }
                }

                if !successful {
                    return Err(());
                }



            }

            println!("...success\n");
        }

        Ok(())
    }

    // -dsls must contain exactly one piece match the target
    fn swap_parity_test<S, Fa, Fb>(
        target: Piece,
        dsls: &[&'static str],
        funcs: (Fa, Fb),
    ) -> std::result::Result<(), ()>
    where
        S: SwapGenerator,
        Fa: FnMut(&mut S, HexLocation, Option<HexLocation>) -> Vec<S::Position>,
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

            let mut successful = true;

            for position in expected_result.iter() {
                let matches_one_reference = actual_result.iter().any(|actual_position| {
                    actual_position.is_equivalent(position)
                });
                if !matches_one_reference {
                    println!("----------");
                    println!("expected position missing:\n{}\n", position.to_dsl());
                    println!("----------");
                    successful = false;
                }

            }

            for position in actual_result.iter() {
                let matches_no_references = expected_result.iter().all(|expected_position| {
                    !expected_position.is_equivalent(position)
                });
                if matches_no_references {
                    println!("----------");
                    println!("unexpected position found:\n{}\n", position.to_dsl());
                    println!("----------");
                    successful = false;
                }
            }

            if !successful {
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
    fn move_parity_test<M: MoveGenerator, Fa, Fb>(
        target: Piece,
        dsls: &[&'static str],
        funcs: (Fa, Fb),
    ) -> std::result::Result<(), ()>
    where
        Fa: FnMut(&mut M, HexLocation) -> Vec<M::Position>,
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

            let mut successful = true;

            for position in expected_result.iter() {
                let matches_one_reference = actual_result.iter().any(|actual_position| {
                    actual_position.is_equivalent(position)
                });
                if !matches_one_reference {
                    println!("----------");
                    println!("expected position missing:\n{}\n", position.to_dsl());
                    println!("----------");
                    successful = false;
                }

            }

            for position in actual_result.iter() {
                let matches_no_references = expected_result.iter().all(|expected_position| {
                    !expected_position.is_equivalent(position)
                });
                if matches_no_references {
                    println!("----------");
                    println!("unexpected position found:\n{}\n", position.to_dsl());
                    println!("----------");
                    successful = false;
                }
            }

            if !successful {
                return Err(());
            }

            println!("...success\n");
        }
        return Ok(());
    }

    pub fn test_spider_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Spider, White),
            &SPIDER_MOVES[..],
            (M::spider_moves, PositionGeneratorDebugger::spider_moves),
        )
    }

    pub fn test_grasshopper_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Grasshopper, White),
            &GRASSHOPPER_MOVES[..],
            (M::grasshopper_moves, PositionGeneratorDebugger::grasshopper_moves),
        )
    }

    pub fn test_queen_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Queen, White),
            &QUEEN_MOVES[..],
            (M::queen_moves, PositionGeneratorDebugger::queen_moves),
        )
    }

    pub fn test_ant_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Ant, White),
            &ANT_MOVES[..],
            (M::ant_moves, PositionGeneratorDebugger::ant_moves),
        )
    }

    pub fn test_beetle_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Beetle, White),
            &BEETLE_MOVES[..],
            (M::beetle_moves, PositionGeneratorDebugger::beetle_moves),
        )
    }

    pub fn test_ladybug_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Ladybug, White),
            &LADYBUG_MOVES[..],
            (M::ladybug_moves, PositionGeneratorDebugger::ladybug_moves),
        )
    }

    pub fn test_pillbug_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Pillbug, White),
            &PILLBUG_MOVES[..],
            (M::pillbug_moves, PositionGeneratorDebugger::pillbug_moves),
        )
    }

    pub fn test_mosquito_moves<M: MoveGenerator>() -> Result<(), ()> {
        move_parity_test(
            Piece::new(Mosquito, White),
            &MOSQUITO_MOVES[..],
            (M::mosquito_moves, PositionGeneratorDebugger::mosquito_moves),
        )
    }

    pub fn test_pillbug_swaps<S: SwapGenerator>() -> Result<(), ()> {
        swap_parity_test(
            Piece::new(Pillbug, White),
            &PILLBUG_SWAPS[..],
            (S::pillbug_swaps, PositionGeneratorDebugger::pillbug_swaps),
        )
    }

    pub fn test_mosquito_swaps<S: SwapGenerator>() -> Result<(), ()>{
        swap_parity_test(
            Piece::new(Mosquito, White),
            &MOSQUITO_SWAPS[..],
            (S::pillbug_swaps, PositionGeneratorDebugger::pillbug_swaps),
        )
    }

    pub fn test_placements<I : IntoPieces, P: PlacementGenerator<I>>() -> Result<(), ()> {
        placement_parity_test(
            &PLACEMENTS[..],
            (P::placements, PositionGeneratorDebugger::placements),
        )
    }
    

    // TODO: tournament rules integration test suite with generate_positions_for
}
