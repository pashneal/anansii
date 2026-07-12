mod bitgrid;
mod constants;
mod data_analysis;
mod game;
mod generator;
mod hex_grid;
mod hex_grid_dsl;
mod location;
mod piece;
mod testing_utils;
mod uhp;
mod magic_search;

use clap::{Parser, Subcommand};
use uhp::UHPInterface;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<MainCommands>,
}

#[derive(Subcommand)]
enum MainCommands {
    /// Runs an interactive interface for a hive game engine that supports uhp
    Uhp,

    /// Runs a script to analyze the certain statistics for hive positions
    Analyze,

    /// Runs a script to try and determine a magic number set up by 
    /// the magic_search module
    MagicSearch,

    /// Runs a script to try and determine a magic number set up by 
    /// the magic_search module
    MagicSearches,

    /// Interprets a number as an Axial and prints the bitboard
    Bitboard { number: u64 },

    /// Tests a vectorized single step instruction on a bitboard
    SingleStep,
}

pub fn run_universal_hive_protocol() {
    let mut uhp = UHPInterface::new();
    let mut input = String::new();
    let output = uhp.run_command("info");
    print!("{}", output);

    loop {
        input.clear();
        std::io::stdin().read_line(&mut input).unwrap();
        let output = uhp.run_command(&input);
        print!("{}", output);
    }
}

pub fn main() {
    let args = Cli::parse();
    match args.command {
        Some(MainCommands::Uhp) => run_universal_hive_protocol(),
        Some(MainCommands::Analyze) => data_analysis::check_positions(),
        Some(MainCommands::Bitboard { number }) => {
            let bitboard = bitgrid::board::AxialBitboard::from_u64(number);
            println!("{}", bitboard);
        }
        Some(MainCommands::MagicSearch) => {
            let magic_number = magic_search::find_magic();
            println!("Found magic number: {}", magic_number);
        }

        Some(MainCommands::MagicSearches) => {
            let magic_numbers = magic_search::find_centered_magics();
            println!("Found magic numbers: {:#?}", magic_numbers);
        }

        Some(MainCommands::SingleStep) => {
            let mut bitboards = [
                bitgrid::board::AxialBitboard::from_u64(0x0000000000000001),
                bitgrid::board::AxialBitboard::from_u64(0x0000000000000002),
                bitgrid::board::AxialBitboard::from_u64(0x0000000000000004),
                bitgrid::board::AxialBitboard::from_u64(0x0000000000000008),
            ];

            bitgrid::vectorized::single_step::single_step(&mut bitboards);

            for (i, bb) in bitboards.iter().enumerate() {
                println!("Bitboard {}: {:#018x}", i, bb.to_u64());
            }
        }

        None => run_universal_hive_protocol(),
    }
}
