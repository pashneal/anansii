mod bitgrid;
mod constants;
mod game;
mod hex_grid;
mod hex_grid_dsl;
mod location;
mod move_generator;
mod piece;
mod testing_utils;
mod uhp;
mod data_analysis;

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

    /// Interprets a number as an Axial and prints the bitboard
    Bitboard{
        number: u64,
    }
}

pub fn run_uhp() {
    let mut uhp = UHPInterface::new();
    let mut input = String::new();
    let output = uhp.command("info");
    print!("{}", output);

    loop {
        input.clear();
        std::io::stdin().read_line(&mut input).unwrap();
        let output = uhp.command(&input);
        print!("{}", output);
    }
}

pub fn main() {
    let args = Cli::parse();
    match args.command {
        Some(MainCommands::Uhp) =>  run_uhp(),
        Some(MainCommands::Analyze) =>  data_analysis::check_positions(),
        Some(MainCommands::Bitboard{number}) => {
            let bitboard = bitgrid::board::AxialBitboard::from_u64(number);
            println!("{}", bitboard);
        }

        None => run_uhp(),
    }
}
