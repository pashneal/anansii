mod bitgrid;
mod testing_utils;
mod constants;
mod game;
mod hex_grid;
mod hex_grid_dsl;
mod location;
mod move_generator;
mod piece;
mod uhp;

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
        Some(MainCommands::Uhp) => {
            run_uhp();
        }
        None => run_uhp(),
    }
}
