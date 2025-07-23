use crate::bitgrid::basic::BasicBitGrid;
use crate::game::GameDebugger;
use crate::uhp::UHPInterface;
use regex::Regex;
use std::collections::HashMap;

pub fn uhp_strings() -> Vec<&'static str> {
    include_str!("data.txt").lines().collect()
}

pub struct Stats {
    sizes: HashMap<usize, usize>,
}

impl Stats {
    pub fn new() -> Self {
        Stats {
            sizes: HashMap::new(),
        }
    }

    pub fn add(&mut self, size: usize) {
        let count = self.sizes.entry(size).or_insert(0);
        *count += 1;
    }

    pub fn print(&self) {
        let mut sizes: Vec<_> = self.sizes.iter().collect();
        sizes.sort_by_key(|x| x.0);
        let total: usize = sizes.iter().map(|x| x.1).sum();
        println!("Total positions: {}", total);
        for (size, count) in sizes {
            let percent = (*count as f64 / total as f64) * 100.0;
            let percent = (percent * 10000.0).round() / 10000.0;
            println!("{}: {} ({}%)", size, count, percent);
        }
    }
}

pub fn get_size(game: &GameDebugger) -> usize {
    let position: Result<BasicBitGrid, _> = game.position().try_into();
    let position = position.expect("Failed to convert game position to bitgrid");
    let bounds = position.bounding_box();
    if bounds.is_none() {
        return 0;
    }
    let bounds = bounds.unwrap();
    bounds.width().max(bounds.height())
}

pub fn check_positions() {
    let mut stats = Stats::new();
    let games = uhp_strings();
    let mut failures = 0;

    let mut uhp = UHPInterface::new();
    for (index, game) in games.iter().enumerate() {
        let output = uhp.run_command(&format!("newgame {}", game));

        if output.contains("err") {
            //println!("Failed to load game: {}", game);
            //println!("{}", output);
            failures += 1;
            continue;
        }

        let mut game_debugger = uhp.game_debugger();

        loop {
            let size = get_size(&game_debugger);
            stats.add(size);
            let result = game_debugger.undo_move();
            if result.is_err() {
                break;
            }
        }

        if index % 100 == 0 {
            println!("Processed {} games, with {} failures", index + 1, failures);
            println!("Current stats:");
            stats.print();
        }
    }

    stats.print();
}
