use crate::bitgrid::gates::surround_power_set;
use crate::bitgrid::board::AxialBitboard;
use itertools::Itertools;
use rand::{Rng, RngExt};
use rand::{rngs::ChaCha8Rng, SeedableRng};

// 1189240572695543808 - magic for converting powerset of surround to index into 64 bit array

const ALL_CENTERED_LOCATIONS : AxialBitboard = AxialBitboard(0x7e7e7e7e7e7e00);

fn generate_magic_candidate(rng: &mut impl rand::Rng) -> u64 {
    let mut candidate = rng.random::<u64>();
    for i in 0..4 {
        let and_mask = rng.random::<u64>();
        candidate &= and_mask;
    }

    candidate
}

pub fn power_set(board: AxialBitboard) -> Vec<AxialBitboard> {
    assert!(board.lsb() == board);
    let power_sets : Vec<AxialBitboard> = board.centered_neighbors().into_iter().powerset().map(
        |v| v.into_iter().fold(AxialBitboard(0), |acc, x| acc | x)
    ).collect();
    assert!(power_sets.len() == (1 << 6));
    power_sets
}

pub fn find_centered_magics() -> Vec<(u32, u64)> {
    let boards : Vec<AxialBitboard> = ALL_CENTERED_LOCATIONS.into_iter().collect();
    let mut magics = Vec::new();
    let power_sets : Vec<Vec<AxialBitboard>> = boards.iter().map(|&b| power_set(b)).collect();

    let mut rng = ChaCha8Rng::seed_from_u64(rand::random::<u64>());

    for (i, board) in boards.iter().enumerate() {
        let powerset = &power_sets[i];
        let vec = vec![0u64; powerset.len()];
        let upper_bits_func = |x: u64| x >> (64 - 6);

        for _ in 0..10000000 {
            let candidate = generate_magic_candidate(&mut rng);
            let mut seen = vec.clone();
            let mut collision = false;
            for (_, &test) in powerset.iter().enumerate() {
                let index = upper_bits_func(candidate.wrapping_mul(test.to_u64()));
                if seen[index as usize] != 0 {
                    collision = true;
                    break;
                }
                seen[index as usize] = test.to_u64();
            }
            if !collision {
                magics.push((board.trailing_zeros(), candidate));
                break;
            }
        }
    }
    if magics.len() != boards.len() {
        panic!("Could not find magic numbers for all boards");
    }
    magics


}

pub fn find_magic() -> u64 {
    let powerset = surround_power_set()
                        .into_iter()
                        .map(|set| set.0)
                        .collect::<Vec<u64>>();
    let vec = vec![0u64; powerset.len()];
    let upper_bits_func = |x: u64| x >> (64 - 6);
    let mut rng = ChaCha8Rng::seed_from_u64(rand::random::<u64>());

    for _ in 0..10000000 {
        let candidate = generate_magic_candidate(&mut rng);
        let mut seen = vec.clone();
        let mut collision = false;
        for (_, &test) in powerset.iter().enumerate() {
            let index = upper_bits_func(candidate.wrapping_mul(test));
            if seen[index as usize] != 0 {
                collision = true;
                break;
            }
            seen[index as usize] = test;
        }
        if !collision {
            return candidate;
        }
    }
    panic!("Could not find magic number");
}
