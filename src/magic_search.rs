use crate::bitgrid::gates::surround_power_set;
use rand::{Rng, RngExt};
use rand::{rngs::ChaCha8Rng, SeedableRng};

// 1189240572695543808 - magic for converting powerset of surround to index into 64 bit array

fn generate_magic_candidate(rng: &mut impl rand::Rng) -> u64 {
    let mut candidate = rng.random::<u64>();
    for i in 0..4 {
        let and_mask = rng.random::<u64>();
        candidate &= and_mask;
    }

    candidate
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
