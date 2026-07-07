use super::*;
use crate::location::*;
use std::sync::OnceLock;

use itertools::*;

const SURROUND_MAGIC : u64 = 0x1081080000000000;

const SURROUND : AxialBitboard = AxialBitboard(0x30506);
const TEST_GATE: AxialBitboard = AxialBitboard(0x200);
static GATES : OnceLock<Vec<AxialBitboard>> = OnceLock::new();

fn gated_locations(board: AxialBitboard, center: AxialBitboard) -> AxialBitboard {
    assert!(center.lsb() == center);
    assert!(SURROUND & board == board);
    let mut final_board = AxialBitboard(0);

    let location = MiniBitGridLocation {board_index: 0, mask : center.to_u64() };
    for direction in Direction::all() {
        let candidate = !board & location.apply(direction).mask;

        if candidate.is_empty() {
            continue;
        }

        let (left, right) = direction.adjacent();
        let left_loc = board & location.apply(left).mask;
        let right_loc = board & location.apply(right).mask;

        if left_loc.is_empty() || right_loc.is_empty() {
            final_board |= candidate;
        }
    }

    let mut original_neighbors = AxialBitboard(location.mask).centered_neighbors();
    original_neighbors &= board;

    let mut grid = [AxialBitboard::empty(); 4];
    grid[location.board_index] |= original_neighbors;
    let original_reach = AxialBitboard::fast_neighbors_grid(grid, location.board_index); 
    let original_reach_board = original_reach[location.board_index]; 

    final_board = original_reach_board & final_board;

    final_board
}

pub fn surround_power_set() -> Vec<AxialBitboard> {
    SURROUND.into_iter().powerset().map(
        |v| v.into_iter().fold(AxialBitboard(0), |acc, x| acc | x)
    ).collect()
}

pub fn gated_neighbors(board : AxialBitboard, location : MiniBitGridLocation) -> AxialBitboard {
    assert!(AxialBitboard(location.mask).is_centered());
    let index_func = |x: AxialBitboard| (SURROUND_MAGIC.wrapping_mul(x.to_u64()) >> (64 - 6)) as usize; 
    let boards = GATES.get_or_init(|| {
        let powerset : Vec<(AxialBitboard, AxialBitboard)> = surround_power_set()
            .into_iter()
            .map(|gates| (gates, gated_locations(gates, TEST_GATE)))
            .collect();
        let mut map = vec![AxialBitboard::empty(); powerset.len()];
        for (gates, value) in powerset {
            let index = index_func(gates);
            map[index] = value;
        }
        map
    });
    let mut start = AxialBitboard(location.mask);

    let shift = (TEST_GATE.trailing_zeros() as i32) - (start.trailing_zeros() as i32);
    start = start.centered_neighbors();

    let mut candidate = board & start;
    if shift >= 0 { candidate <<= shift } else { candidate >>= -shift}

    let solution = boards[index_func(candidate)];
    let solution = if shift >= 0 {solution >> shift} else { solution << -shift };

    solution
}
    

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_gated_neighbors() {
        let location_mask = AxialBitboard(0x1000000000);
        let neighbors = AxialBitboard(0x302010000000);
        let location = MiniBitGridLocation{ board_index: 0, mask: location_mask.0 };
        let expected = AxialBitboard(0x80800000000);
        let response = gated_neighbors(neighbors, location);

        println!("expected:\n{}", expected);
        println!("response:\n{}", response); 
        assert_eq!(expected, response);
    }
}
