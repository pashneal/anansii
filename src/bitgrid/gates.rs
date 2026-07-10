use super::*;
use crate::location::*;
use crate::bitgrid::mini::*;
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

pub fn bottom_gated_neighbors(
    board_bottom : AxialBitboard,
    board_center : AxialBitboard,
    location : MiniBitGridLocation
) -> (AxialBitboard, AxialBitboard) {

    let neighbors = gated_neighbors(
        (board_bottom & TOP_OVERFLOW_MASK).flip_northwest() | (board_center << 8),
        MiniBitGridLocation {board_index: location.board_index, mask: location.mask << 8},
    );

    let bottom_neighbors = (neighbors & BOTTOM_OVERFLOW_MASK).flip_southeast();
    let center_neighbors = (neighbors & !BOTTOM_OVERFLOW_MASK) >> 8;

    (center_neighbors, bottom_neighbors)
}

pub fn top_gated_neighbors(
    board_top : AxialBitboard,
    board_center : AxialBitboard,
    location : MiniBitGridLocation
) -> (AxialBitboard, AxialBitboard) {

    let neighbors = gated_neighbors(
        (board_top & BOTTOM_OVERFLOW_MASK).flip_southeast() | (board_center >> 8),
        MiniBitGridLocation {board_index: location.board_index, mask: location.mask >> 8}
    );

    let top_neighbors = (neighbors & TOP_OVERFLOW_MASK).flip_northwest();
    let center_neighbors = (neighbors & !TOP_OVERFLOW_MASK) << 8;

    (top_neighbors, center_neighbors)
}

pub fn right_gated_neighbors(
    board_right: AxialBitboard,
    board_center: AxialBitboard,
    location: MiniBitGridLocation
) -> (AxialBitboard, AxialBitboard) {

    let neighbors = gated_neighbors(
        (board_right & LEFT_OVERFLOW_MASK).flip_west() | (board_center & !LEFT_OVERFLOW_MASK) << 1,
        MiniBitGridLocation {board_index: location.board_index, mask: location.mask << 1}
    );

    let right_neighbors = (neighbors & RIGHT_OVERFLOW_MASK).flip_east();
    let center_neighbors = (neighbors & !RIGHT_OVERFLOW_MASK) >> 1;

    (center_neighbors, right_neighbors)
}

pub fn left_gated_neighbors(
    board_left: AxialBitboard, 
    board_center: AxialBitboard, 
    location: MiniBitGridLocation
) -> (AxialBitboard, AxialBitboard) {

    let neighbors = gated_neighbors(
        (board_left & RIGHT_OVERFLOW_MASK).flip_east() | (board_center & !RIGHT_OVERFLOW_MASK) >> 1,
        MiniBitGridLocation {board_index: location.board_index, mask: location.mask >> 1}
    );

    let left_neighbors = (neighbors & LEFT_OVERFLOW_MASK).flip_west();
    let center_neighbors = (neighbors & !LEFT_OVERFLOW_MASK) << 1;

    (left_neighbors, center_neighbors)
}

pub fn gated_neighbors(board : AxialBitboard, location : MiniBitGridLocation) -> AxialBitboard {
    //assert!(AxialBitboard(location.mask).is_centered());
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

    #[test]
    pub fn test_left_gated_neighbors() {
        let location_mask  = AxialBitboard(0x8000000000);
        let center_neighbors = AxialBitboard(0x804080000000);
        let left_neighbors = AxialBitboard(0x10000000000);

        let location = MiniBitGridLocation{ board_index: 0, mask: location_mask.0 };
        let (left_neighbors, center_neighbors) = left_gated_neighbors(left_neighbors, center_neighbors, location);
        let expected_left_neighbor = AxialBitboard(0x101000000);
        let expected_right_neighbor = AxialBitboard(0x0);

        println!("expected left:\n{}", expected_left_neighbor);
        println!("expected right:\n{}", expected_right_neighbor);
        println!("response left:\n{}", left_neighbors);
        println!("response right:\n{}", center_neighbors);
        assert_eq!(expected_right_neighbor, center_neighbors);
        assert_eq!(expected_left_neighbor, left_neighbors);
    }

    #[test]
    pub fn test_right_gated_neighbors() {
        let location_mask = AxialBitboard(0x10000);
        let center = AxialBitboard(0x1020200);
        let right = AxialBitboard(0x80000000);

        let location = MiniBitGridLocation{ board_index: 0, mask: location_mask.0 };
        let expected_right = AxialBitboard(0x800000);
        let expected_center = AxialBitboard(0x100);
        let (center_neighbors, right_neighbors) = right_gated_neighbors(right, center, location);

        println!("expected right:\n{}", expected_right);
        println!("expected center:\n{}", expected_center);
        println!("response right:\n{}", right_neighbors);
        println!("response center:\n{}", center_neighbors);
        assert_eq!(expected_right, right_neighbors);
        assert_eq!(expected_center, center_neighbors);

    }

    #[test]
    pub fn test_top_gated_neighbors() {
        let location_mask = AxialBitboard(0x200000000000000);
        let center = AxialBitboard(0x403000000000000);
        let top = AxialBitboard(0);

        let location = MiniBitGridLocation{ board_index: 0, mask: location_mask.0 };
        let expected_top = AxialBitboard(0x2);
        let expected_center  = AxialBitboard(0x100000000000000);

        let (top_neighbors, center_neighbors) = top_gated_neighbors(top, center, location);

        println!("expected top:\n{}", expected_top);
        println!("expected center:\n{}", expected_center);
        println!("response top:\n{}", top_neighbors);
        println!("response center:\n{}", center_neighbors);

        assert_eq!(expected_top, top_neighbors);
        assert_eq!(expected_center, center_neighbors);
    }

    #[test]
    pub fn test_bottom_gated_neighbors() {
        let location_mask = AxialBitboard(0x10);
        let center = AxialBitboard(0x1438);
        let bottom = AxialBitboard(0x20000000000000);

        let location = MiniBitGridLocation{ board_index: 0, mask: location_mask.0 };

        let expected_bottom = AxialBitboard(0x3000000000000000);
        let expected_center = AxialBitboard(0);

        let (center_neighbors, bottom_neighbors) = bottom_gated_neighbors(bottom, center, location);

        println!("expected bottom:\n{}", expected_bottom);
        println!("expected center:\n{}", expected_center);
        println!("response bottom:\n{}", bottom_neighbors);
        println!("response center:\n{}", center_neighbors);

        assert_eq!(expected_bottom, bottom_neighbors);
        assert_eq!(expected_center, center_neighbors);



    }
}
