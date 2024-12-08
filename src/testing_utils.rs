use crate::hex_grid::*;
use crate::location::*;
use std::collections::HashSet;

pub fn compare_moves(
    start_location: HexLocation,
    expected: &str,
    original_position: &HexGrid,
    test_positions: &Vec<HexGrid>,
) {
    let expected_locations = HexGrid::selector(expected);
    let mut original_position = original_position.clone();
    let piece = original_position
        .remove(start_location)
        .expect("Expected piece at start location");
    let mut expected_positions = Vec::new();

    for location in expected_locations {
        let mut new_position = original_position.clone();
        new_position.add(piece, location);
        expected_positions.push(new_position);
    }

    for position in test_positions {
        println!("test_position:\n{}\n", position.to_dsl());
    }
    for position in expected_positions.iter() {
        println!("expected_position:\n{}\n", position.to_dsl());
    }

    assert_eq!(expected_positions.len(), test_positions.len());
    for position in expected_positions {
        assert!(test_positions.contains(&position));
    }
}

/// Checks to see if the representation of the board is "localized"
/// that is, for all locations within a certain given *distance* moves
/// away from given *start* location without backtracking , there are no duplicate locations. 
///
/// Additionally, checks that the HexLocation -> H mapping is preserved under shifting.
///
/// This is useful for checking the minimum safe size of a wrapping board,
/// as wrapping boards eventually must collide with itself.
pub fn is_localized<H : Shiftable + FromHex>(start : H, reference : HexLocation, distance : usize) -> bool {
    if H::from_hex(reference) != start {
        println!("error -> start: {:?}, converted reference: {:?}", start, H::from_hex(reference));
        return false;
    }


    pub fn dfs<H : Shiftable + FromHex>(
        visited_references : &mut HashSet<HexLocation>,
        dfs_stack : &mut Vec<H>,
        reference : HexLocation,
        current : H,
        distance : usize,
    ) -> bool {
        if visited_references.contains(&reference) {
            return true;
        }
        visited_references.insert(reference);

        if dfs_stack.contains(&current) {
            println!("the board wraps and collides with itself, stack is : {:?}", dfs_stack);
            println!("current: {:?}, reference: {:?}", current, reference);
            return false;
        }

        if H::from_hex(reference) != current {
            println!("The mapping Hexlocation -> H is not preserved under shifting, current: {:?}, reference: {:?}", current, reference);
            return false;
        }

        if distance == 0 {
            return true;
        }

        let e = (current.shift_east(), reference.shift_east());
        let w = (current.shift_west(), reference.shift_west());
        let ne = (current.shift_northeast(), reference.shift_northeast());
        let nw = (current.shift_northwest(), reference.shift_northwest());
        let se = (current.shift_southeast(), reference.shift_southeast());
        let sw = (current.shift_southwest(), reference.shift_southwest());
        let neighbors = vec![e, w, ne, nw, se, sw];

        dfs_stack.push(current);
        for (neighbor, new_reference) in neighbors {
            if !dfs(visited_references, dfs_stack, new_reference, neighbor, distance - 1) {
                return false;
            }
        }
        dfs_stack.pop();
        true
    }

    dfs(&mut HashSet::new(), &mut Vec::new(), reference, start, distance)
}
