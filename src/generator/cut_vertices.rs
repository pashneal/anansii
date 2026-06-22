use crate::hex_grid::Shiftable;
use crate::piece::Peekable;
use crate::location::Direction;
use std::collections::{HashSet, HashMap};

fn neighbors<T: Shiftable>(graph: &impl Peekable<Location = T>, node: &T) -> Vec<T> {
    let mut neighbors = Vec::new();

    for direction in Direction::all() {
        let neighbor = node.apply(direction);
        if !graph.peek(neighbor.clone()).is_empty() {
            neighbors.push(neighbor);
        }
    }

    neighbors
}

pub fn cut_vertices<T: Shiftable>(
    graph: &impl Peekable<Location = T>, 
    visited: &mut HashSet<T>, 
    discovery_time: &mut HashMap<T, u32>,
    low_time: &mut HashMap<T, u32>,
    node: T,
    time: u32,
    parent: Option<&T>,
) -> HashSet<T> {
    let mut children = 0;
    discovery_time.insert(node.clone(), time);
    low_time.insert(node.clone(), time);
    visited.insert(node.clone());


    let mut results = HashSet::new();

    for neighbor in neighbors(graph, &node) {
        if visited.contains(&neighbor){
            if let Some(parent_node) = parent {
                if neighbor != *parent_node {
                    let min_time = low_time[&node].min(discovery_time[&neighbor]);
                    low_time.insert(node.clone(), min_time);
                }
            }
            continue
        }

        children += 1;
        let mut child_results = cut_vertices(
            graph, 
            visited, 
            discovery_time, 
            low_time, 
            neighbor.clone(), 
            time + 1, 
            Some(&node)
        );

        results.extend(child_results.drain());

        let lo = low_time[&neighbor].min(low_time[&node]);
        low_time.insert(node.clone(), lo);

        if parent.is_none() && children > 1 {
            results.insert(node.clone());
        } else if parent.is_some() && lo >= discovery_time[&node] {
            results.insert(node.clone());
        }
    }

    results
}


#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::hex_grid::{HexGrid};
    use crate::piece::PieceType::*;
    use crate::piece::PieceColor::*;
    use crate::piece::*;

    #[test]
    fn test_cut_vertices() {
        let mut visited = HashSet::new();
        let mut discovery_time = HashMap::new();
        let mut low_time = HashMap::new();
        
        let grid = concat!(
            ". . . . . . .\n",
            " . . L B . Q .\n",
            ". l M . A S .\n",
            " . a G . . . .\n",
            ". . a a . . .\n",
            " . . . . . . .\n\n",
            "start - [ 0 -2 ]\n\n",
        );

        let graph = HexGrid::from_dsl(grid);
        let node = graph.find(Piece::new(Queen, White)).unwrap().0;

        let cut_vertices = cut_vertices(
            &graph, 
            &mut visited, 
            &mut discovery_time, 
            &mut low_time, 
            node,
            0, 
            None
        );

        let beetle = graph.find(Piece::new(Beetle, White)).unwrap().0;
        let ant  = graph.find(Piece::new(Ant, White)).unwrap().0;
        let spider = graph.find(Piece::new(Spider, White)).unwrap().0;
        let ladybug = graph.find(Piece::new(Ladybug, White)).unwrap().0;
        let mosquito = graph.find(Piece::new(Mosquito, White)).unwrap().0;
        let grasshopper = graph.find(Piece::new(Grasshopper, White)).unwrap().0;


        assert!(cut_vertices.contains(&beetle));
        assert!(cut_vertices.contains(&ant));
        assert!(cut_vertices.contains(&spider));
        assert!(cut_vertices.contains(&ladybug));
        assert!(cut_vertices.contains(&mosquito));
        println!("Cut vertices: {:?}", cut_vertices);
        // for every hex location insert a placeholder/wildcard into a new hexgrid 
        // so we can debug it
        let mut debug_grid = HexGrid::new();
        for loc in cut_vertices.clone(){
            debug_grid.add(Piece::new(WildCard, White), loc.clone());
        }
        println!("Debug grid:\n{}", debug_grid.to_dsl());
        assert!(!cut_vertices.contains(&node));
        assert!(!cut_vertices.contains(&grasshopper));
        assert!(cut_vertices.len() == 5);
    }

    #[test]
    fn test_cut_vertices_regression() {
        let mut visited = HashSet::new();
        let mut discovery_time = HashMap::new();
        let mut low_time = HashMap::new();
        
        let grid = concat!(
            ". . . . . . . . . . . . . .\n",
            " . . . . . Q . . . . . . . .\n",
            ". . . . . P . . . . . . . .\n",
            " . . B . . G . . s . . . . .\n",
            ". . . G 2 m G b a . s g g .\n",
            " . a S A . . b . a g . A . .\n",
            ". S . . . . l p . q . . . .\n",
            " . . . . . M . A . . . . . .\n",
            ". . . . . . . . . . . . . .\n\n",
            "start - [ 2 -6 ]\n\n",
            "2 - [ L B ]\n",
        );


        let graph = HexGrid::from_dsl(grid);
        let node = graph.find(Piece::new(Queen, White)).unwrap().0;

        let cut_vertices = cut_vertices(
            &graph, 
            &mut visited, 
            &mut discovery_time, 
            &mut low_time, 
            node,
            0, 
            None
        );

        let black_ladybug = graph.find(Piece::new(Ladybug, Black)).unwrap().0;
        assert!(cut_vertices.len() > 0);
        assert!(cut_vertices.contains(&black_ladybug));
    }
}

