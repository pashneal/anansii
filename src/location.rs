#[derive(Copy, Clone, Debug)]
pub enum Direction {
    NW,
    NE,
    E,
    SE,
    SW,
    W,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct HexLocation {
    pub x: i8,
    pub y: i8,
}

impl Direction {
    pub fn all() -> Vec<Direction> {
        use Direction::*;
        vec![NW, NE, E, SE, SW, W]
    }

    /// Returns the two directions that are adjacent to this one.
    ///
    /// Edges are labeled as follows:
    ///
    ///   NW - NE
    ///   /     \
    ///  W       E
    ///   \     /
    ///   SW - SE
    ///
    /// For example, the adjacent directions of `NW` are `W` and `NE`.
    pub fn adjacent(&self) -> (Direction, Direction) {
        use Direction::*;
        match self {
            NW => (W, NE),
            NE => (NW, E),
            E => (NE, SE),
            SE => (E, SW),
            SW => (SE, W),
            W => (SW, NW),
        }
    }

    pub fn opposite(&self) -> Direction {
        use Direction::*;
        match self {
            NW => SE,
            NE => SW,
            E => W,
            SE => NW,
            SW => NE,
            W => E,
        }
    }
}

impl HexLocation {
    pub fn new(x: i8, y: i8) -> HexLocation {
        HexLocation { x, y }
    }

    pub fn apply(&self, direction: Direction) -> Self {
        use Direction::*;
        let (mut x, mut y) = (self.x, self.y);
        match direction {
            NW => y -= 1,
            E => x += 1,
            W => x -= 1,
            SE => y += 1,
            NE => {
                x += 1;
                y -= 1
            }
            SW => {
                x -= 1;
                y += 1
            }
        }
        HexLocation::new(x, y)
    }

    pub fn add(&self, other: HexLocation) -> HexLocation {
        HexLocation::new(self.x + other.x, self.y + other.y)
    }
}

impl Shiftable for HexLocation {
    fn shift_west(&self) -> HexLocation {
        self.apply(Direction::W)
    }

    fn shift_east(&self) -> HexLocation {
        self.apply(Direction::E)
    }

    fn shift_northwest(&self) -> HexLocation {
        self.apply(Direction::NW)
    }

    fn shift_northeast(&self) -> HexLocation {
        self.apply(Direction::NE)
    }

    fn shift_southwest(&self) -> HexLocation {
        self.apply(Direction::SW)
    }

    fn shift_southeast(&self) -> HexLocation {
        self.apply(Direction::SE)
    }

    fn center() -> HexLocation {
        HexLocation::new(0, 0)
    }
}

impl FromHexLocation for HexLocation {
    fn from_hex(hex: HexLocation) -> HexLocation {
        hex
    }
}

pub trait FromHexLocation: PartialEq + std::fmt::Debug + Sized + Clone {
    // Creates a new location from the given hex location. 
    //
    // Guarantees that every HexLocation has at least one
    // corresponding location of the Self type, and that the same HexLocation 
    // will always produce the same location of this type.
    fn from_hex(hex: HexLocation) -> Self;
}

pub trait Shiftable : std::hash::Hash + Eq + Clone + FromHexLocation {
    // Deterministically shifts the location west by one hex.
    fn shift_west(&self) -> Self;
    // Deterministically shifts the location east by one hex.
    fn shift_east(&self) -> Self;
    // Deterministically shifts the location northwest by one hex.
    fn shift_northwest(&self) -> Self;
    // Deterministically shifts the location northeast by one hex.
    fn shift_northeast(&self) -> Self;
    // Deterministically shifts the location southwest by one hex.
    fn shift_southwest(&self) -> Self;
    // Deterministically shifts the location southeast by one hex.
    fn shift_southeast(&self) -> Self;
    // Returns a consistent center location for the given type.
    fn center() -> Self;

    fn apply(&self, direction: Direction) -> Self
    where
        Self: Sized,
    {
        match direction {
            Direction::NW => self.shift_northwest(),
            Direction::NE => self.shift_northeast(),
            Direction::E => self.shift_east(),
            Direction::SE => self.shift_southeast(),
            Direction::SW => self.shift_southwest(),
            Direction::W => self.shift_west(),
        }
    }

}

pub trait Distanceable: Shiftable {
    fn distance(&self, other: &Self) -> (i8, i8); 

    fn distance_func(&self, other: Self) -> impl Fn(Self) -> Self {
        move |loc: Self| {
            let (mut dx, mut dy) = self.distance(&loc);
            let mut loc = loc;
            while dx > 0 {
                loc = loc.shift_east();
                dx -= 1;
            }
            while dx < 0 {
                loc = loc.shift_west();
                dx += 1;
            }
            while dy > 0 {
                loc = loc.shift_southeast();
                dy -= 1;
            }
            while dy < 0 {
                loc = loc.shift_northwest();
                dy += 1;
            }

            loc
        }
    }

    // Returns true if the two Vec<Self> are isomorphic, meaning that there exists a
    // bijection between the two sets of locations that preserves relative distances (after
    // shifting).
    fn is_isomorphic(shape1: Vec<Self>, shape2: Vec<Self>) -> bool {
        if shape1.len() != shape2.len() {
            return false;
        }
        if shape1.is_empty() {
            return true;
        }

        // we can try a bijection from each element of shape2 to the first element of shape1, and
        // see if any of them work. We can construct a distance function
        // that transforms all of needle into haystack and perform set equality
        // it's slow but it works and we are going for correctness right now

        for candidate in shape2.clone().into_iter() {
            let distance_func = shape1[0].distance_func(candidate);
            let transformed_shape: Vec<Self> = shape1.iter().map(|n| distance_func(n.clone())).collect();
            if transformed_shape.iter().all(|n| shape2.contains(n)) {
                return true;
            }
        }


        false
    }

    // returns an isomorphic_func for the lifetime of the two shapes,
    // meaning that it transforms shape1 into shape2 and preserves relative
    // distances (after shifting).
    fn isomorphic_func<'a>(shape1: &'a Vec<Self>, shape2: &'a Vec<Self>) -> Option<impl Fn(Self) -> Self + 'a> {
        if shape1.len() != shape2.len() {
            return None;
        }
        if shape1.is_empty() {
            return None;
        }

        for candidate in shape2.clone().into_iter() {
            let distance_func = shape1[0].distance_func(candidate);
            let transformed_shape: Vec<Self> = shape1.iter().map(|n| distance_func(n.clone())).collect();
            if transformed_shape.iter().all(|n| shape2.contains(n)) {
                return Some(distance_func);
            }
        }

        None
    }

}


impl Distanceable for HexLocation {
    fn distance(&self, other: &Self) -> (i8, i8) {
        let dx = other.x - self.x;
        let dy = other.y - self.y;
        (dx, dy)
    }
}

pub trait Topology<T: Shiftable> {
    fn connected_components(shiftables: Vec<T>) -> Vec<Vec<T>>;
    fn route(start: T, available: Vec<T>) -> Vec<Direction>;
}

// TODO: replace the many many implementations of 
// Topology across the codebase with this generic implementation.
impl <T: Shiftable> Topology<T> for T {
    fn connected_components(shiftables: Vec<T>) -> Vec<Vec<T>>
    {
        let mut components = Vec::new();
        let mut visited = std::collections::HashSet::new();

        for shiftable in shiftables.clone().into_iter() {
            if visited.contains(&shiftable) {
                continue;
            }

            let mut component = Vec::new();
            let mut stack = vec![shiftable];

            while let Some(current) = stack.pop() {
                if visited.contains(&current) {
                    continue;
                }
                visited.insert(current.clone());
                component.push(current.clone());

                for direction in Direction::all() {
                    let neighbor = current.apply(direction);
                    if shiftables.contains(&neighbor) && !visited.contains(&neighbor) {
                        stack.push(neighbor);
                    }
                }
            }

            components.push(component);
        }

        components
    }

    fn route(start: T, available: Vec<T>) -> Vec<Direction> {
        assert!(
            available.contains(&start),
            "Start location must be in the list of available locations."
        );

        assert!(
            Self::connected_components(available.clone()).len() == 1,
            "Num components must be == 1"
        );

        let mut visited = std::collections::HashSet::new();
        let mut route = Vec::new();

        fn dfs<T: Shiftable>(current: T, available: &Vec<T>, visited: &mut std::collections::HashSet<T>, route: &mut Vec<Direction>) {
            visited.insert(current.clone());

            for direction in Direction::all() {
                let neighbor = current.apply(direction);
                if available.contains(&neighbor) && !visited.contains(&neighbor) {
                    route.push(direction);
                    dfs(neighbor, available, visited, route);
                    route.push(direction.opposite());
                }
            }
        }

        dfs(start, &available, &mut visited, &mut route);

        route
    }

}
