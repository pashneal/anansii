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
}

impl HexLocation {
    pub fn new(x: i8, y: i8) -> HexLocation {
        HexLocation { x, y }
    }

    /// Applies a direction to this location, 
    /// returning the new location. This function is deterministic.
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

impl FromHex for HexLocation {
    fn from_hex(hex: HexLocation) -> HexLocation {
        hex
    }
}

pub trait FromHex: PartialEq + std::fmt::Debug {
    fn from_hex(hex: HexLocation) -> Self;
}

pub trait Shiftable {
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
}
