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
    /// These directions are "gates" that pieces are not allowed to squeeze
    /// between if both gates are occupied
    ///
    /// Edges are labeled as follows:
    ///
    ///   NW - NE
    ///   /     \
    ///  W       E
    ///   \     /
    ///   SW - SE
    ///
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

impl Location for HexLocation {
    fn to_hex(&self) -> HexLocation {
        *self
    }
    fn from_hex(hex: HexLocation) -> Self {
        hex
    }
}

pub trait Location {
    fn to_hex(&self) -> HexLocation;
    fn from_hex(hex: HexLocation) -> Self; 
}
