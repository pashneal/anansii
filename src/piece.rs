use crate::hex_grid::{HexGrid, HexGridError};
use crate::location::HexLocation;
use crate::uhp::GameType;

type Result<T> = std::result::Result<T, HexGridError>;

pub const PIECE_COUNTS: [(PieceType, usize); 8] = [
    (PieceType::Queen, 1),
    (PieceType::Grasshopper, 3),
    (PieceType::Spider, 2),
    (PieceType::Beetle, 2),
    (PieceType::Ant, 3),
    (PieceType::Pillbug, 1),
    (PieceType::Ladybug, 1),
    (PieceType::Mosquito, 1),
];

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PieceType {
    Queen,
    Grasshopper,
    Spider,
    Beetle,
    Ant,
    Pillbug,
    Ladybug,
    Mosquito,
}

impl PieceType {
    pub fn to_str(&self) -> &str {
        use PieceType::*;
        match self {
            Queen => "Q",
            Grasshopper => "G",
            Spider => "S",
            Beetle => "B",
            Ant => "A",
            Pillbug => "P",
            Ladybug => "L",
            Mosquito => "M",
        }
    }

    pub fn all(game_type: GameType) -> Vec<PieceType> {
        use GameType::*;
        use PieceType::*;
        match game_type {
            Standard => vec![Queen, Grasshopper, Spider, Beetle, Ant],
            M => vec![Queen, Grasshopper, Spider, Beetle, Ant, Mosquito],
            MP => vec![Queen, Grasshopper, Spider, Beetle, Ant, Mosquito, Pillbug],
            ML => vec![Queen, Grasshopper, Spider, Beetle, Ant, Mosquito, Ladybug],
            MLP => vec![
                Queen,
                Grasshopper,
                Spider,
                Beetle,
                Ant,
                Mosquito,
                Pillbug,
                Ladybug,
            ],
            L => vec![Queen, Grasshopper, Spider, Beetle, Ant, Ladybug],
            LP => vec![Queen, Grasshopper, Spider, Beetle, Ant, Ladybug, Pillbug],
            P => vec![Queen, Grasshopper, Spider, Beetle, Ant, Pillbug],
        }
    }

    fn try_from_char(c: &char) -> Result<PieceType> {
        let string = c.to_string();
        PieceType::try_from_str(&string)
    }
    fn try_from_str(string: &str) -> Result<PieceType> {
        use PieceType::*;
        match string.to_uppercase().as_str() {
            "Q" => Ok(Queen),
            "G" => Ok(Grasshopper),
            "S" => Ok(Spider),
            "B" => Ok(Beetle),
            "A" => Ok(Ant),
            "P" => Ok(Pillbug),
            "L" => Ok(Ladybug),
            "M" => Ok(Mosquito),
            _ => Err(HexGridError::PieceError),
        }
    }
}

impl TryFrom<char> for PieceType {
    type Error = HexGridError;
    fn try_from(c: char) -> Result<PieceType> {
        PieceType::try_from_char(&c)
    }
}

impl TryFrom<&str> for PieceType {
    type Error = HexGridError;
    fn try_from(s: &str) -> Result<PieceType> {
        PieceType::try_from_str(s)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PieceColor {
    Black,
    White,
}

impl PieceColor {
    pub fn opposite(&self) -> PieceColor {
        match self {
            PieceColor::White => PieceColor::Black,
            PieceColor::Black => PieceColor::White,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            PieceColor::White => "White",
            PieceColor::Black => "Black",
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Piece {
    pub piece_type: PieceType,
    pub color: PieceColor,
}

impl Default for Piece {
    fn default() -> Piece {
        Piece {
            piece_type: PieceType::Ant,
            color: PieceColor::Black,
        }
    }
}

impl Piece {
    pub fn new(piece: PieceType, color: PieceColor) -> Piece {
        Piece {
            piece_type: piece,
            color,
        }
    }

    /// Uppercase letter for white, lowercase for black
    pub fn to_str(&self) -> String {
        let piece_str = match self.color {
            PieceColor::White => self.piece_type.to_str().to_uppercase(),
            PieceColor::Black => self.piece_type.to_str().to_lowercase(),
        };

        piece_str
    }

    pub fn to_uhp(&self, id: u8) -> String {
        match self.color {
            PieceColor::White => format!("w{}{}", self.piece_type.to_str(), id),
            PieceColor::Black => format!("b{}{}", self.piece_type.to_str(), id),
        }
    }

    pub fn from_uhp(uhp: &str) -> Result<Piece> {
        let color = match &uhp[0..1] {
            "w" => PieceColor::White,
            "b" => PieceColor::Black,
            _ => return Err(HexGridError::PieceError),
        };

        let piece = PieceType::try_from_char(&uhp.chars().nth(1).unwrap())?;
        Ok(Piece::new(piece, color))
    }
}

impl std::hash::Hash for Piece {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.piece_type.hash(state);
        self.color.hash(state);
    }
}

pub trait IntoPieces {
    /// Returns a list of pieces and their locations in "board order", that
    /// is first by row top to bottom then by column left to right,
    /// and with the stacks of pieces from bottom to top.
    ///
    /// The returned list is required to be deterministic given
    /// the same state of the type.
    fn pieces(&self) -> Vec<(Vec<Piece>, HexLocation)>;

    fn to_hex_grid(&self) -> HexGrid {
        HexGrid::from_pieces(self.pieces())
    }
}
