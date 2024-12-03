use thiserror::Error;
use crate::uhp::GameType;

#[derive(Error, Debug)]
pub enum HexGridError {
    #[error("String input cannot be converted to piece")]
    PieceError,
}

pub type Result<T> = std::result::Result<T, HexGridError>;

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

    pub fn all(game_type : GameType) -> Vec<PieceType> {
        use PieceType::*;
        use GameType::*;
        match game_type {
            Standard => vec![Queen, Grasshopper, Spider, Beetle, Ant],
            M => vec![Queen, Grasshopper, Spider, Beetle, Ant, Mosquito],
            MP => vec![Queen, Grasshopper, Spider, Beetle, Ant, Mosquito, Pillbug],
            ML => vec![Queen, Grasshopper, Spider, Beetle, Ant, Mosquito, Ladybug],
            MLP => vec![Queen, Grasshopper, Spider, Beetle, Ant, Mosquito, Pillbug, Ladybug],
            L => vec![Queen, Grasshopper, Spider, Beetle, Ant, Ladybug],
            LP => vec![Queen, Grasshopper, Spider, Beetle, Ant, Ladybug, Pillbug],
            P => vec![Queen, Grasshopper, Spider, Beetle, Ant, Pillbug],
        }
    }

    pub fn try_from_char(c: &char) -> Result<PieceType> {
        let string = c.to_string();
        PieceType::try_from_str(&string)
    }
    pub fn try_from_str(string: &str) -> Result<PieceType> {
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
    pub piece: PieceType,
    pub color: PieceColor,
}

impl Piece {
    pub fn new(piece: PieceType, color: PieceColor) -> Piece {
        Piece { piece, color }
    }

    /// Uppercase letter for white, lowercase for black
    pub fn to_str(&self) -> String {
        let piece_str = match self.color {
            PieceColor::White => self.piece.to_str().to_uppercase(),
            PieceColor::Black => self.piece.to_str().to_lowercase(),
        };

        piece_str
    }

    pub fn to_uhp(&self, id: u8) -> String {
        match self.color {
            PieceColor::White => format!("w{}{}", self.piece.to_str(), id),
            PieceColor::Black => format!("b{}{}", self.piece.to_str(), id),
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
        self.piece.hash(state);
        self.color.hash(state);
    }
}
