use crate::internals::{BitBoard};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Piece {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}


#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Rank {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

impl Rank {
    /// Convert a rank to a bitboard
    pub fn to_bitboard(&self) -> BitBoard {
        match self {
            Rank::One => BitBoard::RANK_1,
            Rank::Two => BitBoard::RANK_2,
            Rank::Three => BitBoard::RANK_3,
            Rank::Four => BitBoard::RANK_4,
            Rank::Five => BitBoard::RANK_5,
            Rank::Six => BitBoard::RANK_6,
            Rank::Seven => BitBoard::RANK_7,
            Rank::Eight => BitBoard::RANK_8,
        }
    }

    /// Convert a rank to an index
    pub fn to_idx(&self) -> u8 {
        match self {
            Rank::One => 0,
            Rank::Two => 1,
            Rank::Three => 2,
            Rank::Four => 3,
            Rank::Five => 4,
            Rank::Six => 5,
            Rank::Seven => 6,
            Rank::Eight => 7,
        }
    }
    
    /// Convert a rank index to a rank
    ///
    /// # Panics
    /// Panics if the index is not in the range 0..8
    pub fn from_idx(idx: u8) -> Rank {
        match idx {
            0 => Rank::One,
            1 => Rank::Two,
            2 => Rank::Three,
            3 => Rank::Four,
            4 => Rank::Five,
            5 => Rank::Six,
            6 => Rank::Seven,
            7 => Rank::Eight,
            _ => panic!("Invalid rank index"),
        }
    }
}



#[derive(Clone, Copy, PartialEq, Eq)]
pub enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

impl File {
    /// Convert a file to a bitboard
    pub fn to_bitboard(&self) -> BitBoard {
        match self {
            File::A => BitBoard::FILE_A,
            File::B => BitBoard::FILE_B,
            File::C => BitBoard::FILE_C,
            File::D => BitBoard::FILE_D,
            File::E => BitBoard::FILE_E,
            File::F => BitBoard::FILE_F,
            File::G => BitBoard::FILE_G,
            File::H => BitBoard::FILE_H,
        }
    }

    /// Convert a file to an index
    pub fn to_idx(&self) -> u8 {
        match self {
            File::A => 0,
            File::B => 1,
            File::C => 2,
            File::D => 3,
            File::E => 4,
            File::F => 5,
            File::G => 6,
            File::H => 7,
        }
    }

    /// Convert a file index to a file
    ///
    /// # Panics
    /// Panics if the index is not in the range 0..8
    pub fn from_idx(idx: u8) -> File {
        match idx {
            0 => File::A,
            1 => File::B,
            2 => File::C,
            3 => File::D,
            4 => File::E,
            5 => File::F,
            6 => File::G,
            7 => File::H,
            _ => panic!("Invalid file index"),
        }
    }
}


/// A square on the board
#[derive(Clone, Copy)]
pub struct Square {
    rank: Rank,
    file: File,
}

impl Square {
    /// Create a new square
    pub fn new(rank: Rank, file: File) -> Self {
        Square { rank, file }
    }

    /// Get the rank of the square
    pub fn rank(&self) -> Rank {
        self.rank
    }

    /// Get the file of the square
    pub fn file(&self) -> File {
        self.file
    }

    /// Convert a square to a bitboard
    pub fn to_bitboard(&self) -> BitBoard {
        BitBoard::from_square(self.file.to_idx(), self.rank.to_idx())
    }

    /// Convert a square to an index
    pub fn to_idx(&self) -> u8 {
        self.file.to_idx() + self.rank.to_idx() * 8
    }

    /// Convert an index to a square
    ///
    /// # Panics
    /// Panics if the index is not in the range 0..64
    pub fn from_idx(idx: u8) -> Square {
        let rank = Rank::from_idx(idx / 8);
        let file = File::from_idx(idx % 8);
        Square::new(rank, file)
    }

    pub fn from_file_rank(file: File, rank: Rank) -> Square {
        Square::new(rank, file)
    }
}

/// A move
#[derive(Clone, Copy)]
pub struct Move {
    from: Square,
    to: Square,
    promotion: Option<Piece>,
}

impl Move {
    /// Create a new move
    pub fn new(from: Square, to: Square, promotion: Option<Piece>) -> Self {
        Move { from, to, promotion }
    }

    /// Set the promotion piece
    pub fn set_promotion(&mut self, promotion: Piece) {
        self.promotion = Some(promotion);
    }

    /// Get the promotion piece
    pub fn promotion(&self) -> Option<Piece> {
        self.promotion
    }

    /// Get the from square
    pub fn from(&self) -> Square {
        self.from
    }

    /// Get the to square
    pub fn to(&self) -> Square {
        self.to
    }
}

impl std::ops::Not for Color {
    type Output = Color;

    fn not(self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Pieces {
    kings: BitBoard,
    queens: BitBoard,
    rooks: BitBoard,
    bishops: BitBoard,
    knights: BitBoard,
    pawns: BitBoard,
}

struct Board {
    pieces: [Pieces; 2],
    side: Color,
    en_passant: Option<BitBoard>,
    castling: BitBoard,
    promotion_pending: Option<BitBoard>,
}

impl Board {
    fn new() -> Self {
        unimplemented!();
        //Board {
        //    pieces: [
        //        Pieces {
        //            kings: BitBoard(1 << 4),
        //            queens: BitBoard(1 << 3),
        //            rooks: BitBoard(0b10000001),
        //            bishops: BitBoard(0b01000010),
        //            knights: BitBoard(0b00100100),
        //            pawns: BitBoard(0b0000000011111111 << 8),
        //        },
        //        Pieces {
        //            kings: BitBoard(1 << 60),
        //            queens: BitBoard(1 << 59),
        //            rooks: BitBoard(0b10000001 << 56),
        //            bishops: BitBoard(0b01000010 << 56),
        //            knights: BitBoard(0b00100100 << 56),
        //            pawns: BitBoard(0b0000000011111111 << 48),
        //        },
        //    ]
        //}
    }

    fn colored(&self, color: Color) -> BitBoard {
        let piece_set = self.pieces[color as usize];
        piece_set.kings
            | piece_set.queens
            | piece_set.rooks
            | piece_set.bishops
            | piece_set.knights
            | piece_set.pawns
    }

    fn colored_piece(&self, color: Color, piece: Piece) -> BitBoard {
        let piece_set = self.pieces[color as usize];
        match piece {
            Piece::King => piece_set.kings,
            Piece::Queen => piece_set.queens,
            Piece::Rook => piece_set.rooks,
            Piece::Bishop => piece_set.bishops,
            Piece::Knight => piece_set.knights,
            Piece::Pawn => piece_set.pawns,
        }
    }

    fn colored_piece_set(&self, color: Color) -> Pieces {
        self.pieces[color as usize]
    }
    
    fn all(&self) -> BitBoard {
        self.colored(Color::White) | self.colored(Color::Black)
    }

    fn empty(&self) -> BitBoard {
        !self.all()
    }

    pub fn get_piece(&self, square: Square) -> Option<(Color, Piece)> {
        let bitboard = square.to_bitboard();
        let is_white = self.colored(Color::White) & bitboard != BitBoard(0);
        let is_black = self.colored(Color::Black) & bitboard != BitBoard(0);
        if !is_white && !is_black {
            return None;
        }
        let color = if is_white { Color::White } else { Color::Black };
        let piece = if self.colored_piece(color, Piece::King) & bitboard != BitBoard(0) {
            Piece::King
        } else if self.colored_piece(color, Piece::Queen) & bitboard != BitBoard(0) {
            Piece::Queen
        } else if self.colored_piece(color, Piece::Rook) & bitboard != BitBoard(0) {
            Piece::Rook
        } else if self.colored_piece(color, Piece::Bishop) & bitboard != BitBoard(0) {
            Piece::Bishop
        } else if self.colored_piece(color, Piece::Knight) & bitboard != BitBoard(0) {
            Piece::Knight
        } else if self.colored_piece(color, Piece::Pawn) & bitboard != BitBoard(0) {
            Piece::Pawn
        } else {
            // This should never happen, if it does, the board state is corrupted
            return None;
        };
        Some((color, piece))
    }
}

struct State {
    board: Board,
}

mod test {
    use crate::internals::BitBoard;

    #[test]
    pub fn print_sides() {
        use crate::game::{Board, Color};
    }
}
