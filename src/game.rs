use crate::internals::{BitBoard};

enum Piece {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

enum Color {
    White,
    Black,
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
    
    fn all(&self) -> BitBoard {
        self.colored(Color::White) | self.colored(Color::Black)
    }

    fn pawn(&self, color: Color) -> BitBoard {
        let mut captures = BitBoard(0);
        todo!()
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
        let board = Board::new();
    }
}
