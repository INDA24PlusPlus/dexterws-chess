use crate::{
    game::{Color, Move, Piece, Rank, Square},
    internals::BitBoard,
};

/// Generate all possible pawn attack squares
/// Does not check for enemey pieces
pub(crate) fn pawn_pseudo_attacks(color: Color, pawn: BitBoard) -> BitBoard {
    let pawns = pawn.0;
    // Opposite direction
    let pawns = if color == Color::White {
        pawns
    } else {
        pawns.reverse_bits()
    };
    let left = pawns << 7 & !BitBoard::FILE_H.0;
    let right = pawns << 9 & !BitBoard::FILE_A.0;
    BitBoard(if color == Color::White {
        left | right
    } else {
        (left | right).reverse_bits()
    })
}

/// Generate all possible single pushes for pawns
fn pawn_single_pushes(color: Color, pawn: BitBoard) -> BitBoard {
    let pawns = pawn.0;
    let pushes = if color == Color::White {
        pawns << 8
    } else {
        pawns >> 8
    };
    BitBoard(pushes)
}

/// Generate all possible double pushes for pawns
fn pawn_double_pushes(color: Color, pawn: BitBoard) -> BitBoard {
    let pawns = pawn.0;
    // Take the intersection of the double pushes and the rank 4 or 5 to only allow double pushes from the starting rank
    let pushes = if color == Color::White {
        pawns << 16 & BitBoard::RANK_4.0
    } else {
        pawns >> 16 & BitBoard::RANK_5.0
    };
    BitBoard(pushes)
}

pub(crate) fn pawn_moves(
    color: Color,
    pawn: Square,
    ctx: (BitBoard, BitBoard),
    en_passant: Option<BitBoard>,
) -> Vec<Move> {
    let (own, other) = ctx;
    let position = pawn.to_bitboard();

    let single_pushes = pawn_single_pushes(color, position) & (!own | !other);
    let double_pushes = pawn_double_pushes(color, position) & (!own | !other);
    let attacks =
        pawn_pseudo_attacks(color, position) & (other | en_passant.unwrap_or(BitBoard(0)));

    let moves_bitboard = single_pushes | double_pushes | attacks;
    let count = moves_bitboard.popcnt();
    let mut moves = Vec::with_capacity(count as usize);
    for idx in moves_bitboard {
        let to = Square::from_idx(idx);
        let should_promote = if color == Color::White {
            to.rank == Rank::Eight
        } else {
            to.rank == Rank::One
        };
        if should_promote {
            moves.push(Move::new(pawn, to, Some(Piece::Queen)));
        } else {
            moves.push(Move::new(pawn, to, None));
        }
    }
    moves
}

pub(crate) fn knight_bitboard_moves(knight: Square, ctx: (BitBoard, BitBoard)) -> BitBoard {
    let (own, other) = ctx;
    let position = knight.to_bitboard();

    let jumps = ((position.0 << 17 | position.0 >> 15) & !BitBoard::FILE_A.0
        | (position.0 << 15 | position.0 >> 17) & !BitBoard::FILE_H.0
        | (position.0 << 10 | position.0 >> 6) & !(BitBoard::FILE_G.0 | BitBoard::FILE_H.0)
        | (position.0 << 6 | position.0 >> 10) & !(BitBoard::FILE_A.0 | BitBoard::FILE_B.0))
        // Remove own pieces
        & (!own).0;
    BitBoard(jumps)
}

pub(crate) fn knight_moves(knight: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    let jumps = knight_bitboard_moves(knight, ctx);

    let count = jumps.popcnt();
    let mut moves = Vec::with_capacity(count as usize);
    for idx in jumps {
        moves.push(Move::new(knight, Square::from_idx(idx), None));
    }
    moves
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Direction {
    North,
    South,
    East,
    West,
    NorthEast,
    NorthWest,
    SouthEast,
    SouthWest,
}

pub(crate) const DIAGONALS: [Direction; 4] = [
    Direction::NorthEast,
    Direction::NorthWest,
    Direction::SouthEast,
    Direction::SouthWest,
];

pub(crate) const ORTHAGONALS: [Direction; 4] = [
    Direction::North,
    Direction::South,
    Direction::East,
    Direction::West,
];

pub(crate) const ALL_DIRECTIONS: [Direction; 8] = [
    Direction::North,
    Direction::South,
    Direction::East,
    Direction::West,
    Direction::NorthEast,
    Direction::NorthWest,
    Direction::SouthEast,
    Direction::SouthWest,
];

impl Direction {
    pub(crate) fn shift(&self, square: BitBoard) -> BitBoard {
        match self {
            Direction::North => square << BitBoard(8),
            Direction::South => square >> BitBoard(8),
            Direction::East => (square & !BitBoard::FILE_H) << BitBoard(1),
            Direction::West => (square & !BitBoard::FILE_A) >> BitBoard(1),
            Direction::NorthEast => (square & !BitBoard::FILE_H) << BitBoard(9),
            Direction::NorthWest => (square & !BitBoard::FILE_A) << BitBoard(7),
            Direction::SouthEast => (square & !BitBoard::FILE_H) >> BitBoard(7),
            Direction::SouthWest => (square & !BitBoard::FILE_A) >> BitBoard(9),
        }
    }

    pub(crate) fn from_squares(from: Square, to: Square) -> Direction {
        let dx = to.file.to_idx() as i8 - from.file.to_idx() as i8;
        let dy = to.rank.to_idx() as i8 - from.rank.to_idx() as i8;
        let diag = dx.abs() == dy.abs();
        let orth = dx == 0 || dy == 0;
        match (dx, dy) {
            (0, y) if y > 0 => Direction::North,
            (0, y) if y < 0 => Direction::South,
            (x, 0) if x > 0 => Direction::East,
            (x, 0) if x < 0 => Direction::West,
            (x, y) if x > 0 && y > 0 && diag => Direction::NorthEast,
            (x, y) if x < 0 && y > 0 && diag => Direction::NorthWest,
            (x, y) if x > 0 && y < 0 && diag => Direction::SouthEast,
            (x, y) if x < 0 && y < 0 && diag => Direction::SouthWest,
            _ => panic!("Invalid direction"),
        }
    }

    pub(crate) fn opposite(&self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::East => Direction::West,
            Direction::West => Direction::East,
            Direction::NorthEast => Direction::SouthWest,
            Direction::NorthWest => Direction::SouthEast,
            Direction::SouthEast => Direction::NorthWest,
            Direction::SouthWest => Direction::NorthEast,
        }
    }
}

pub(crate) fn get_slide_direction_bitboard(
    piece: Square,
    direction: Direction,
    ctx: (BitBoard, BitBoard),
) -> BitBoard {
    let (own, other) = ctx;
    let mut current = direction.shift(piece.to_bitboard());
    let mut moves = current;
    while current & !own & !other != BitBoard(0) {
        current = direction.shift(current);
        moves = moves | current;
    }
    moves = moves & !own;
    moves
}

/// Same as get_slide_direction_moves but only stops at edges
pub(crate) fn get_slide_direction_xray(piece: Square, direction: Direction) -> BitBoard {
    let mut current = direction.shift(piece.to_bitboard());
    let mut moves = current;
    while current != BitBoard(0) {
        current = direction.shift(current);
        moves = moves | current;
    }
    moves
}

pub(crate) fn get_slider_moves(
    piece: Square,
    directions: &[Direction],
    ctx: (BitBoard, BitBoard),
) -> Vec<Move> {
    let mut slides = directions
        .iter()
        .map(|dir| get_slide_direction_bitboard(piece, *dir, ctx))
        .fold(BitBoard(0), |acc, x| acc | x);
    let count = slides.popcnt();
    let mut moves = Vec::with_capacity(count as usize);
    for idx in slides {
        moves.push(Move::new(piece, Square::from_idx(idx), None));
    }
    moves
}

pub(crate) fn get_slider_xrays(piece: Square, directions: &[Direction]) -> BitBoard {
    let slides = directions
        .iter()
        .map(|dir| get_slide_direction_xray(piece, *dir))
        .fold(BitBoard(0), |acc, x| acc | x);
    slides
}

pub(crate) fn bishop_moves(bishop: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    get_slider_moves(bishop, &DIAGONALS, ctx)
}

pub(crate) fn bishop_xrays(bishop: Square) -> BitBoard {
    get_slider_xrays(bishop, &DIAGONALS)
}

pub(crate) fn rook_moves(rook: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    get_slider_moves(rook, &ORTHAGONALS, ctx)
}

pub(crate) fn rook_xrays(rook: Square) -> BitBoard {
    get_slider_xrays(rook, &ORTHAGONALS)
}

pub(crate) fn queen_moves(queen: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    get_slider_moves(queen, &ALL_DIRECTIONS, ctx)
}

pub(crate) fn queen_xrays(queen: Square) -> BitBoard {
    get_slider_xrays(queen, &ALL_DIRECTIONS)
}

pub(crate) fn king_moves(king: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    let (own, other) = ctx;
    let position = king.to_bitboard();

    let walks = (position.0 << 8
        | position.0 >> 8
        | position.0 << 1
        | position.0 >> 1
        | position.0 << 9
        | position.0 >> 9
        | position.0 << 7
        | position.0 >> 7)
        & (!own).0;
    let walks = BitBoard(walks);

    let count = walks.popcnt();
    let mut moves = Vec::with_capacity(count as usize);
    for idx in walks {
        moves.push(Move::new(king, Square::from_idx(idx), None));
    }
    moves
}

pub(crate) fn xray_subsection(from: Square, to: Square) -> BitBoard {
    let direction = Direction::from_squares(from, to);
    let mut pos = BitBoard::from_square(from);
    let mut ray = BitBoard(0);
    loop {
        pos = direction.shift(pos);
        if Square::from_bitboard(pos) == to {
            break;
        }
        ray = ray | pos;
    }
    ray
}

mod test {
    use crate::{game::Square, square};

    #[test]
    pub fn test_pawn_pseudo_attacks() {
        use crate::game::Color;
        use crate::internals::BitBoard;
    }

    pub fn test_rook_moves() {
        use crate::game::Color;
        use crate::internals::BitBoard;
        let rook = square!(Four, D);
        let moves = super::rook_moves(rook, (BitBoard(0), BitBoard(0)));
        let mut moves_total = BitBoard(0);
        for m in moves {
            moves_total = moves_total | m.to().to_bitboard();
        }
    }

    #[test]
    fn test_bishop_moves() {
        use crate::game::Color;
        use crate::internals::BitBoard;
    }
}
