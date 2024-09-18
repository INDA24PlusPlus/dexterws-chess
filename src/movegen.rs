use crate::{game::{Color, Move, Piece, Rank, Square}, internals::BitBoard};

fn pawn_pseudo_attacks(color: Color, pawn: BitBoard) -> BitBoard {
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

fn pawn_single_pushes(color: Color, pawn: BitBoard) -> BitBoard {
    let pawns = pawn.0;
    let pushes = if color == Color::White {
        pawns << 8
    } else {
        pawns >> 8
    };
    BitBoard(pushes)
}

fn pawn_double_pushes(color: Color, pawn: BitBoard) -> BitBoard {
    let pawns = pawn.0;
    let pushes = if color == Color::White {
        pawns << 16 & BitBoard::RANK_4.0
    } else {
        pawns >> 16 & BitBoard::RANK_5.0
    };
    BitBoard(pushes)
}

fn pawn_moves(color: Color, pawn: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    let (own, other) = ctx;
    let position = pawn.to_bitboard();
    let single_pushes = pawn_single_pushes(color, position) & (!own | !other);
    let double_pushes = pawn_double_pushes(color, position) & (!own | !other);
    let attacks = pawn_pseudo_attacks(color, position) & other;
    let moves_bitboard = single_pushes | double_pushes | attacks;
    let count = moves_bitboard.popcnt();
    let mut moves = Vec::with_capacity(count as usize);
    for idx in moves_bitboard {
        let to = Square::from_idx(idx);
        let should_promote = if color == Color::White {
            to.rank() == Rank::Eight
        } else {
            to.rank() == Rank::One
        };
        if should_promote {
            moves.push(Move::new(pawn, to, Some(Piece::Queen)));
        } else {
            moves.push(Move::new(pawn, to, None));
        }
    }
    moves
}

fn knight_moves(knight: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    let (own, other) = ctx;
    let position = knight.to_bitboard();
    let jumps = ((position.0 << 17 | position.0 >> 15) & !BitBoard::FILE_A.0
        | (position.0 << 15 | position.0 >> 17) & !BitBoard::FILE_H.0
        | (position.0 << 10 | position.0 >> 6) & !(BitBoard::FILE_G.0 | BitBoard::FILE_H.0)
        | (position.0 << 6 | position.0 >> 10) & !(BitBoard::FILE_A.0 | BitBoard::FILE_B.0)) & (!own).0;
    let jumps = BitBoard(jumps);
    let count = jumps.popcnt();
    let mut moves = Vec::with_capacity(count as usize);
    for idx in jumps {
        moves.push(Move::new(knight, Square::from_idx(idx), None));
    }
    moves
}

fn bishop_moves(bishop: Square, ctx: (BitBoard, BitBoard)) -> Vec<Move> {
    let (own, other) = ctx;
    let position = bishop.to_bitboard();
    let attacks = {
        let mut attacks = BitBoard(0);
        let mut idx = bishop.idx();
        while idx % 8 < 7 && idx / 8 < 7 {
            idx += 9;
            attacks.0 |= 1 << idx;
        }
        let mut idx = bishop.idx();
        while idx % 8 > 0 && idx / 8 < 7 {
            idx += 7;
            attacks.0 |= 1 << idx;
        }
        let mut idx = bishop.idx();
        while idx % 8 < 7 && idx / 8 > 0 {
            idx -= 7;
            attacks.0 |= 1 << idx;
        }
        let mut idx = bishop.idx();
        while idx % 8 > 0 && idx / 8 > 0 {
            idx -= 9;
            attacks.0 |= 1 << idx;
        }
        attacks
    };
    let attacks = attacks & !own;
    let count = attacks.popcnt();
    let mut moves = Vec::with_capacity(count as usize);
    for idx in attacks {
        moves.push(Move::new(bishop, Square::from_idx(idx), None));
    }
    moves
}


mod test {
    #[test]
    pub fn test_pawn_pseudo_attacks() {
        use crate::game::Color;
        use crate::internals::BitBoard;
        let pawns = BitBoard(0x0000000000000200);
        println!("{}", pawns);
        let attacks = super::pawn_pseudo_attacks(Color::Black, pawns);
        println!("{}", attacks);
    }
}
