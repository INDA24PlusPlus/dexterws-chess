use std::collections::{HashMap, HashSet};

use crate::movegen::{
    bishop_moves, bishop_xrays, get_slide_direction_bitboard, get_slider_moves, get_slider_xrays,
    king_moves, knight_bitboard_moves, knight_moves, pawn_pseudo_attacks, queen_xrays, rook_moves,
    rook_xrays, xray_subsection, Direction, DIAGONALS,
};
use crate::{bitboard, internals::BitBoard, movegen::pawn_moves};

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Piece {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
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
    pub const fn to_idx(&self) -> u8 {
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

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
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
    pub const fn to_idx(&self) -> u8 {
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
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Square {
    pub rank: Rank,
    pub file: File,
}

impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            (self.file as u8 + 97) as char,
            self.rank as u8 + 1
        )
    }
}

/// Create a square from a rank and file
///
/// # Example
/// ```
/// let square = square!(One, A);
/// ```
#[macro_export(local_inner_macros)]
macro_rules! square {
    ($rank:ident, $file:ident) => {{
        use $crate::game::{File, Rank, Square};
        Square::new(Rank::$rank, File::$file)
    }};
}

impl Square {
    /// Create a new square from a rank and file
    pub fn new(rank: Rank, file: File) -> Self {
        Square { rank, file }
    }

    /// Convert a square to a bitboard
    pub(crate) fn to_bitboard(&self) -> BitBoard {
        BitBoard::from_square(*self)
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

    pub(crate) fn from_bitboard(bitboard: BitBoard) -> Square {
        let idx = bitboard.0.trailing_zeros() as u8;
        Square::from_idx(idx)
    }
}

/// A struct representing a move on the board
#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    from: Square,
    to: Square,
    promotion: Option<Piece>,
}

#[macro_export(local_inner_macros)]
macro_rules! mv {
    (($from_file:ident, $from_rank:ident), ($to_file:ident, $to_rank:ident)) => {
        {
            use $crate::game::Move;
            Move::new(square!($from_rank, $from_file), square!($to_rank, $to_file), None)
        }
    };
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.from, self.to,)
    }
}

impl Move {
    /// Create a new move
    pub fn new(from: Square, to: Square, promotion: Option<Piece>) -> Self {
        Move {
            from,
            to,
            promotion,
        }
    }

    /// Set the promotion piece
    /// This is only relevant for pawn moves
    /// when a move is returned from a generator, and the promotion square is reached
    /// it will default to a queen promotion. You can override this by setting the promotion
    /// to a different piece.
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

#[derive(Hash, Clone, Copy, PartialEq, Eq)]
struct Pieces {
    kings: BitBoard,
    queens: BitBoard,
    rooks: BitBoard,
    bishops: BitBoard,
    knights: BitBoard,
    pawns: BitBoard,
}

const LONG: File = File::A;
const LONG_KING_TO: File = File::C;
const LONG_ROOK_TO: File = File::D;
const SHORT: File = File::H;
const SHORT_KING_TO: File = File::G;
const SHORT_ROOK_TO: File = File::F;
struct Castling {
    short: bool,
    long: bool,
}

pub struct Board {
    pieces: [Pieces; 2],
    side: Color,
    en_passant: Option<BitBoard>,
    castling: [Castling; 2],
    pinned: BitBoard,
    checking: BitBoard,
    half_move_clock: u8,
    full_move_number: u64,
    seen_positions: HashMap<([Pieces; 2], Color), usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameResult {
    Checkmate { winner: Color },
    Stalemate,
    ThreefoldRepetition,
    FiftyMoveRule,
    Draw,
    InProgress,
}

impl Board {
    fn new() -> Self {
        Board {
            pieces: [
                Pieces {
                    kings: BitBoard(1 << 4),
                    queens: BitBoard(1 << 3),
                    rooks: BitBoard(0b10000001),
                    bishops: BitBoard(0b00100100),
                    knights: BitBoard(0b01000010),
                    pawns: BitBoard(0b0000000011111111 << 8),
                },
                Pieces {
                    kings: BitBoard(1 << 60),
                    queens: BitBoard(1 << 59),
                    rooks: BitBoard(0b10000001 << 56),
                    bishops: BitBoard(0b00100100 << 56),
                    knights: BitBoard(0b01000010 << 56),
                    pawns: BitBoard(0b0000000011111111 << 48),
                },
            ],
            side: Color::White,
            en_passant: None,
            castling: [
                Castling {
                    short: true,
                    long: true,
                },
                Castling {
                    short: true,
                    long: true,
                },
            ],
            pinned: BitBoard(0),
            checking: BitBoard(0),
            half_move_clock: 0,
            full_move_number: 0,
            seen_positions: HashMap::new(),
        }
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

    fn colored_piece_mut(&mut self, color: Color, piece: Piece) -> &mut BitBoard {
        let piece_set = &mut self.pieces[color as usize];
        match piece {
            Piece::King => &mut piece_set.kings,
            Piece::Queen => &mut piece_set.queens,
            Piece::Rook => &mut piece_set.rooks,
            Piece::Bishop => &mut piece_set.bishops,
            Piece::Knight => &mut piece_set.knights,
            Piece::Pawn => &mut piece_set.pawns,
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

    fn king(&self, color: Color) -> BitBoard {
        self.colored_piece(color, Piece::King)
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

    fn pins_and_checks(&mut self) {
        let enemy_king = self.king(!self.side);
        let king_sq = Square::from_bitboard(enemy_king);
        let own_pieces = self.colored_piece_set(self.side);
        let bishops = own_pieces.bishops;
        let rooks = own_pieces.rooks;
        let queens = own_pieces.queens;
        let knights = own_pieces.knights;
        let pawns = own_pieces.pawns;

        let attacking_bishops = bishop_xrays(king_sq) & bishops;
        let attacking_rooks = rook_xrays(king_sq) & rooks;
        let attacking_queens = queen_xrays(king_sq) & queens;

        let attacking = attacking_bishops | attacking_rooks | attacking_queens;

        // Reset pins and checks
        self.pinned = BitBoard(0);
        self.checking = BitBoard(0);

        // Add sliding piece checkers and pins
        for attacker in attacking {
            let from = Square::from_idx(attacker);
            let to = king_sq;
            let ray = xray_subsection(from, to);
            let blockers = ray & self.all();
            match blockers.0 {
                0 => {
                    self.checking = self.checking | BitBoard::from_idx(attacker);
                }
                1 => {
                    self.pinned = self.pinned | blockers;
                }
                _ => {
                    // None
                }
            }
        }

        // Add non sliding piece checkers
        let attacking_knights =
            knight_bitboard_moves(king_sq, (self.colored(self.side), self.colored(!self.side)))
                & knights;
        let attacking_pawns = pawn_pseudo_attacks(!self.side, king_sq.to_bitboard()) & pawns;

        self.checking = self.checking | attacking_knights | attacking_pawns;
    }

    // Gets the allowed squares to move in a position
    // only relevant when the king is in check,
    // as you need to block the check or capture the checking piece
    fn allowed_squares(&self) -> BitBoard {
        let squares = if self.checking.is_empty() {
            return BitBoard::FULL;
        } else {
            // Should only be one checker, otherwise it is game over
            let checker = Square::from_bitboard(self.checking);
            let king = Square::from_bitboard(self.king(self.side));
            let ray = xray_subsection(checker, king);
            ray | self.checking
        };
        squares
    }

    // Validate the legality of the moves
    // Check that it blocks/removes potential checks
    // and that it does not move pinned pieces in the wrong direction
    fn get_legal_moves(&self, origin: Square, moves: Vec<Move>) -> Vec<Move> {
        let own_king = self.king(self.side);
        let pinned = self.pinned;
        let from_bb = origin.to_bitboard();
        let allowed_squares = self.allowed_squares();
        let is_pinned = from_bb & pinned != BitBoard(0);

        let valid_moves = moves
            .into_iter()
            .filter(|m| {
                let to = m.to();
                let to_bb = to.to_bitboard();
                if to_bb & allowed_squares == BitBoard(0) {
                    return false;
                }
                if !is_pinned {
                    return true;
                }
                let pinned_direction =
                    Direction::from_squares(origin, Square::from_bitboard(own_king));
                let opposite_direction = pinned_direction.opposite();
                let allowed_moves =
                    get_slider_xrays(origin, &[pinned_direction, opposite_direction]);
                return allowed_moves & to_bb != BitBoard(0);
            })
            .collect();
        valid_moves
    }

    fn get_bishop_moves(&self, origin: Square) -> Vec<Move> {
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let moves = bishop_moves(origin, ctx);
        let valid_moves = self.get_legal_moves(origin, moves);
        valid_moves
    }

    fn get_rook_moves(&self, origin: Square) -> Vec<Move> {
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let moves = rook_moves(origin, ctx);
        let valid_moves = self.get_legal_moves(origin, moves);
        valid_moves
    }

    fn get_pawn_moves(&self, origin: Square) -> Vec<Move> {
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let moves = pawn_moves(self.side, origin, ctx, self.en_passant);
        let valid_moves = self.get_legal_moves(origin, moves);
        valid_moves
    }

    fn get_knight_moves(&self, origin: Square) -> Vec<Move> {
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let moves = knight_moves(origin, ctx);
        let valid_moves = self.get_legal_moves(origin, moves);
        valid_moves
    }

    fn get_queen_moves(&self, origin: Square) -> Vec<Move> {
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let moves = {
            let bishop_moves = bishop_moves(origin, ctx);
            let rook_moves = rook_moves(origin, ctx);
            [bishop_moves, rook_moves].concat()
        };
        let valid_moves = self.get_legal_moves(origin, moves);
        valid_moves
    }

    fn is_king_safe(&self, square: Square) -> bool {
        let king = self.king(self.side);
        let own = self.colored(self.side) & !square.to_bitboard();
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let enemy_pieces = self.colored_piece_set(!self.side);
        let enemy_bishops = enemy_pieces.bishops;
        let enemy_rooks = enemy_pieces.rooks;
        let enemy_queens = enemy_pieces.queens;
        let enemy_knights = enemy_pieces.knights;
        let enemy_pawns = enemy_pieces.pawns;
        let enemy_king = self.king(!self.side);

        let bishop_intersects = DIAGONALS
            .iter()
            .map(|&dir| get_slide_direction_bitboard(square, dir, ctx))
            .any(|bb| bb & (enemy_bishops | enemy_queens) != BitBoard(0));

        let rook_intersects = [
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ]
        .iter()
        .map(|&dir| get_slide_direction_bitboard(square, dir, ctx))
        .any(|bb| bb & (enemy_rooks | enemy_queens) != BitBoard(0));

        let knight_intersects = knight_moves(square, ctx)
            .iter()
            .any(|m| m.to().to_bitboard() & enemy_knights != BitBoard(0));

        let pawn_intersects =
            pawn_pseudo_attacks(self.side, square.to_bitboard()) & enemy_pawns != BitBoard(0);

        let king_intersects = king_moves(square, ctx)
            .iter()
            .any(|m| m.to().to_bitboard() & enemy_king != BitBoard(0));

        !(bishop_intersects || rook_intersects || knight_intersects || pawn_intersects)
    }

    fn get_castling(&self) -> Vec<Move> {
        let own_king = self.king(self.side);
        let castling_rights = &self.castling[self.side as usize];
        if !castling_rights.long && !castling_rights.short {
            return vec![];
        }
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);

        let own_king_sq = Square::from_bitboard(own_king);
        let own_king_safe = self.is_king_safe(own_king_sq);
        let own_back_rank = match self.side {
            Color::White => Rank::One,
            Color::Black => Rank::Eight,
        };

        // Check both castle sides
        let castles: &[[File; 2]] = match (castling_rights.long, castling_rights.short) {
            (true, true) => &[[LONG, LONG_KING_TO], [SHORT, SHORT_KING_TO]],
            (true, false) => &[[LONG, LONG_KING_TO]],
            (false, true) => &[[SHORT, SHORT_KING_TO]],
            _ => &[],
        };
        castles
            .iter()
            .filter_map(|&[rook_file, king_to_file]| {
                // Look if the path between the rook and king is empty
                let rook_square = Square::new(own_back_rank, rook_file);
                let ray = xray_subsection(own_king_sq, rook_square);
                let blockers = ray & self.all();
                let is_clear = blockers.is_empty();

                // Look if the king can walk to the destination square without walking through a check
                let king_to_square = Square::new(own_back_rank, king_to_file);
                // King cannot be in check on the destination square
                let ray_to_walk =
                    xray_subsection(own_king_sq, king_to_square) | king_to_square.to_bitboard();
                let is_safe = ray_to_walk
                    .into_iter()
                    .all(|idx| self.is_king_safe(Square::from_idx(idx)));

                if is_clear && is_safe {
                    let m = Move::new(own_king_sq, rook_square, None);
                    return Some(m);
                }
                None
            })
            .collect()
    }

    fn get_king_moves(&self, origin: Square) -> Vec<Move> {
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let moves = king_moves(origin, ctx);
        let valid_moves = moves
            .into_iter()
            .filter(|m| self.is_king_safe(m.to()))
            .collect();
        let castle_moves = self.get_castling();
        [valid_moves, castle_moves].concat()
    }

    /// Make a move on the board
    /// 
    /// # Errors
    /// Returns an error if the move is illegal
    pub fn make_move(&mut self, m: Move) -> Result<GameResult, &'static str> {
        let mut all_moves: HashSet<Move> = HashSet::from_iter(self.all_moves().into_iter());
        if !all_moves.contains(&m) {
            return Err("Illegal move");
        }
        let from = m.from();
        let to = m.to();

        let piece = if let Some(piece) = self.get_piece(from) {
            piece
        } else {
            return Err("No piece at origin square");
        };
        let (color, piece) = piece;
        if color != self.side {
            return Err("Incorrect turn");
        }

        // If the move is a castling move, a rook of the playing side will be "captured"
        let castle = self.colored_piece(self.side, Piece::Rook) & to.to_bitboard() != BitBoard(0);

        let pieces = self.colored_piece_mut(color, piece);

        if !castle {
            *pieces = *pieces & !from.to_bitboard();
            if let Some((opp_color, opp_piece)) = self.get_piece(to) {
                let opp_pieces = self.colored_piece_mut(opp_color, opp_piece);
                *opp_pieces = *opp_pieces & !to.to_bitboard();
                self.half_move_clock = 0;
            }
            let pieces = self.colored_piece_mut(color, piece);
            *pieces = *pieces | to.to_bitboard();
        } else {
            // Get the files and change castling rights depending on which side to castle to
            let (rook_file, king_to_file, rook_to_file) = match to.file {
                LONG => {
                    self.castling[self.side as usize].long = false;
                    (LONG, LONG_KING_TO, LONG_ROOK_TO)
                }
                SHORT => {
                    self.castling[self.side as usize].short = false;
                    (SHORT, SHORT_KING_TO, SHORT_ROOK_TO)
                }
                _ => return Err("Invalid castling move"),
            };

            let rook = self.colored_piece_mut(color, Piece::Rook);
            let rook_to_sq = Square::new(from.rank, rook_to_file);
            *rook = *rook & !to.to_bitboard();
            *rook = *rook | rook_to_sq.to_bitboard();

            let king = self.colored_piece_mut(color, Piece::King);
            let king_to_sq = Square::new(from.rank, king_to_file);
            *king = *king & !from.to_bitboard();
            *king = *king | king_to_sq.to_bitboard();
        }

        self.en_passant = None;
        match piece {
            Piece::King => {
                self.castling[self.side as usize].long = false;
                self.castling[self.side as usize].short = false;
            }
            Piece::Rook => {
                match from.file {
                    LONG => self.castling[self.side as usize].long = false,
                    SHORT => self.castling[self.side as usize].short = false,
                    _ => {}
                }
            }
            Piece::Pawn => {
                self.half_move_clock = 0;
                // Can be cleared since a pawn move is not reversible
                self.seen_positions.clear();

                if let Some(piece) = m.promotion() {
                    let pawns = self.colored_piece_mut(color, Piece::Pawn);
                    *pawns = *pawns & !to.to_bitboard();
                    let promoted = self.colored_piece_mut(color, piece);
                    *promoted = *promoted | to.to_bitboard();
                }

                // Check if the move is a double pawn push
                match (from.rank, to.rank) {
                    (Rank::Two, Rank::Four) => {
                        let en_passant_sq = Square::new(Rank::Three, to.file);
                        self.en_passant = Some(en_passant_sq.to_bitboard());
                    }
                    (Rank::Seven, Rank::Five) => {
                        let en_passant_sq = Square::new(Rank::Six, to.file);
                        self.en_passant = Some(en_passant_sq.to_bitboard());
                    }
                    _ => {}
                }
            }
            _ => (),
        }

        // Update clock
        if self.side == Color::Black {
            self.full_move_number += 1;
        }

        let pos_count = match self.seen_positions.get_mut(&(self.pieces, self.side)) {
            Some(count) => {
                *count += 1;
                *count
            }
            None => {
                self.seen_positions.insert((self.pieces, self.side), 1);
                1
            }
        };


        // Update pins and switch sides
        self.pins_and_checks();
        self.side = !self.side;

        let result = self.get_game_result();
        Ok(result)
    }

    /// Get all the playable moves for the current side
    pub fn all_moves(&self) -> Vec<Move> {
        let piece_set = self.colored_piece_set(self.side);
        let mut moves = vec![];

        let bishop_moves = piece_set.bishops.into_iter().flat_map(|idx| {
            let sq = Square::from_idx(idx);
            self.get_bishop_moves(sq)
        });
        let rook_moves = piece_set.rooks.into_iter().flat_map(|idx| {
            let sq = Square::from_idx(idx);
            self.get_rook_moves(sq)
        });
        let queen_moves = piece_set.queens.into_iter().flat_map(|idx| {
            let sq = Square::from_idx(idx);
            self.get_queen_moves(sq)
        });
        let knight_moves = piece_set.knights.into_iter().flat_map(|idx| {
            let sq = Square::from_idx(idx);
            self.get_knight_moves(sq)
        });
        let pawn_moves = piece_set.pawns.into_iter().flat_map(|idx| {
            let sq = Square::from_idx(idx);
            self.get_pawn_moves(sq)
        });
        let king_moves = piece_set.kings.into_iter().flat_map(|idx| {
            let sq = Square::from_idx(idx);
            self.get_king_moves(sq)
        });

        moves.extend(bishop_moves);
        moves.extend(rook_moves);
        moves.extend(queen_moves);
        moves.extend(knight_moves);
        moves.extend(pawn_moves);
        moves.extend(king_moves);
        moves
    }

    /// Get the result of the game
    /// 
    /// Note: This does not take into account the 3 fold repetition rule
    /// that check is done in the `make_move` function
    pub fn get_game_result(&mut self) -> GameResult {
        let own_king = self.king(self.side);
        let own = self.colored(self.side);
        let enemy = self.colored(!self.side);
        let ctx = (own, enemy);
        let own_king_sq = Square::from_bitboard(own_king);
        let own_king_moves = king_moves(own_king_sq, ctx);
        let own_king_safe = own_king_moves.iter().all(|m| self.is_king_safe(m.to()));

        let own_moves = self.all_moves();
        let own_moves_len = own_moves.len();
        let own_moves_empty = own_moves_len == 0;

        let own_checking = self.checking;
        let own_checking_empty = own_checking.is_empty();

        let pos_count = match self.seen_positions.get(&(self.pieces, self.side)) {
            Some(count) => *count,
            None => 0,
        };

        for m in own_moves {
            println!("{}", m);
        }


        if own_moves_empty {
            if own_checking_empty {
                return GameResult::Stalemate;
            } else {
                return GameResult::Checkmate { winner: !self.side };
            }
        } else if !own_king_safe {
            return GameResult::Checkmate { winner: !self.side };
        } else {
            if self.half_move_clock >= 50 {
                return GameResult::FiftyMoveRule;
            }
            if pos_count >= 3 {
                return GameResult::ThreefoldRepetition;
            }
            return GameResult::InProgress;
        }
    }

    /// Get the moves of a piece at a given square
    pub fn get_moves(&self, from: Square) -> Option<Vec<Move>> {
        if let Some(piece) = self.get_piece(from) {
            let (color, piece) = piece;
            if color != self.side {
                return None;
            }
            let moves = match piece {
                Piece::King => self.get_king_moves(from),
                Piece::Queen => self.get_queen_moves(from),
                Piece::Rook => self.get_rook_moves(from),
                Piece::Bishop => self.get_bishop_moves(from),
                Piece::Knight => self.get_knight_moves(from),
                Piece::Pawn => self.get_pawn_moves(from),
            };
            return Some(moves);
        }
        None
    }

    /// Get the pieces on the board
    pub fn get_all_pieces(&self) -> [Option<(Piece, Color)>; 64] {
        let mut pieces = [None; 64];
        for idx in 0..64 {
            let square = Square::from_idx(idx);
            if let Some((color, piece)) = self.get_piece(square) {
                pieces[idx as usize] = Some((piece, color));
            }
        }
        pieces

    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut board = String::new();
        let mut i = 0;
        for rank in (0..8).rev() {
            for file in 0..8 {
                if i % 2 == 0 {
                    board.push_str("\x1b[42m");
                } else {
                    board.push_str("\x1b[43m");
                }
                i += 1;
                let square = Square::new(Rank::from_idx(rank), File::from_idx(file));
                let piece = self.get_piece(square);
                let piece_str = match piece {
                    Some((Color::White, Piece::King)) => "♔ ",
                    Some((Color::White, Piece::Queen)) => "♕ ",
                    Some((Color::White, Piece::Rook)) => "♖ ",
                    Some((Color::White, Piece::Bishop)) => "♗ ",
                    Some((Color::White, Piece::Knight)) => "♘ ",
                    Some((Color::White, Piece::Pawn)) => "♙ ",
                    Some((Color::Black, Piece::King)) => "♚ ",
                    Some((Color::Black, Piece::Queen)) => "♛ ",
                    Some((Color::Black, Piece::Rook)) => "♜ ",
                    Some((Color::Black, Piece::Bishop)) => "♝ ",
                    Some((Color::Black, Piece::Knight)) => "♞ ",
                    Some((Color::Black, Piece::Pawn)) => "♟ ",
                    None => "  ",
                };
                board.push_str(piece_str);
            }
            i += 1;
            board.push_str("\x1b[0m\n");
        }
        write!(f, "{}", board)
    }
}

struct State {
    board: Board,
}

mod test {
    use crate::internals::BitBoard;

    #[test]
    pub fn test_get_piece() {
        use crate::game::{Board, Color, Piece, Square};
        let board = Board::new();
        let piece = board.get_piece(Square::from_idx(0));
        println!("{:?}", piece);
        assert_eq!(piece, Some((Color::White, Piece::Rook)));
    }

    #[test]
    pub fn test_bishop_moves() {
        use crate::game::{Board, Color, Piece, Square, *};
        let board = Board::new();
        let moves = board.get_bishop_moves(Square::new(Rank::Five, File::C));
        let mut m = BitBoard(0);
        for mv in moves {
            m = m | mv.to().to_bitboard();
        }
        println!("{}", m);
    }

    #[test]
    pub fn test_knight_moves() {
        use crate::game::{Board, Color, Piece, Square, *};
        let board = Board::new();
        let moves = board.get_knight_moves(Square::new(Rank::Five, File::C));
        let mut m = BitBoard(0);
        for mv in moves {
            m = m | mv.to().to_bitboard();
        }
        println!("{}", m);
    }

    #[test]
    pub fn test_all_moves() {
        use crate::game::{Board, Color, Piece, Square, *};
        let board = Board::new();
        println!("{}", board);
        let moves = board.all_moves();
        let mut m = BitBoard(0);
        for mv in moves {
            m = m | mv.to().to_bitboard();
        }
        println!("{}", m);
    }

    #[test]
    pub fn test_fools_mate() {
        use crate::game::{Board, Color, Piece, Square, *};
        let mut board = Board::new();
        let moves = vec![
            Move::new(square!(Two, F), square!(Four, F), None),
            Move::new(square!(Seven, E), square!(Five, E), None),
            Move::new(square!(Two, G), square!(Four, G), None),
            Move::new(square!(Eight, D), square!(Four, H), None),
        ];
        for m in moves {
            let result = board.make_move(m);
            println!("{}", board);
            match result {
                Ok(GameResult::InProgress) => (),
                Err(e) => {
                    assert!(false, "{}", e);
                }
                res => {
                    assert_eq!(
                        res,
                        Ok(GameResult::Checkmate {
                            winner: Color::Black
                        })
                    );
                    return;
                }
            }
        }
        assert!(false, "{}", "Game did not end");
    }

    #[test]
    pub fn test_castling_success() {
        use crate::game::{Board, Color, Piece, Square, *};
        let mut board = Board::new();
        let moves = vec![
            Move::new(square!(Two, E), square!(Four, E), None),
            Move::new(square!(Seven, E), square!(Five, E), None),
            Move::new(square!(Two, F), square!(Three, F), None),
            Move::new(square!(Seven, D), square!(Six, D), None),
            Move::new(square!(One, F), square!(Three, G), None),
            Move::new(square!(Six, D), square!(Five, D), None),
            Move::new(square!(One, G), square!(Two, F), None),
            Move::new(square!(Five, D), square!(Four, D), None),
            Move::new(square!(One, E), square!(One, H), None),
        ];
        for m in moves {
            let result = board.make_move(m);
            match result {
                Err(e) => {
                    assert!(false, "{}", e);
                }
                _ => (),
            }
        }
        let king_pos = Square::from_bitboard(board.colored_piece(Color::White, Piece::King));
        assert_eq!(king_pos, square!(One, G));
    }

    #[test]
    pub fn test_castling_fail() {
        use crate::game::{Board, Color, Piece, Square, *};
        let mut board = Board::new();
        let moves = vec![
            Move::new(square!(Two, E), square!(Four, E), None),
            Move::new(square!(Seven, E), square!(Five, E), None),
            Move::new(square!(Two, F), square!(Three, F), None),
            Move::new(square!(Seven, D), square!(Six, D), None),
            Move::new(square!(One, F), square!(Three, G), None),
            Move::new(square!(Six, D), square!(Five, D), None),
            Move::new(square!(One, G), square!(Two, F), None),
            Move::new(square!(Five, D), square!(Four, D), None),
            Move::new(square!(One, H), square!(One, G), None),
            Move::new(square!(Seven, A), square!(Six, A), None),
            Move::new(square!(One, G), square!(One, H), None),
            Move::new(square!(Six, A), square!(Five, A), None),
            Move::new(square!(One, E), square!(One, H), None),
        ];
        for m in &moves {
            let result = board.make_move(*m);
            println!("{}", board);
            match result {
                Err(e) => {
                    if *m == moves[12] {
                        return;
                    }
                    assert!(false, "{}", e);
                }
                _ => (),
            }
        }
        let king_pos = Square::from_bitboard(board.colored_piece(Color::White, Piece::King));
        assert_eq!(king_pos, square!(One, E));
    }

    #[test]
    pub fn test_stalemate() {
        use crate::game::{Board, Color, Piece, Square, *};
        let mut board = Board::new();
        let moves: &[Move] = &[
            mv!((C, Two), (C, Three)),
            mv!((A, Seven), (A, Five)),
            mv!((D, One), (A, Four)),
            mv!((A, Eight), (A, Six)),
            mv!((A, Four), (A, Five)),
            mv!((H, Seven), (H, Five)),
            mv!((H, Two), (H, Four)),
            mv!((A, Six), (H, Six)),
            mv!((A, Five), (C, Seven)),
            mv!((F, Seven), (F, Six)),
            mv!((C, Seven), (D, Seven)),
            mv!((E, Eight), (F, Seven)),
            mv!((D, Seven), (B, Seven)),
            mv!((D, Eight), (D, Three)),
            mv!((B, Seven), (B, Eight)),
            mv!((D, Three), (H, Seven)),
            mv!((B, Eight), (C, Eight)),
            mv!((F, Seven), (G, Six)),
            mv!((C, Eight), (E, Six)),
        ];
        for m in moves {
            println!("{}", m);
            let result = board.make_move(*m);
            println!("{}", board);
            println!("{}", board.pinned);
            match result {
                Err(e) => {
                    assert!(false, "{}", e);
                }
                Ok(GameResult::Stalemate) => {
                    return;
                }
                _ => (),
            }
        }
        assert!(false, "{}", "Game did not end");
    }
}
