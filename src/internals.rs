use std::ops;

use crate::game::Square;

macro_rules! impl_op {
    ($trait:ident, $method:ident, $op:tt) => {
        impl ops::$trait for BitBoard {
            type Output = BitBoard;

            fn $method(self, rhs: BitBoard) -> BitBoard {
                BitBoard(self.0 $op rhs.0)
            }
        }
    };
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct BitBoard(pub(crate) u64);

#[macro_export]
macro_rules! bitboard {
    ($a:expr, $b:expr, $c:expr, $d:expr, $e:expr, $f:expr, $g:expr, $h:expr) => {
        BitBoard(
            ($a as u64) << 0
                | ($b as u64) << 8
                | ($c as u64) << 16
                | ($d as u64) << 24
                | ($e as u64) << 32
                | ($f as u64) << 40
                | ($g as u64) << 48
                | ($h as u64) << 56,
        )
    };
}

impl_op!(Add, add, +);
impl_op!(Sub, sub, -);
impl_op!(Mul, mul, *);
impl_op!(Div, div, /);
impl_op!(BitAnd, bitand, &);
impl_op!(BitOr, bitor, |);
impl_op!(BitXor, bitxor, ^);
impl_op!(Shl, shl, <<);
impl_op!(Shr, shr, >>);

impl ops::Not for BitBoard {
    type Output = BitBoard;

    fn not(self) -> BitBoard {
        BitBoard(!self.0 & BitBoard::FULL.0)
    }
}

impl std::fmt::Display for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for i in 0..64 {
            if i % 8 == 0 {
                writeln!(f)?;
            }
            write!(f, "{}", if self.0 & (1 << i) != 0 { "x " } else { ". " })?;
        }
        Ok(())
    }
}

impl std::iter::Iterator for BitBoard {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            return None;
        }
        let idx = self.0.trailing_zeros() as u8;
        self.0 &= self.0 - 1;
        Some(idx)
    }
}

impl BitBoard {
    pub(crate) const FULL: BitBoard = BitBoard(0xFFFFFFFFFFFFFFFF);
    pub(crate) const FILE_A: BitBoard = BitBoard(0x0101010101010101);
    pub(crate) const FILE_B: BitBoard = BitBoard(0x0202020202020202);
    pub(crate) const FILE_C: BitBoard = BitBoard(0x0404040404040404);
    pub(crate) const FILE_D: BitBoard = BitBoard(0x0808080808080808);
    pub(crate) const FILE_E: BitBoard = BitBoard(0x1010101010101010);
    pub(crate) const FILE_F: BitBoard = BitBoard(0x2020202020202020);
    pub(crate) const FILE_G: BitBoard = BitBoard(0x4040404040404040);
    pub(crate) const FILE_H: BitBoard = BitBoard(0x8080808080808080);
    pub(crate) const RANK_1: BitBoard = BitBoard(0x00000000000000FF);
    pub(crate) const RANK_2: BitBoard = BitBoard(0x000000000000FF00);
    pub(crate) const RANK_3: BitBoard = BitBoard(0x0000000000FF0000);
    pub(crate) const RANK_4: BitBoard = BitBoard(0x00000000FF000000);
    pub(crate) const RANK_5: BitBoard = BitBoard(0x000000FF00000000);
    pub(crate) const RANK_6: BitBoard = BitBoard(0x0000FF0000000000);
    pub(crate) const RANK_7: BitBoard = BitBoard(0x00FF000000000000);
    pub(crate) const RANK_8: BitBoard = BitBoard(0xFF00000000000000);

    pub(crate) const fn from_idx(square: u8) -> BitBoard {
        BitBoard(1u64 << square)
    }

    pub(crate) const fn from_square(square: Square) -> BitBoard {
        BitBoard(1 << (square.file.to_idx() + square.rank.to_idx() * 8))
    }

    pub(crate) const fn popcnt(&self) -> u32 {
        self.0.count_ones()
    }

    pub(crate) const fn is_empty(&self) -> bool {
        self.0 == 0
    }
}

mod test {
    use super::*;

    #[test]
    fn test_add() {
        let a = BitBoard(0x0000000000000001);
        let b = BitBoard(0x0000000000000002);
        let c = a + b;
        assert_eq!(c.0, 0x0000000000000003);
    }

    #[test]
    fn test_iter() {
        let a = BitBoard(0b0000000000001001);
        let mut iter = a.into_iter();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), None);
    }
}
