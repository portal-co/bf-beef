#![no_std]
#[cfg(feature = "alloc")]
#[doc(hidden)]
pub extern crate alloc;

#[doc(hidden)]
pub use core;

#[cfg(feature = "alloc")]
mod heap;
use core::fmt::Display;

#[cfg(feature = "alloc")]
pub use heap::*;

use either::Either;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BfInstr {
    Plus,
    Minus,
    Next,
    Prev,
    In,
    Out,
}
impl BfInstr {
    pub fn from_char(a: char) -> Option<Self> {
        Some(match a {
            '+' => Self::Plus,
            '-' => Self::Minus,
            '>' => Self::Next,
            '<' => Self::Prev,
            ',' => Self::In,
            '.' => Self::Out,
            _ => return None,
        })
    }
    pub fn no_copy(self) -> impl Iterator<Item = BfToken> {
        match self {
            BfInstr::Plus => BfToken::iter_str(">+<+[>-]>[->>+<]<<"),
            BfInstr::Minus => BfToken::iter_str(">+<[>-]>[->>-<]<<-"),
            BfInstr::Next => BfToken::iter_str(">>>>"),
            BfInstr::Prev => BfToken::iter_str("<<<<"),
            BfInstr::In => BfToken::iter_str(">>>[-]<<<[-],"),
            BfInstr::Out => BfToken::iter_str("."),
        }
    }
}
impl Display for BfInstr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            BfInstr::Plus => write!(f, "+"),
            BfInstr::Minus => write!(f, "-"),
            BfInstr::Next => write!(f, ">"),
            BfInstr::Prev => write!(f, "<"),
            BfInstr::In => write!(f, ","),
            BfInstr::Out => write!(f, "."),
        }
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BfToken {
    Instr(BfInstr),
    LoopBegin,
    LoopEnd,
}
impl BfToken {
    pub fn from_char(a: char) -> Option<Self> {
        Some(match a {
            '[' => Self::LoopBegin,
            ']' => Self::LoopEnd,
            a => Self::Instr(BfInstr::from_char(a)?),
        })
    }
    pub fn iter_str(a: &str) -> impl Iterator<Item = Self> {
        a.chars().filter_map(|a| Self::from_char(a))
    }
    pub fn no_copy(self) -> impl Iterator<Item = BfToken> {
        match self {
            BfToken::Instr(i) => Either::Left(i.no_copy()),
            BfToken::LoopBegin => {
                Either::Right(BfToken::iter_str(">+<[>-]>[->+>[<-]<[<]>[-<+>]]<-[+<"))
            }
            BfToken::LoopEnd => {
                Either::Right(BfToken::iter_str(">+<[>-]>[->+>[<-]<[<]>[-<+>]]<-]<"))
            }
        }
    }
    pub fn ncopy_loop(a: usize) -> impl Iterator<Item = BfToken> {
        BfToken::iter_str("[-")
            .chain([BfToken::Instr(BfInstr::Next)].into_iter().cycle().take(a))
            .chain(BfToken::iter_str("+"))
            .chain([BfToken::Instr(BfInstr::Prev)].into_iter().cycle().take(a))
            .chain(BfToken::iter_str("]"))
    }
}
impl Display for BfToken {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            BfToken::Instr(bf_instr) => write!(f, "{bf_instr}"),
            BfToken::LoopBegin => write!(f, "["),
            BfToken::LoopEnd => write!(f, "]"),
        }
    }
}
