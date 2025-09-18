use core::iter::once;

use alloc::{boxed::Box, collections::btree_map::BTreeMap, vec::Vec};

use crate::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BfAst {
    Instr(BfInstr),
    Loop(Vec<BfAst>),
}
impl BfAst {
    pub fn parse(a: &mut (dyn Iterator<Item = BfToken> + '_)) -> Option<Self> {
        match a.next()? {
            BfToken::Instr(i) => return Some(Self::Instr(i)),
            BfToken::LoopBegin => {
                return Some(Self::Loop(core::iter::from_fn(|| Self::parse(a)).collect()));
            }
            BfToken::LoopEnd => return None,
        }
    }
    pub fn tokens<'a>(&'a self) -> Box<dyn Iterator<Item = BfToken> + 'a> {
        Box::new(match self {
            BfAst::Instr(bf_instr) => Either::Left([BfToken::Instr(*bf_instr)].into_iter()),
            BfAst::Loop(bf_asts) => Either::Right(
                [BfToken::LoopBegin]
                    .into_iter()
                    .chain(bf_asts.iter().flat_map(|a| a.tokens()))
                    .chain([BfToken::LoopEnd]),
            ),
        })
    }
}
impl Display for BfAst {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            BfAst::Instr(bf_instr) => write!(f, "{bf_instr}"),
            BfAst::Loop(bf_asts) => {
                write!(f, "[")?;
                for a in bf_asts {
                    write!(f, "{a}")?;
                }
                write!(f, "]")?;
                Ok(())
            }
        }
    }
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BfTree<T> {
    pub begin: u32,
    pub blocks: BTreeMap<u32, BfBlock<T>>,
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BfBlock<T> {
    pub instrs: T,
    pub term: BfTerm,
}
impl<T> BfBlock<T> {
    pub fn as_ref<'a>(&'a self) -> BfBlock<&'a T> {
        BfBlock {
            instrs: &self.instrs,
            term: self.term.clone(),
        }
    }
    pub fn as_mut<'a>(&'a mut self) -> BfBlock<&'a mut T> {
        BfBlock {
            instrs: &mut self.instrs,
            term: self.term.clone(),
        }
    }
    pub fn map<E, U>(self, f: &mut (dyn FnMut(T) -> Result<U, E> + '_)) -> Result<BfBlock<U>, E> {
        Ok(BfBlock {
            instrs: f(self.instrs)?,
            term: self.term,
        })
    }
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BfTerm {
    Static(u32),
    Dynamic,
}
impl<T> BfTree<T> {
    pub fn as_ref<'a>(&'a self) -> BfTree<&'a T> {
        BfTree {
            begin: self.begin,
            blocks: self.blocks.iter().map(|(a, b)| (*a, b.as_ref())).collect(),
        }
    }
    pub fn as_mut<'a>(&'a mut self) -> BfTree<&'a mut T> {
        BfTree {
            begin: self.begin,
            blocks: self
                .blocks
                .iter_mut()
                .map(|(a, b)| (*a, b.as_mut()))
                .collect(),
        }
    }
    pub fn map<E, U>(self, f: &mut (dyn FnMut(T) -> Result<U, E> + '_)) -> Result<BfTree<U>, E> {
        Ok(BfTree {
            begin: self.begin,
            blocks: self
                .blocks
                .into_iter()
                .map(|(a, b)| b.map(f).map(|c| (a, c)))
                .collect::<Result<_, E>>()?,
        })
    }
    pub fn to_cff(
        &self,
        iterate: &mut (
                 dyn for<'a> FnMut(&'a T, u32) -> Box<dyn Iterator<Item = BfToken> + 'a> + '_
             ),
    ) -> impl Iterator<Item = BfToken> {
        once(BfToken::Instr(BfInstr::Plus))
            .cycle()
            .take(self.begin as usize)
            .chain([BfToken::LoopBegin])
            .chain(self.blocks.iter().flat_map(move |(b, c)| {
                once(BfToken::Instr(BfInstr::Minus))
                    .cycle()
                    .take(*b as usize)
                    .chain([BfToken::LoopBegin, BfToken::Instr(BfInstr::Next)])
                    .chain(iterate(&c.instrs, *b).flat_map(|a| match a {
                        BfToken::Instr(BfInstr::Next) => {
                            Either::Left([BfToken::Instr(BfInstr::Next); 2].into_iter())
                        }
                        BfToken::Instr(BfInstr::Prev) => {
                            Either::Left([BfToken::Instr(BfInstr::Prev); 2].into_iter())
                        }
                        a => Either::Right([a].into_iter()),
                    }))
                    .chain(match c.term {
                        BfTerm::Dynamic => Either::Left(BfToken::iter_str("[<+>-]<")),
                        BfTerm::Static(a) => {
                            Either::Right([BfToken::Instr(BfInstr::Next)].into_iter().chain(
                                once(BfToken::Instr(BfInstr::Plus)).cycle().take(a as usize),
                            ))
                        }
                    })
                    .chain(
                        once(BfToken::Instr(BfInstr::Minus))
                            .cycle()
                            .take(*b as usize),
                    )
                    .chain([BfToken::LoopEnd])
                    .chain(
                        once(BfToken::Instr(BfInstr::Plus))
                            .cycle()
                            .take(*b as usize),
                    )
            }))
            .chain([BfToken::LoopEnd])
    }
}
