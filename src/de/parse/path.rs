use std::fmt::Display;

use nom::{
    bytes::complete::tag,
    combinator::{opt, value},
    multi::many0,
    IResult, Parser,
};
use stonemason_proc::{Unparse, UnparseDisplay};

// #[cfg(test)]
// use proptest_derive::Arbitrary;

use super::{identifier, Identifier, Parsed, Unparse};

mod type_;
pub use type_::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum SimplePathSegment<'data> {
    Id(Identifier<'data>),
    #[unparse("super")]
    Super,
    #[unparse("self")]
    Self_,
    #[unparse("stone")]
    Stone,
    #[unparse("$stone")]
    MacroStone,
}

impl<'data> From<Identifier<'data>> for SimplePathSegment<'data> {
    fn from(value: Identifier<'data>) -> Self {
        Self::Id(value)
    }
}

impl<'data> Parsed<&'data str> for SimplePathSegment<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        simple_path_segment(input)
    }
}

fn simple_path_segment(input: &str) -> IResult<&str, SimplePathSegment> {
    value(SimplePathSegment::Super, tag("super"))
        .or(value(SimplePathSegment::Self_, tag("self")))
        .or(value(SimplePathSegment::Stone, tag("stone")))
        .or(value(SimplePathSegment::MacroStone, tag("$stone")))
        .or(identifier.map(SimplePathSegment::Id))
        .parse(input)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, UnparseDisplay)]
pub struct SimplePath<'data> {
    /// whether it's preceded by a '::'
    qualified: bool,
    segments: Vec<SimplePathSegment<'data>>,
}

impl<'data> Unparse for SimplePath<'data> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.qualified {
            write!(f, "::")?;
        }
        let mut segments = self.segments.iter();
        segments.next().unwrap().fmt(f)?;
        for segment in segments {
            write!(f, "::{segment}")?;
        }
        Ok(())
    }
}

impl<'data> Parsed<&'data str> for SimplePath<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        simple_path(input)
    }
}

fn simple_path(input: &str) -> IResult<&str, SimplePath> {
    opt(tag("::"))
        .and(simple_path_segment)
        .and(many0(tag("::").and(simple_path_segment).map(|(_, s)| s)))
        .map(|((qualified, first), mut segments)| SimplePath {
            qualified: qualified.is_some(),
            segments: {
                segments.insert(0, first);
                segments
            },
        })
        .parse(input)
}
