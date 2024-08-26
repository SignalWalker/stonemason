use super::{
    tag::{KwAs, PathSeparator},
    Authorial0, AuthorialDelim, GenericArgs, Identifier, Many0, Many1, ParseError, Parsed,
    Separated1, Token, Type, Unparse,
};
use nom::{bytes::complete::tag, combinator::opt, multi::many0, IResult, Parser};
use std::fmt::Display;
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

// #[cfg(test)]
// use proptest_derive::Arbitrary;

mod type_;
pub use type_::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, Parsed)]
pub enum SimplePathSegment<'data> {
    #[unparse("super")]
    #[parsed("super")]
    Super,
    #[unparse("self")]
    #[parsed("self")]
    Self_,
    #[unparse("stone")]
    #[parsed("stone")]
    Stone,
    #[unparse("$stone")]
    #[parsed("$stone")]
    MacroStone,
    Id(Identifier<'data>),
}

impl<'data> From<Identifier<'data>> for SimplePathSegment<'data> {
    fn from(value: Identifier<'data>) -> Self {
        Self::Id(value)
    }
}

fn simple_path_segment(input: &str) -> IResult<&str, SimplePathSegment> {
    SimplePathSegment::from_parse(input)
}

#[derive(Clone, Eq, PartialEq, Hash, UnparseDisplay)]
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

impl std::fmt::Debug for SimplePath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.unparse(f)
    }
}

impl<'data> Parsed<&'data str> for SimplePath<'data> {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> nom::IResult<&'data str, Self, Error> {
        opt(tag("::"))
            .and(SimplePathSegment::from_parse)
            .and(many0(
                tag("::").and(SimplePathSegment::from_parse).map(|(_, s)| s),
            ))
            .map(|((qualified, first), mut segments)| SimplePath {
                qualified: qualified.is_some(),
                segments: {
                    segments.insert(0, first);
                    segments
                },
            })
            .parse(input)
    }
}

fn simple_path(input: &str) -> IResult<&str, SimplePath> {
    SimplePath::from_parse(input)
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct PathExprSegment<'data> {
    id: PathIdentSegment<'data>,
    generics: Option<(AuthorialDelim<'data, PathSeparator>, GenericArgs<'data>)>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct PathInExpression<'data> {
    qualifier: Option<PathSeparator>,
    first: PathExprSegment<'data>,
    segments: Many0<(PathSeparator, PathExprSegment<'data>)>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct QualifiedPathType<'data> {
    _open: (Token<'<'>, Authorial0<'data>),
    ty: Type<'data>,
    path: Option<(AuthorialDelim<'data, KwAs>, TypePath<'data>)>,
    _close: (Authorial0<'data>, Token<'>'>),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct QualifiedPathInExpression<'data> {
    ty: QualifiedPathType<'data>,
    segments: Many1<(PathSeparator, PathExprSegment<'data>)>,
}
