use nom::{bytes::complete::tag, Parser};
use stonemason_proc::{Unparse, UnparseDisplay};

use crate::de::parse::{
    tag::{ArrowRight, PathSeparator},
    Authorial0, AuthorialDelim, GenericArgs, Identifier, Parsed, Separated, Token, Type, Unparse,
};

use nom::combinator::value;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum PathIdentSegment<'data> {
    Id(Identifier<'data>),
    #[unparse("super")]
    Super,
    #[unparse("self")]
    Self_,
    #[unparse("Self")]
    SelfType,
    #[unparse("stone")]
    Stone,
    #[unparse("$stone")]
    MacroStone,
}

impl<'data> Parsed<&'data str> for PathIdentSegment<'data> {
    fn from_parse(input: &'data str) -> nom::IResult<&'data str, Self> {
        (value(Self::Super, tag("super")))
            .or(value(Self::Self_, tag("self")))
            .or(value(Self::SelfType, tag("Self")))
            .or(value(Self::Stone, tag("stone")))
            .or(value(Self::MacroStone, tag("$stone")))
            .or(Identifier::from_parse.map(Self::Id))
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TypePathFnOutput<'data>(
    Authorial0<'data>,
    ArrowRight,
    Authorial0<'data>,
    Type<'data>,
);

impl<'data> Parsed<&'data str> for TypePathFnOutput<'data> {
    fn from_parse(input: &'data str) -> nom::IResult<&'data str, Self> {
        Authorial0::from_parse
            .and(ArrowRight::from_parse)
            .and(Authorial0::from_parse)
            .and(Type::from_parse)
            .map(|(((ca, arrow), cb), t)| Self(ca, arrow, cb, t))
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TypePathFn<'data> {
    #[unparse(prefix('('))]
    inner_comment_1: Authorial0<'data>,
    inputs: Separated<AuthorialDelim<'data, Type<'data>>, Token<','>>,
    inner_comment_2: Authorial0<'data>,
    #[unparse(prefix(')'))]
    output: Option<TypePathFnOutput<'data>>,
}

impl<'data> Parsed<&'data str> for TypePathFn<'data> {
    fn from_parse(input: &'data str) -> nom::IResult<&'data str, Self> {
        Authorial0::from_parse
            .and(Separated::from_parse)
            .and(Authorial0::from_parse)
            .and(Option::from_parse)
            .map(
                |(((inner_comment_1, inputs), inner_comment_2), output)| Self {
                    inner_comment_1,
                    inputs,
                    inner_comment_2,
                    output,
                },
            )
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum TypePathSegmentInner<'data> {
    Generic(GenericArgs<'data>),
    Fn(TypePathFn<'data>),
}

impl<'data> Parsed<&'data str> for TypePathSegmentInner<'data> {
    fn from_parse(input: &'data str) -> nom::IResult<&'data str, Self> {
        GenericArgs::from_parse
            .map(Self::Generic)
            .or(TypePathFn::from_parse.map(Self::Fn))
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TypePathSegment<'data> {
    id: PathIdentSegment<'data>,
    separator: Option<PathSeparator>,
    inner: Option<TypePathSegmentInner<'data>>,
}

impl<'data> Parsed<&'data str> for TypePathSegment<'data> {
    fn from_parse(input: &'data str) -> nom::IResult<&'data str, Self> {
        PathIdentSegment::from_parse
            .and(Option::from_parse)
            .and(Option::from_parse)
            .map(|((id, separator), inner)| Self {
                id,
                separator,
                inner,
            })
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TypePath<'data> {
    root: Option<(PathSeparator, Authorial0<'data>)>,
    segments: Separated<AuthorialDelim<'data, TypePathSegment<'data>>, PathSeparator, true>,
}

impl<'data> Parsed<&'data str> for TypePath<'data> {
    fn from_parse(input: &'data str) -> nom::IResult<&'data str, Self> {
        Option::from_parse
            .and(Separated::from_parse)
            .map(|(root, segments)| Self { root, segments })
            .parse(input)
    }
}
