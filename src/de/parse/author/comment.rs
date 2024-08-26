use std::ops::{RangeFrom, RangeTo};

use super::{IsolatedCr, ParseError, Parsed};
use crate::de::parse::Unparse;
use nom::{
    bytes::{complete::tag, complete::take_while},
    character::{
        complete::{anychar, satisfy},
        streaming::none_of,
    },
    combinator::{not, opt, recognize, value},
    multi::many0,
    sequence::{delimited, preceded},
    AsChar, Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset,
    Parser, Slice,
};
use stonemason_proc::{Unparse, UnparseDisplay};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(prefix("//"))]
pub struct LineComment<Text>(pub Text);
impl<Input: Clone + InputTake + Compare<&'static str> + InputTakeAtPosition<Item = char>>
    Parsed<Input> for LineComment<Input>
{
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        preceded(tag("//"), take_while(|c| c != '\n'))
            .map(Self)
            .parse(input)
    }
}

pub trait LineDocTextInput:
    Offset + Clone + Slice<RangeTo<usize>> + Slice<RangeFrom<usize>> + InputLength
{
}

impl<Input: Offset + Clone + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + InputLength>
    LineDocTextInput for Input
{
}

fn line_doc_text_no_isolated_cr<
    Input: LineDocTextInput + InputIter<Item = impl AsChar>,
    Error: ParseError<Input>,
>(
    input: Input,
) -> IResult<Input, Input, Error> {
    recognize::<Input, _, Error, _>(many0(not_char('\n').and(not(IsolatedCr::from_parse))))
        .parse(input)
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(prefix("//!"))]
pub struct LineDocInner<Text>(pub Text);
impl<
        Input: LineDocTextInput + InputIter<Item = impl AsChar> + Compare<&'static str> + InputTake,
    > Parsed<Input> for LineDocInner<Input>
{
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        preceded(tag("//!"), line_doc_text_no_isolated_cr)
            .map(Self)
            .parse(input)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(prefix("///"))]
pub struct LineDocOuter<Text>(pub Text);
impl<
        Input: LineDocTextInput + InputIter<Item = impl AsChar> + Compare<&'static str> + InputTake,
    > Parsed<Input> for LineDocOuter<Input>
{
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        preceded(
            tag("///"),
            recognize(opt(not_char('/').and(line_doc_text_no_isolated_cr))),
        )
        .map(Self)
        .parse(input)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(delim("/*", "*/"))]
pub struct BlockComment<Text>(pub Text);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(delim("/*!", "*/"))]
pub struct BlockDocInner<Text>(pub Text);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(delim("/**", "*/"))]
pub struct BlockDocOuter<Text>(pub Text);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, Parsed)]
pub enum Comment<Text>
where
    LineComment<Text>: Parsed<Text>,
    LineDocInner<Text>: Parsed<Text>,
    LineDocOuter<Text>: Parsed<Text>,
    BlockComment<Text>: Parsed<Text>,
    BlockDocInner<Text>: Parsed<Text>,
    BlockDocOuter<Text>: Parsed<Text>,
{
    Line(LineComment<Text>),
    LineDocInner(LineDocInner<Text>),
    LineDocOuter(LineDocOuter<Text>),
    Block(BlockComment<Text>),
    BlockDocInner(BlockDocInner<Text>),
    BlockDocOuter(BlockDocOuter<Text>),
}

impl<Text> std::fmt::Debug for Comment<Text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Line(_) => write!(f, "//..."),
            Self::LineDocInner(_) => write!(f, "//!..."),
            Self::LineDocOuter(_) => write!(f, "///..."),
            Self::Block(_) => write!(f, "/*...*/"),
            Self::BlockDocInner(_) => write!(f, "/*!...*/"),
            Self::BlockDocOuter(_) => write!(f, "/**...*/"),
        }
    }
}

fn not_char<
    Item: AsChar,
    Input: InputIter<Item = Item> + Slice<RangeFrom<usize>>,
    Error: ParseError<Input>,
>(
    c: char,
) -> impl Parser<Input, char, Error> {
    satisfy(move |v| v != c)
}

pub fn outer_line_doc<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, Comment<&'data str>, Error> {
    preceded(
        tag("///"),
        recognize(opt(not_char('/').and(line_doc_text_no_isolated_cr))),
    )
    .map(Comment::LineDocOuter)
    .parse(input)
}

fn block_doc_text_no_isolated_cr<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, ((), char), Error> {
    not(tag("*/").or(recognize(IsolatedCr::from_parse)))
        .and(anychar)
        .parse(input)
}

pub fn inner_block_doc<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, Comment<&'data str>, Error> {
    delimited(
        tag("/*!"),
        recognize(block_comment_or_doc).or(recognize(many0(block_doc_text_no_isolated_cr))),
        tag("*/"),
    )
    .map(Comment::BlockDocInner)
    .parse(input)
}

pub fn outer_block_doc<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, Comment<&'data str>, Error> {
    delimited(
        tag("/**"),
        recognize(
            (recognize(not_char('*')).or(recognize(block_comment_or_doc))).and(recognize(many0(
                recognize(block_comment_or_doc).or(recognize(block_doc_text_no_isolated_cr)),
            ))),
        ),
        tag("*/"),
    )
    .map(Comment::BlockDocOuter)
    .parse(input)
}

pub fn block_comment<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, Comment<&'data str>, Error> {
    delimited(
        tag("/*"),
        recognize(
            recognize(none_of("*!"))
                .or(tag("**"))
                .or(recognize(block_comment_or_doc))
                .and(recognize(many0(
                    recognize(block_comment_or_doc).or(recognize(not(tag("*/")).and(anychar))),
                ))),
        ),
        tag("*/"),
    )
    .map(Comment::Block)
    .or(value(Comment::Block(""), tag("/**/")))
    .or(value(Comment::Block("*"), tag("/***/")))
    .parse(input)
}

pub fn block_comment_or_doc<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, Comment<&'data str>, Error> {
    block_comment
        .or(outer_block_doc)
        .or(inner_block_doc)
        .parse(input)
}

impl<Input> Parsed<Input> for Comment<Input> {
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        nom::error::context(
            "Comment",
            inner_line_doc
                .or(outer_line_doc)
                .or(line_comment)
                .or(inner_block_doc)
                .or(outer_block_doc)
                .or(block_comment),
        )
        .parse(input)
    }
}
