use super::{ParseError, Parsed};
use crate::de::parse::Unparse;
use nom::{
    character::complete::satisfy,
    combinator::recognize,
    multi::{many0, many1},
    AsChar, IResult, InputIter, InputLength, Offset, Parser, Slice,
};
use std::ops::{RangeFrom, RangeTo};
use stonemason_proc::{Unparse, UnparseDisplay};

pub fn pattern_white_space<
    Item: AsChar,
    Input: InputIter<Item = Item> + Slice<RangeFrom<usize>>,
    Error: ParseError<Input>,
>(
    input: Input,
) -> IResult<Input, char, Error> {
    satisfy(|c| {
        let b = c as u32;
        (0x9 <= b && b <= 0xD) // \t \n `vertical tab` `form feed` \r
            || (c == '\u{20}') // `space`
            || (c == '\u{85}') // `next line`
            || (c == '\u{200E}') // `left-to-right mark`
            || (c == '\u{200F}') // `right-to-left mark`
            || (c == '\u{2028}') // `line separator`
            || (c == '\u{2029}') // `paragraph separator`
    })
    .parse(input)
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct Whitespace<Text, const REQ: bool>(pub Text);
pub type Whitespace0<Text> = Whitespace<Text, false>;
pub type Whitespace1<Text> = Whitespace<Text, true>;

impl<Text, const REQ: bool> std::fmt::Debug for Whitespace<Text, REQ> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "°")
    }
}

impl<
        Item: AsChar,
        Text: Clone
            + InputIter<Item = Item>
            + Slice<RangeFrom<usize>>
            + InputLength
            + Slice<RangeTo<usize>>
            + Offset,
        const REQ: bool,
    > Parsed<Text> for Whitespace<Text, REQ>
{
    fn from_parse<Error: ParseError<Text>>(input: Text) -> nom::IResult<Text, Self, Error> {
        match REQ {
            true => nom::error::context(
                "Whitespace1",
                recognize(many1(pattern_white_space)).map(Self),
            )
            .parse(input),
            false => nom::error::context(
                "Whitespace0",
                recognize(many0(pattern_white_space)).map(Self),
            )
            .parse(input),
        }
    }
}
