use std::ops::RangeFrom;

use super::Unparse;
use nom::bytes::complete::tag;
use nom::character::complete as nchar;
use nom::combinator::value;
use nom::sequence::terminated;
use nom::{
    combinator::{not, peek},
    IResult,
};
use nom::{AsChar, Compare, InputIter, InputTake, Parser, Slice};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

mod numeric;
pub use numeric::*;
mod textual;
pub use textual::*;

use super::{ParseError, Parsed};

pub fn suffix<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, &'data str, Error> {
    super::IdentifierOrKeyword::from_parse
        .map(|id| id.0)
        .parse(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Unparse, UnparseDisplay)]
#[unparse('\r')]
pub struct IsolatedCr;

impl From<IsolatedCr> for char {
    fn from(_: IsolatedCr) -> Self {
        '\r'
    }
}

impl<Item: AsChar, Input: Clone + Slice<RangeFrom<usize>> + InputIter<Item = Item>> Parsed<Input>
    for IsolatedCr
{
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        value(
            Self,
            terminated(nchar::char('\r'), peek(not(nchar::char('\n')))),
        )(input)
    }
}

impl<Input: Clone + InputTake + Compare<&'static str>> Parsed<Input> for bool {
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        nom::error::context(
            "bool",
            value(true, tag::<&'static str, Input, Error>("true"))
                .or(value(false, tag::<&'static str, Input, Error>("false"))),
        )
        .parse(input)
    }
}

impl super::Unparse for bool {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            true => write!(f, "true"),
            false => write!(f, "false"),
        }
    }
}

// #[derive(Clone, Copy, PartialEq, Eq, Hash, Unparse, UnparseDisplay, Parsed)]
// pub struct Boolean(pub bool);

#[cfg(test)]
pub(crate) mod tests {

    use crate::{de::parse::identifier::tests::IDENTIFIER_OR_KEYWORD, test_parse};

    pub const SUFFIX: &str = IDENTIFIER_OR_KEYWORD;

    test_parse!(suffix, SUFFIX);
    test_parse!(isolated_cr, r#"\r[^\n]?"#; input => input.get(1..).unwrap_or(""); '\r');

    test_parse!(boolean, r#"(true|false)"#; input => match input.as_str() {
        "true" => true,
        "false" => false,
        _ => unreachable!()
    });
}
