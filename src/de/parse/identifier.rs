use std::ops::{RangeFrom, RangeTo};

use nom::bytes::complete::tag;
use nom::character::complete as nchar;
use nom::combinator::verify;
use nom::{
    bytes::complete::{take_while, take_while1},
    character::complete::satisfy,
    combinator::recognize,
    sequence::pair,
    IResult, Parser,
};
use nom::{AsChar, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, Slice};
use stonemason_proc::{Parsed, Unparse, UnparseDebug, UnparseDisplay};
use unicode_ident::{is_xid_continue, is_xid_start};

use super::tag::RawTag;
use super::{ParseError, Parsed, Unparse};

lazy_static::lazy_static! {
    pub static ref STRICT_KEYWORDS: Box<[&'static str]> = {
        let mut res = Box::new(["mod"]);
        res.sort_unstable();
        res
    };
    pub static ref RESERVED_KEYWORDS: Box<[&'static str]> = {
        let mut res = Box::new(["macro"]);
        res.sort_unstable();
        res
    };
}

#[derive(Copy, Clone, Unparse, UnparseDisplay, PartialEq, Eq, Hash, UnparseDebug)]
pub struct IdentifierOrKeyword<Text>(pub Text);
impl<
        Item: AsChar,
        Input: Slice<RangeTo<usize>>
            + Slice<RangeFrom<usize>>
            + Offset
            + Clone
            + InputTake
            + InputLength
            + InputIter<Item = Item>
            + InputTakeAtPosition<Item = char>,
    > Parsed<Input> for IdentifierOrKeyword<Input>
{
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        recognize(
            pair(satisfy(is_xid_start), take_while(is_xid_continue))
                .or(nchar::char('_').and(take_while1(is_xid_continue))),
        )
        .map(Self)
        .parse(input)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, UnparseDebug, Parsed)]
#[unparse(prefix("r#"))]
#[parsed(prefix("r#"))]
pub struct RawIdentifier<'data>(pub IdentifierOrKeyword<&'data str>);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, UnparseDebug)]
pub struct NonKeywordIdentifier<Text>(pub IdentifierOrKeyword<Text>);

impl<Input: Clone + AsRef<str>> Parsed<Input> for NonKeywordIdentifier<Input>
where
    IdentifierOrKeyword<Input>: Parsed<Input>,
{
    fn from_parse<Error: ParseError<Input>>(input: Input) -> IResult<Input, Self, Error> {
        verify(
            <IdentifierOrKeyword<Input> as Parsed<Input>>::from_parse::<Error>,
            |res| {
                let res = res.0.as_ref();
                STRICT_KEYWORDS.binary_search(&res).is_err()
                    && RESERVED_KEYWORDS.binary_search(&res).is_err()
            },
        )
        .map(Self)
        .parse(input)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, Parsed)]
pub enum Identifier<'data> {
    Raw(RawIdentifier<'data>),
    Id(NonKeywordIdentifier<&'data str>),
}

impl std::fmt::Debug for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Raw(raw) => std::fmt::Debug::fmt(raw, f),
            Self::Id(id) => std::fmt::Display::fmt(id, f),
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::{strategy::Strategy, string::string_regex};

    use crate::{
        de::parse::{
            Identifier, IdentifierOrKeyword, Parsed, RawIdentifier, RESERVED_KEYWORDS,
            STRICT_KEYWORDS,
        },
        test_parse, test_parse_complex,
    };

    pub const IDENTIFIER_OR_KEYWORD: &str =
        r#"(\p{XID_Start}\p{XID_Continue}*)|(_\p{XID_Continue}+)"#;
    lazy_static::lazy_static! {
        pub(crate) static ref RAW_IDENTIFIER: String = format!("r#({IDENTIFIER_OR_KEYWORD})");
        pub(crate) static ref NON_KEYWORD_IDENTIFIER: String = format!("({IDENTIFIER_OR_KEYWORD})");
        pub(crate) static ref IDENTIFIER: String = format!("({})|({})", *NON_KEYWORD_IDENTIFIER, *RAW_IDENTIFIER);
    }

    test_parse_complex!(identifier_or_keyword, IdentifierOrKeyword::from_parse::<nom::error::VerboseError<&str>>;
        id in string_regex(&IDENTIFIER_OR_KEYWORD).unwrap()
        => id.as_str()
        => ""; IdentifierOrKeyword(id.as_str())
    );
    test_parse!(raw_identifier, string_regex(&RAW_IDENTIFIER).unwrap(); input => RawIdentifier::new(&input[2..]));
    test_parse!(
        non_keyword_identifier,
        string_regex(&NON_KEYWORD_IDENTIFIER).unwrap().prop_filter(
            "non-keyword identifiers must not be strict or reserved keywords",
            |i| {
                STRICT_KEYWORDS.binary_search(&i.as_str()).is_err()
                    && RESERVED_KEYWORDS.binary_search(&i.as_str()).is_err()
            }
        )
    );
    test_parse_complex!(identifier, Identifier::from_parse::<nom::error::VerboseError<&str>>;
        id in string_regex(&IDENTIFIER).unwrap().prop_filter(
            "non-keyword identifiers must not be strict or reserved keywords",
            |i| {
                STRICT_KEYWORDS.binary_search(&i.as_str()).is_err()
                    && RESERVED_KEYWORDS.binary_search(&i.as_str()).is_err()
            }
        ) => id.as_str()
        => "";
        {if id.starts_with("r#") { Identifier::Raw(RawIdentifier::new(&id[2..])) } else { Identifier::Id(id.as_str()) }}
    );
}
