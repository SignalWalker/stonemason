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
use stonemason_proc::{Unparse, UnparseDisplay};
use unicode_ident::{is_xid_continue, is_xid_start};

use super::{Parsed, Unparse};

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

pub fn identifier_or_keyword(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(satisfy(is_xid_start), take_while(is_xid_continue))
            .or(nchar::char('_').and(take_while1(is_xid_continue))),
    )(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(prefix("r#"))]
pub struct RawIdentifier<'data>(&'data str);

impl<'data> RawIdentifier<'data> {
    fn new(id: &'data str) -> Self {
        Self(id)
    }
}

pub fn raw_identifier(input: &str) -> IResult<&str, RawIdentifier> {
    tag("r#")
        .and(identifier_or_keyword)
        .map(|(_, id)| RawIdentifier(id))
        .parse(input)
}

impl<'data> Parsed<&'data str> for RawIdentifier<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        raw_identifier(input)
    }
}

pub fn non_keyword_identifier(input: &str) -> IResult<&str, &str> {
    verify(identifier_or_keyword, |res: &str| {
        STRICT_KEYWORDS.binary_search(&res).is_err()
            && RESERVED_KEYWORDS.binary_search(&res).is_err()
    })(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum Identifier<'data> {
    Raw(RawIdentifier<'data>),
    Id(&'data str),
}

pub fn identifier(input: &str) -> IResult<&str, Identifier> {
    raw_identifier
        .map(Identifier::Raw)
        .or(non_keyword_identifier.map(Identifier::Id))
        .parse(input)
}

impl<'data> Parsed<&'data str> for Identifier<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        identifier(input)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::{strategy::Strategy, string::string_regex};

    use crate::{
        de::parse::{Identifier, RawIdentifier, RESERVED_KEYWORDS, STRICT_KEYWORDS},
        test_parse,
    };

    pub const IDENTIFIER_OR_KEYWORD: &str =
        r#"(\p{XID_Start}\p{XID_Continue}*)|(_\p{XID_Continue}+)"#;
    lazy_static::lazy_static! {
        pub(crate) static ref RAW_IDENTIFIER: String = format!("r#({IDENTIFIER_OR_KEYWORD})");
        pub(crate) static ref NON_KEYWORD_IDENTIFIER: String = format!("({IDENTIFIER_OR_KEYWORD})");
        pub(crate) static ref IDENTIFIER: String = format!("({})|({})", *NON_KEYWORD_IDENTIFIER, *RAW_IDENTIFIER);
    }

    test_parse!(identifier_or_keyword, IDENTIFIER_OR_KEYWORD);
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
    test_parse!(
        identifier,
        string_regex(&IDENTIFIER).unwrap().prop_filter(
            "non-keyword identifiers must not be strict or reserved keywords",
            |i| {
                STRICT_KEYWORDS.binary_search(&i.as_str()).is_err()
                    && RESERVED_KEYWORDS.binary_search(&i.as_str()).is_err()
            }
        );
        input => {if input.starts_with("r#") { Identifier::Raw(RawIdentifier::new(&input[2..])) } else { Identifier::Id(input.as_str()) }}
    );
}
