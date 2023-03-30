use nom::bytes::complete::{self as bytes, tag};
use nom::character::complete as nchar;
use nom::combinator::{not, peek, success, verify};
use nom::{
    branch::alt,
    bytes::complete::{take_while, take_while1},
    character::complete::satisfy,
    combinator::recognize,
    sequence::pair,
    IResult, Parser,
};
use unicode_ident::{is_xid_continue, is_xid_start};

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

pub fn raw_identifier(input: &str) -> IResult<&str, (&str, &str)> {
    tag("r#").and(identifier_or_keyword).parse(input)
}

pub fn non_keyword_identifier(input: &str) -> IResult<&str, &str> {
    verify(identifier_or_keyword, |res: &str| {
        STRICT_KEYWORDS.binary_search(&res).is_err()
            && RESERVED_KEYWORDS.binary_search(&res).is_err()
    })(input)
}

pub fn identifier(input: &str) -> IResult<&str, (Option<&str>, &str)> {
    raw_identifier
        .map(|(pre, id)| (Some(pre), id))
        .or(non_keyword_identifier.map(|id| (None, id)))
        .parse(input)
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::{strategy::Strategy, string::string_regex};

    use crate::{
        de::parse::{RESERVED_KEYWORDS, STRICT_KEYWORDS},
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
    test_parse!(raw_identifier, string_regex(&RAW_IDENTIFIER).unwrap(); input => ("r#", &input[2..]));
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
        input => {if input.starts_with("r#") { (Some("r#"), &input[2..]) } else { (None, input.as_str()) }}
    );
}
