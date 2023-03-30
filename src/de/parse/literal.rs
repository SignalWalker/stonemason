use nom::bytes::complete::tag;
use nom::character::complete as nchar;
use nom::combinator::value;
use nom::sequence::terminated;
use nom::Parser;
use nom::{
    branch::alt,
    character::complete::none_of,
    combinator::{eof, not, peek, recognize},
    sequence::pair,
    IResult,
};

mod numeric;
pub use numeric::*;
mod textual;
pub use textual::*;

pub fn suffix(input: &str) -> IResult<&str, &str> {
    super::identifier_or_keyword(input)
}

pub fn isolated_cr(input: &str) -> IResult<&str, char> {
    terminated(nchar::char('\r'), peek(not(nchar::char('\n'))))(input)
}

pub fn boolean(input: &str) -> IResult<&str, bool> {
    value(true, tag("true"))
        .or(value(false, tag("false")))
        .parse(input)
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::{
        strategy::{Filter, Strategy},
        string::{string_regex, RegexGeneratorStrategy},
    };

    use crate::{de::parse::identifier::tests::IDENTIFIER_OR_KEYWORD, test_parse};

    use std::fmt::Debug;

    const SUFFIX: &str = IDENTIFIER_OR_KEYWORD;

    test_parse!(suffix, SUFFIX);
    test_parse!(isolated_cr, r#"\r[^\n]?"#; input => input.get(1..).unwrap_or(""); '\r');

    test_parse!(boolean, r#"(true|false)"#; input => match input.as_str() {
        "true" => true,
        "false" => false,
        _ => unreachable!()
    });
}
