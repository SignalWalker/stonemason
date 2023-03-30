use nom::{
    bytes::complete::{tag, take_while_m_n},
    character::complete::satisfy,
    combinator::recognize,
    sequence::{delimited, preceded},
    AsChar, IResult, Parser,
};

use nom::character::complete as nchar;

pub fn quote_escape(input: &str) -> IResult<&str, &str> {
    preceded(nchar::char('\\'), tag("'").or(tag("\"")))(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct AsciiEscape(char);

impl From<char> for AsciiEscape {
    fn from(value: char) -> Self {
        Self(value)
    }
}

impl AsciiEscape {
    fn from_code(first: u8, second: u8) -> Option<Self> {
        char::from_u32(((first as u32) << 8) | (second as u32)).map(Self)
    }
}

pub fn ascii_escape(input: &str) -> IResult<&str, &str> {
    preceded(
        nchar::char('\\'),
        (preceded(
            tag(r#"x"#),
            recognize(satisfy(AsChar::is_oct_digit).and(satisfy(AsChar::is_hex_digit))),
        ))
        .or(tag("n"))
        .or(tag("r"))
        .or(tag("t"))
        .or(tag("\\"))
        .or(tag("0")),
    )(input)
}

pub fn unicode_escape(input: &str) -> IResult<&str, &str> {
    preceded(
        tag(r#"\u"#),
        delimited(
            nchar::char('{'),
            take_while_m_n(1, 6, AsChar::is_hex_digit),
            nchar::char('}'),
        ),
    )(input)
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::{
        strategy::{Filter, Strategy},
        string::{string_regex, RegexGeneratorStrategy},
    };

    use crate::{de::parse::identifier::tests::IDENTIFIER_OR_KEYWORD, test_parse};

    test_parse!(quote_escape, r#"\\['"]"#; input => &input[1..]);
    test_parse!(
        ascii_escape,
        r#"(\\x[0-7][[:xdigit:]])|(\\n)|(\\r)|(\\t)|(\\\\)|(\\0)"#; input => match input.get(0..2) {
            Some("\\x") => &input[2..],
            _ => &input[1..]
        }
    );
    test_parse!(unicode_escape, r#"\\u\{[[:xdigit:]]{1,6}\}"#; input => input[3..].split(['{', '}']).next().unwrap());
}
