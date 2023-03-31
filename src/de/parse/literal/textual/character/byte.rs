use std::fmt::Display;

use nom::{
    character::complete::satisfy,
    combinator::{map_opt, map_res, opt, value},
    sequence::{delimited, preceded},
    AsChar, IResult, Parser,
};

use nom::character::complete as nchar;

#[cfg(test)]
use proptest_derive::Arbitrary;

#[cfg(test)]
use proptest::{strategy::Strategy, string::string_regex};

use crate::de::parse::suffix;

pub fn ascii_for_char(input: &str) -> IResult<&str, u8> {
    map_res(
        satisfy(|c| c != '\'' && c != '\\' && c != '\n' && c != '\r' && c != '\t' && c.is_ascii()),
        u8::try_from,
    )(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ByteEscape {
    Code(u8),
    NewLine,
    Return,
    Tab,
    Backslash,
    Null,
    QuoteSingle,
    QuoteDouble,
}

impl Display for ByteEscape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\\{}",
            match self {
                ByteEscape::Code(c) => format!(
                    "x{}{}",
                    char::from_digit((*c as u32 & 0xF0) >> 4, 16).unwrap(),
                    char::from_digit(*c as u32 & 0x0F, 16).unwrap()
                ),
                ByteEscape::NewLine => 'n'.to_string(),
                ByteEscape::Return => 'r'.to_string(),
                ByteEscape::Tab => 't'.to_string(),
                ByteEscape::Backslash => '\\'.to_string(),
                ByteEscape::Null => '0'.to_string(),
                ByteEscape::QuoteSingle => '\''.to_string(),
                ByteEscape::QuoteDouble => '"'.to_string(),
            }
        )
    }
}

impl From<ByteEscape> for u8 {
    fn from(b: ByteEscape) -> Self {
        match b {
            ByteEscape::Code(b) => b,
            ByteEscape::NewLine => b'\n',
            ByteEscape::Return => b'\r',
            ByteEscape::Tab => b'\t',
            ByteEscape::Backslash => b'\\',
            ByteEscape::Null => b'\0',
            ByteEscape::QuoteSingle => b'\'',
            ByteEscape::QuoteDouble => b'\"',
        }
    }
}

pub fn byte_escape(input: &str) -> IResult<&str, ByteEscape> {
    preceded(
        nchar::char('\\'),
        (preceded(
            nchar::char('x'),
            map_opt(satisfy(AsChar::is_hex_digit), |c| c.to_digit(16))
                .and(map_opt(satisfy(AsChar::is_hex_digit), |c| c.to_digit(16))),
        )
        .map(|(first, second)| ByteEscape::Code(u8::try_from((first << 4) | second).unwrap())))
        .or(value(ByteEscape::NewLine, nchar::char('n'))
            .or(value(ByteEscape::Return, nchar::char('r')))
            .or(value(ByteEscape::Tab, nchar::char('t')))
            .or(value(ByteEscape::Backslash, nchar::char('\\')))
            .or(value(ByteEscape::Null, nchar::char('0')))
            .or(value(ByteEscape::QuoteSingle, nchar::char('\'')))
            .or(value(ByteEscape::QuoteDouble, nchar::char('"')))),
    )(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ByteLiteralInner {
    #[cfg_attr(
        test,
        proptest(
            strategy = r##"string_regex(r#"[[:ascii:]&&[^'\\\n\r\t]]"#).unwrap().prop_map(|c| ByteLiteralInner::Ascii(u8::try_from(c.chars().next().unwrap()).unwrap()))"##
        )
    )]
    Ascii(u8),
    Escape(ByteEscape),
}

impl Display for ByteLiteralInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"{}"#,
            match self {
                ByteLiteralInner::Ascii(b) => char::from(*b).to_string(),
                ByteLiteralInner::Escape(e) => e.to_string(),
            }
        )
    }
}

impl From<ByteEscape> for ByteLiteralInner {
    fn from(value: ByteEscape) -> Self {
        Self::Escape(value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ByteLiteral<'suffix> {
    inner: ByteLiteralInner,
    suffix: Option<&'suffix str>,
}

pub fn byte_literal(input: &str) -> IResult<&str, ByteLiteral> {
    preceded(
        nchar::char('b'),
        delimited(
            nchar::char('\''),
            ascii_for_char
                .map(ByteLiteralInner::Ascii)
                .or(nom::combinator::into(byte_escape)),
            nchar::char('\''),
        ),
    )
    .and(opt(suffix))
    .map(|(inner, suffix)| ByteLiteral { inner, suffix })
    .parse(input)
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::{strategy::Strategy, string::string_regex};

    use crate::{
        de::parse::{literal::tests::SUFFIX, ByteEscape, ByteLiteral, ByteLiteralInner},
        test_parse_complex,
    };

    test_parse_complex!(ascii_for_char;
        val in (0u8..128u8).prop_filter("skip escaped characters", |c| {
            let c = char::from(*c);
            c != '\'' && c != '\\' && c != '\n' && c != '\r' && c != '\t'
        })
        => &char::from(val).to_string()
        => ""; val
    );

    test_parse_complex!(byte_escape;
        val in proptest::prelude::any::<ByteEscape>()
        => &val.to_string()
        => ""; val
    );

    test_parse_complex!(byte_literal;
        inner in proptest::prelude::any::<ByteLiteralInner>(),
        suffix in string_regex(&format!(r#"({SUFFIX})?"#)).unwrap()
        => &format!(r#"b'{inner}'{suffix}"#)
        => ""; ByteLiteral {
            inner,
            suffix: match suffix.as_str() {
                "" => None,
                _ => Some(suffix.as_str())
            }
        }
    );
}
