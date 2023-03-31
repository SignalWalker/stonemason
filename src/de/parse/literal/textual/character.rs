use std::fmt::Display;

use nom::{
    bytes::complete::{tag, take_while_m_n},
    character::complete::{none_of, satisfy},
    combinator::{map_opt, opt, value},
    sequence::{delimited, preceded},
    AsChar, IResult, Parser,
};

use nom::character::complete as nchar;

#[cfg(test)]
use proptest_derive::Arbitrary;

#[cfg(test)]
use proptest::{strategy::Strategy, string::string_regex};

use crate::de::parse::suffix;

mod byte;
pub use byte::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum QuoteEscape {
    Single,
    Double,
}

impl Display for QuoteEscape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Single => write!(f, "\\'"),
            Self::Double => write!(f, "\\\""),
        }
    }
}

pub fn quote_escape(input: &str) -> IResult<&str, QuoteEscape> {
    preceded(
        nchar::char('\\'),
        value(QuoteEscape::Single, nchar::char('\''))
            .or(value(QuoteEscape::Double, nchar::char('"'))),
    )(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum AsciiEscape {
    #[cfg_attr(test, proptest(strategy = "(0u8..128u8).prop_map(AsciiEscape::Code)"))]
    Code(u8),
    NewLine,
    Return,
    Tab,
    Backslash,
    Null,
}

impl Display for AsciiEscape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\\")?;
        match self {
            Self::Code(c) => write!(
                f,
                "x{}{}",
                char::from_digit((*c as u32 & 0x000000F0) >> 4, 16).unwrap(),
                char::from_digit(*c as u32 & 0x0000000F, 16).unwrap()
            ),
            Self::NewLine => 'n'.fmt(f),
            Self::Return => 'r'.fmt(f),
            Self::Tab => 't'.fmt(f),
            Self::Backslash => '\\'.fmt(f),
            Self::Null => '0'.fmt(f),
        }
    }
}

impl From<AsciiEscape> for char {
    #[inline]
    fn from(e: AsciiEscape) -> Self {
        match e {
            AsciiEscape::Code(c) => c.into(),
            AsciiEscape::NewLine => '\n',
            AsciiEscape::Return => '\r',
            AsciiEscape::Tab => '\t',
            AsciiEscape::Backslash => '\\',
            AsciiEscape::Null => '\0',
        }
    }
}

pub fn ascii_escape(input: &str) -> IResult<&str, AsciiEscape> {
    preceded(
        nchar::char('\\'),
        (preceded(
            nchar::char('x'),
            map_opt(satisfy(AsChar::is_oct_digit), |c| c.to_digit(16))
                .and(map_opt(satisfy(AsChar::is_hex_digit), |c| c.to_digit(16))),
        )
        .map(|(first, second)| AsciiEscape::Code(u8::try_from((first << 4) | second).unwrap())))
        .or(value(AsciiEscape::NewLine, nchar::char('n'))
            .or(value(AsciiEscape::Return, nchar::char('r')))
            .or(value(AsciiEscape::Tab, nchar::char('t')))
            .or(value(AsciiEscape::Backslash, nchar::char('\\')))
            .or(value(AsciiEscape::Null, nchar::char('0')))),
    )(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct UnicodeEscape(char);

impl Display for UnicodeEscape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.escape_unicode().fmt(f)
    }
}

impl From<char> for UnicodeEscape {
    fn from(value: char) -> Self {
        Self(value)
    }
}

pub fn unicode_escape(input: &str) -> IResult<&str, UnicodeEscape> {
    map_opt(
        preceded(
            tag(r"\u"),
            delimited(
                nchar::char('{'),
                take_while_m_n(1, 6, AsChar::is_hex_digit),
                nchar::char('}'),
            ),
        ),
        |digits: &str| {
            u32::from_str_radix(digits, 16)
                .ok()
                .and_then(char::from_u32)
                .map(UnicodeEscape)
        },
    )
    .parse(input)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum CharLiteralInner {
    #[cfg_attr(
        test,
        proptest(
            strategy = r##"string_regex(r#"[^'\\\n\r\t]"#).unwrap().prop_map(|c| CharLiteralInner::Char(c.chars().next().unwrap()))"##
        )
    )]
    Char(char),
    QuoteEscape(QuoteEscape),
    AsciiEscape(AsciiEscape),
    UnicodeEscape(UnicodeEscape),
}

impl Display for CharLiteralInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char(c) => c.fmt(f),
            Self::QuoteEscape(q) => q.fmt(f),
            Self::AsciiEscape(a) => a.fmt(f),
            Self::UnicodeEscape(u) => u.fmt(f),
        }
    }
}

impl From<QuoteEscape> for CharLiteralInner {
    fn from(value: QuoteEscape) -> Self {
        Self::QuoteEscape(value)
    }
}

impl From<AsciiEscape> for CharLiteralInner {
    fn from(value: AsciiEscape) -> Self {
        Self::AsciiEscape(value)
    }
}

impl From<UnicodeEscape> for CharLiteralInner {
    fn from(value: UnicodeEscape) -> Self {
        Self::UnicodeEscape(value)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CharLiteral<'suffix> {
    inner: CharLiteralInner,
    suffix: Option<&'suffix str>,
}

impl<'suffix> Display for CharLiteral<'suffix> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'{}", self.inner, self.suffix.unwrap_or(""))
    }
}

pub fn char_literal(input: &str) -> IResult<&str, CharLiteral> {
    delimited(
        nchar::char('\''),
        none_of(&['\'', '\\', '\n', '\r', '\t'][..])
            .map(CharLiteralInner::Char)
            .or(Parser::into(quote_escape))
            .or(Parser::into(ascii_escape))
            .or(Parser::into(unicode_escape)),
        nchar::char('\''),
    )
    .and(opt(suffix))
    .map(|(inner, suffix)| CharLiteral { inner, suffix })
    .parse(input)
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::string::string_regex;

    use crate::{
        de::parse::{
            literal::tests::SUFFIX, AsciiEscape, CharLiteral, CharLiteralInner, QuoteEscape,
            UnicodeEscape,
        },
        test_parse_complex,
    };

    // const ASCII_ESCAPE_SPECIALS: &[char] = &['\n', '\r', '\t', '\\', '\0'];

    test_parse_complex!(quote_escape;
        q in proptest::prelude::any::<QuoteEscape>()
        => &q.to_string()
        => ""; q
    );

    test_parse_complex!(ascii_escape;
        val in proptest::prelude::any::<AsciiEscape>()
        => &val.to_string()
        => ""; val
    );
    test_parse_complex!(unicode_escape;
        val in proptest::prelude::any::<UnicodeEscape>()
        => &val.to_string()
        => ""; val
    );

    test_parse_complex!(char_literal;
        inner in proptest::prelude::any::<CharLiteralInner>(),
        suffix in string_regex(&format!(r#"({SUFFIX})?"#)).unwrap()
        => &format!(r#"'{inner}'{suffix}"#)
        => ""; CharLiteral {
            inner,
            suffix: match suffix.as_str() {
                "" => None,
                _ => Some(suffix.as_str())
            }
        }
    );
}
