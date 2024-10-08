use std::char::TryFromCharError;

use nom::{
    character::complete::satisfy,
    combinator::{map_opt, map_res, opt, value},
    error::FromExternalError,
    sequence::{delimited, preceded},
    AsChar, IResult, Parser,
};

use nom::character::complete as nchar;

#[cfg(test)]
use proptest_derive::Arbitrary;

#[cfg(test)]
use proptest::{strategy::Strategy, string::string_regex};
use stonemason_proc::{Unparse, UnparseDisplay};

use crate::de::parse::{fmt_byte_to_ascii_escape, suffix, ParseError, Parsed, Unparse};

pub fn ascii_for_char<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, u8, Error> {
    satisfy(|c| c != '\'' && c != '\\' && c != '\n' && c != '\r' && c != '\t' && c.is_ascii())
        .map(|c| u8::try_from(c).unwrap()) // already checked `is_ascii`
        .parse(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ByteEscape {
    Code(#[unparse({fmt_byte_to_ascii_escape(*__self_0, f)?;})] u8),
    #[unparse("\\n")]
    NewLine,
    #[unparse("\\r")]
    Return,
    #[unparse("\\t")]
    Tab,
    #[unparse("\\\\")]
    Backslash,
    #[unparse("\\0")]
    Null,
    #[unparse("\\'")]
    QuoteSingle,
    #[unparse("\\\"")]
    QuoteDouble,
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

impl<'data> Parsed<&'data str> for ByteEscape {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
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
}

pub fn byte_escape<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, ByteEscape, Error> {
    ByteEscape::from_parse(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ByteLiteralInner {
    #[cfg_attr(
        test,
        proptest(
            strategy = r##"string_regex(r#"[[:ascii:]&&[^'\\\n\r\t]]"#).unwrap().prop_map(|c| ByteLiteralInner::Ascii(u8::try_from(c.chars().next().unwrap()).unwrap()))"##
        )
    )]
    Ascii(#[unparse((char::from(*__self_0)))] u8),
    Escape(ByteEscape),
}

impl From<ByteEscape> for ByteLiteralInner {
    fn from(value: ByteEscape) -> Self {
        Self::Escape(value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct ByteLiteral<'suffix> {
    inner: ByteLiteralInner,
    #[unparse({self.suffix.unwrap_or("").unparse(f)?;})]
    suffix: Option<&'suffix str>,
}

impl<'data> Parsed<&'data str> for ByteLiteral<'data> {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
        preceded::<_, _, _, Error, _, _>(
            nchar::char('b'),
            delimited::<_, _, _, _, Error, _, _, _>(
                nchar::char('\''),
                ascii_for_char
                    .map(ByteLiteralInner::Ascii)
                    .or(nom::combinator::into(byte_escape::<Error>)),
                nchar::char('\''),
            ),
        )
        .and(opt::<_, _, Error, _>(suffix::<Error>))
        .map(|(inner, suffix)| ByteLiteral { inner, suffix })
        .parse(input)
    }
}

pub fn byte_literal<
    'data,
    Error: ParseError<&'data str> + FromExternalError<&'data str, TryFromCharError>,
>(
    input: &'data str,
) -> IResult<&str, ByteLiteral, Error> {
    ByteLiteral::from_parse(input)
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
