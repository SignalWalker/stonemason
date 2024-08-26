use super::{
    ascii_escape, byte_escape, quote_escape, unicode_escape, AsciiEscape, ByteEscape, QuoteEscape,
    UnicodeEscape,
};
use crate::de::parse::{suffix, IsolatedCr, ParseError, Parsed, Unparse};
use nom::character::complete as nchar;
use nom::{
    bytes::complete::tag,
    character::complete::{anychar, one_of, satisfy},
    combinator::{not, opt, recognize, value},
    multi::many0,
    sequence::{delimited, preceded},
    IResult, Parser,
};
#[cfg(test)]
use proptest::{strategy::Strategy, string::string_regex};
#[cfg(test)]
use proptest_derive::Arbitrary;
use stonemason_proc::{Unparse, UnparseDisplay};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Unparse, UnparseDisplay)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum StringLiteralInner {
    #[cfg_attr(
        test,
        proptest(
            strategy = r##"string_regex(r#"[^"\\\n\r\t]"#).unwrap().prop_map(|c| StringLiteralInner::Char(c.chars().next().unwrap()))"##
        )
    )]
    Char(char),
    #[unparse("\\\n")]
    Continue,
    QuoteEscape(QuoteEscape),
    AsciiEscape(AsciiEscape),
    UnicodeEscape(UnicodeEscape),
}

impl From<QuoteEscape> for StringLiteralInner {
    fn from(value: QuoteEscape) -> Self {
        Self::QuoteEscape(value)
    }
}

impl From<AsciiEscape> for StringLiteralInner {
    fn from(value: AsciiEscape) -> Self {
        Self::AsciiEscape(value)
    }
}

impl From<UnicodeEscape> for StringLiteralInner {
    fn from(value: UnicodeEscape) -> Self {
        Self::UnicodeEscape(value)
    }
}

pub fn string_literal_inner<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, StringLiteralInner, Error> {
    not(one_of(&['"', '\\'][..]).or(Parser::into(IsolatedCr::from_parse)))
        .and(anychar)
        .map(|(_, c)| StringLiteralInner::Char(c))
        .or(Parser::into(quote_escape))
        .or(Parser::into(ascii_escape))
        .or(Parser::into(unicode_escape))
        .or(value(StringLiteralInner::Continue, tag("\\\n")))
        .parse(input)
}

#[derive(Debug, Eq, PartialEq, Clone, Unparse, UnparseDisplay)]
pub struct StringLiteral<'data> {
    #[unparse(delim('"', '"'))]
    inner: Vec<StringLiteralInner>,
    #[unparse(self.suffix.unwrap_or(""))]
    suffix: Option<&'data str>,
}

impl<'data> Parsed<&'data str> for StringLiteral<'data> {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
        delimited(
            nchar::char('"'),
            many0(string_literal_inner),
            nchar::char('"'),
        )
        .and(opt(suffix))
        .map(|(inner, suffix)| Self { inner, suffix })
        .parse(input)
    }
}

pub fn string_literal<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, StringLiteral, Error> {
    StringLiteral::from_parse(input)
}

#[derive(Debug, Eq, PartialEq, Clone, UnparseDisplay)]
pub struct RawStringLiteral<'data> {
    hash_depth: usize,
    inner: &'data str,
    suffix: Option<&'data str>,
}

impl<'data> Unparse for RawStringLiteral<'data> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut hash = String::new();
        for _ in 0..self.hash_depth {
            hash.push('#');
        }
        write!(
            f,
            "r{hash}\"{}\"{hash}{}",
            self.inner,
            self.suffix.unwrap_or("")
        )
    }
}

impl<'data> Parsed<&'data str> for RawStringLiteral<'data> {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
        fn raw_string_content<'data, Error: ParseError<&'data str>>(
            input: &'data str,
        ) -> IResult<&'data str, (usize, &'data str), Error> {
            delimited(
                nchar::char('"'),
                recognize(many0(not(IsolatedCr::from_parse).and(anychar))),
                nchar::char('"'),
            )
            .map(|inner| (0, inner))
            .or(delimited(
                nchar::char('#'),
                raw_string_content.map(|(hash_depth, inner)| (hash_depth + 1, inner)),
                nchar::char('#'),
            ))
            .parse(input)
        }
        preceded(nchar::char('r'), raw_string_content)
            .and(opt(suffix))
            .map(|((hash_depth, inner), suffix)| RawStringLiteral {
                hash_depth,
                inner,
                suffix,
            })
            .parse(input)
    }
}

pub fn raw_string_literal<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, RawStringLiteral, Error> {
    RawStringLiteral::from_parse(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Unparse, UnparseDisplay)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ByteStringInner {
    #[cfg_attr(
        test,
        proptest(
            strategy = r##"string_regex(r#"[[:ascii:]&&[^"\\\n\r\t]]"#).unwrap().prop_map(|c| ByteStringInner::Ascii(u8::try_from(c.chars().next().unwrap()).unwrap()))"##
        )
    )]
    #[unparse((*__self_0 as char))]
    Ascii(u8),
    Escape(ByteEscape),
    #[unparse("\\\n")]
    Continue,
}

impl From<ByteEscape> for ByteStringInner {
    fn from(value: ByteEscape) -> Self {
        Self::Escape(value)
    }
}

pub fn byte_string_inner<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, ByteStringInner, Error> {
    not(one_of(&['"', '\\'][..]).or(Parser::into(IsolatedCr::from_parse)))
        .and(satisfy(|c| (c as u32) <= 0x7F))
        .map(|(_, c)| ByteStringInner::Ascii(u8::try_from(c).unwrap()))
        .or(Parser::into(byte_escape))
        .or(value(ByteStringInner::Continue, tag("\\\n")))
        .parse(input)
}

#[derive(Debug, Clone, Eq, PartialEq, UnparseDisplay)]
pub struct ByteStringLiteral<'data> {
    inner: Vec<ByteStringInner>,
    suffix: Option<&'data str>,
}

impl<'data> Unparse for ByteStringLiteral<'data> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "b\"")?;
        for i in &self.inner {
            write!(f, "{i}")?;
        }
        write!(f, "\"{}", self.suffix.unwrap_or(""))
    }
}

impl<'data> Parsed<&'data str> for ByteStringLiteral<'data> {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
        delimited(tag("b\""), many0(byte_string_inner), nchar::char('"'))
            .and(opt(suffix))
            .map(|(inner, suffix)| ByteStringLiteral { inner, suffix })
            .parse(input)
    }
}

pub fn byte_string_literal<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, ByteStringLiteral, Error> {
    ByteStringLiteral::from_parse(input)
}

#[derive(Debug, Eq, PartialEq, Clone, UnparseDisplay)]
pub struct RawByteStringLiteral<'data> {
    hash_depth: usize,
    inner: Vec<u8>,
    suffix: Option<&'data str>,
}

impl<'data> Unparse for RawByteStringLiteral<'data> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut hash = String::new();
        for _ in 0..self.hash_depth {
            hash.push('#');
        }
        write!(f, "br{hash}\"")?;
        for b in &self.inner {
            write!(f, "{}", char::from(*b))?;
        }
        write!(f, "\"{hash}{}", self.suffix.unwrap_or(""))
    }
}

impl<'data> Parsed<&'data str> for RawByteStringLiteral<'data> {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
        fn raw_byte_string_content<'data, Error: ParseError<&'data str>>(
            input: &'data str,
        ) -> IResult<&'data str, (usize, Vec<u8>), Error> {
            delimited(
                nchar::char('"'),
                many0(satisfy(|c| c as u32 <= 0x7F).map(|c| u8::try_from(c).unwrap())),
                nchar::char('"'),
            )
            .map(|inner| (0, inner))
            .or(delimited(
                nchar::char('#'),
                raw_byte_string_content.map(|(hash_depth, inner)| (hash_depth + 1, inner)),
                nchar::char('#'),
            ))
            .parse(input)
        }
        preceded(tag("br"), raw_byte_string_content)
            .and(opt(suffix))
            .map(|((hash_depth, inner), suffix)| RawByteStringLiteral {
                hash_depth,
                inner,
                suffix,
            })
            .parse(input)
    }
}

pub fn raw_byte_string_literal<'data, Error: ParseError<&'data str>>(
    input: &'data str,
) -> IResult<&'data str, RawByteStringLiteral, Error> {
    RawByteStringLiteral::from_parse(input)
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::string::string_regex;

    use crate::{
        de::parse::{
            literal::tests::SUFFIX, ByteStringInner, ByteStringLiteral, StringLiteral,
            StringLiteralInner,
        },
        test_parse_complex,
    };

    test_parse_complex!(string_literal_inner;
        c in proptest::prelude::any::<StringLiteralInner>()
        => &c.to_string()
        => ""; c
    );

    test_parse_complex!(string_literal;
        // TODO: arbitrarily long strings
        chars in proptest::collection::vec(proptest::prelude::any::<StringLiteralInner>(), ..=128),
        suffix in string_regex(&format!("({SUFFIX})?")).unwrap()
        => &format!(r#""{}"{suffix}"#, {let mut res = String::new(); for c in &chars { res.push_str(&c.to_string()) } res})
        => ""; StringLiteral {
            inner: chars,
            suffix: match suffix.as_str() {
                "" => None,
                _ => Some(suffix.as_str())
            }
        }
    );

    test_parse_complex!(byte_string_inner;
        b in proptest::prelude::any::<ByteStringInner>()
        => &b.to_string()
        => ""; b
    );

    test_parse_complex!(byte_string_literal;
        // TODO: arbitrarily long strings
        chars in proptest::collection::vec(proptest::prelude::any::<ByteStringInner>(), ..=128),
        suffix in string_regex(&format!("({SUFFIX})?")).unwrap()
        => &format!(r#"b"{}"{suffix}"#, {let mut res = String::new(); for c in &chars { res.push_str(&c.to_string()) } res})
        => ""; ByteStringLiteral {
            inner: chars,
            suffix: match suffix.as_str() {
                "" => None,
                _ => Some(suffix.as_str())
            }
        }
    );

    // test_parse_complex!(raw_string_literal;
    //     inner in r#"([^\r]|(\r\n))*"#,
    //     => todo!()
    //     => ""; todo!()
    // );
}
