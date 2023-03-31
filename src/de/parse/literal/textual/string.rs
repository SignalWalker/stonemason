use std::fmt::Display;

use nom::{
    bytes::complete::tag,
    character::complete::{anychar, one_of, satisfy},
    combinator::{not, opt, recognize, value},
    multi::many0,
    sequence::{delimited, preceded},
    IResult, Parser,
};
#[cfg(test)]
use proptest_derive::Arbitrary;

#[cfg(test)]
use proptest::{strategy::Strategy, string::string_regex};

use crate::de::parse::{isolated_cr, suffix};

use super::{
    ascii_escape, byte_escape, quote_escape, unicode_escape, AsciiEscape, ByteEscape, QuoteEscape,
    UnicodeEscape,
};

use nom::character::complete as nchar;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum StringLiteralInner {
    #[cfg_attr(
        test,
        proptest(
            strategy = r##"string_regex(r#"[^"\\\n\r\t]"#).unwrap().prop_map(|c| StringLiteralInner::Char(c.chars().next().unwrap()))"##
        )
    )]
    Char(char),
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

impl Display for StringLiteralInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringLiteralInner::Char(c) => c.fmt(f),
            StringLiteralInner::Continue => write!(f, "\\\n"),
            StringLiteralInner::QuoteEscape(q) => q.fmt(f),
            StringLiteralInner::AsciiEscape(a) => a.fmt(f),
            StringLiteralInner::UnicodeEscape(u) => u.fmt(f),
        }
    }
}

pub fn string_literal_inner(input: &str) -> IResult<&str, StringLiteralInner> {
    not(one_of(&['"', '\\'][..]).or(isolated_cr))
        .and(anychar)
        .map(|(_, c)| StringLiteralInner::Char(c))
        .or(Parser::into(quote_escape))
        .or(Parser::into(ascii_escape))
        .or(Parser::into(unicode_escape))
        .or(value(StringLiteralInner::Continue, tag("\\\n")))
        .parse(input)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StringLiteral<'data> {
    inner: Vec<StringLiteralInner>,
    suffix: Option<&'data str>,
}

impl<'data> Display for StringLiteral<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"")?;
        for i in &self.inner {
            write!(f, "{i}")?;
        }
        write!(f, "\"{}", self.suffix.unwrap_or(""))
    }
}

pub fn string_literal(input: &str) -> IResult<&str, StringLiteral> {
    delimited(
        nchar::char('"'),
        many0(string_literal_inner),
        nchar::char('"'),
    )
    .and(opt(suffix))
    .map(|(inner, suffix)| StringLiteral { inner, suffix })
    .parse(input)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RawStringLiteral<'data> {
    hash_depth: usize,
    inner: &'data str,
    suffix: Option<&'data str>,
}

impl<'data> Display for RawStringLiteral<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

pub fn raw_string_literal(input: &str) -> IResult<&str, RawStringLiteral> {
    fn raw_string_content(input: &str) -> IResult<&str, (usize, &str)> {
        delimited(
            nchar::char('"'),
            recognize(many0(not(isolated_cr).and(anychar))),
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ByteStringInner {
    #[cfg_attr(
        test,
        proptest(
            strategy = r##"string_regex(r#"[[:ascii:]&&[^"\\\n\r\t]]"#).unwrap().prop_map(|c| ByteStringInner::Ascii(u8::try_from(c.chars().next().unwrap()).unwrap()))"##
        )
    )]
    Ascii(u8),
    Escape(ByteEscape),
    Continue,
}

impl From<ByteEscape> for ByteStringInner {
    fn from(value: ByteEscape) -> Self {
        Self::Escape(value)
    }
}

impl Display for ByteStringInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteStringInner::Ascii(c) => (*c as char).fmt(f),
            ByteStringInner::Escape(e) => e.fmt(f),
            ByteStringInner::Continue => write!(f, "\\\n"),
        }
    }
}

pub fn byte_string_inner(input: &str) -> IResult<&str, ByteStringInner> {
    not(one_of(&['"', '\\'][..]).or(isolated_cr))
        .and(satisfy(|c| (c as u32) <= 0x7F))
        .map(|(_, c)| ByteStringInner::Ascii(u8::try_from(c).unwrap()))
        .or(Parser::into(byte_escape))
        .or(value(ByteStringInner::Continue, tag("\\\n")))
        .parse(input)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ByteStringLiteral<'data> {
    inner: Vec<ByteStringInner>,
    suffix: Option<&'data str>,
}

impl<'data> Display for ByteStringLiteral<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "b\"")?;
        for i in &self.inner {
            write!(f, "{i}")?;
        }
        write!(f, "\"{}", self.suffix.unwrap_or(""))
    }
}

pub fn byte_string_literal(input: &str) -> IResult<&str, ByteStringLiteral> {
    delimited(tag("b\""), many0(byte_string_inner), nchar::char('"'))
        .and(opt(suffix))
        .map(|(inner, suffix)| ByteStringLiteral { inner, suffix })
        .parse(input)
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
