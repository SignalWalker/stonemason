use std::fmt::Display;

use nom::{
    bytes::complete::tag,
    character::complete::{anychar, one_of},
    combinator::{not, opt, value},
    multi::many0,
    sequence::delimited,
    IResult, Parser,
};
#[cfg(test)]
use proptest_derive::Arbitrary;

#[cfg(test)]
use proptest::{strategy::Strategy, string::string_regex};

use crate::de::parse::{isolated_cr, suffix};

use super::{ascii_escape, quote_escape, unicode_escape, AsciiEscape, QuoteEscape, UnicodeEscape};

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

#[cfg(test)]
pub(crate) mod tests {
    use proptest::string::string_regex;

    use crate::{
        de::parse::{literal::tests::SUFFIX, StringLiteral, StringLiteralInner},
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
}
