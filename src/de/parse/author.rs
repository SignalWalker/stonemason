use std::fmt::Display;

use nom::{
    bytes::{complete::take_while, streaming::tag},
    character::{
        complete::{anychar, satisfy},
        streaming::none_of,
    },
    combinator::{not, opt, recognize, value},
    error::ParseError,
    multi::{many0, many1},
    sequence::{delimited, preceded},
    IResult, Parser,
};
use stonemason_proc::{Unparse, UnparseDisplay};

use crate::de::parse::Unparse;

use super::{isolated_cr, Parsed};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum Comment<'text> {
    #[unparse(delim("//", _, '\n'))]
    Line(&'text str),
    #[unparse(delim("//!", _, '\n'))]
    LineDocInner(&'text str),
    #[unparse(delim("///", _, '\n'))]
    LineDocOuter(&'text str),
    #[unparse(delim("/*", _, "*/"))]
    Block(&'text str),
    #[unparse(delim("/*!", _, "*/"))]
    BlockDocInner(&'text str),
    #[unparse(delim("/**", _, "*/"))]
    BlockDocOuter(&'text str),
}

fn not_char<'data, Error: ParseError<&'data str>>(c: char) -> impl Parser<&'data str, char, Error> {
    satisfy(move |v| v != c)
}

fn line_doc_text_no_isolated_cr(input: &str) -> IResult<&str, Vec<(char, ())>> {
    many0(not_char('\n').and(not(isolated_cr))).parse(input)
}

pub fn line_comment(input: &str) -> IResult<&str, Comment> {
    preceded(tag("//"), take_while(|c| c != '\n'))
        .map(Comment::Line)
        .parse(input)
}

pub fn inner_line_doc(input: &str) -> IResult<&str, Comment> {
    preceded(tag("//!"), recognize(line_doc_text_no_isolated_cr))
        .map(Comment::LineDocInner)
        .parse(input)
}

pub fn outer_line_doc(input: &str) -> IResult<&str, Comment> {
    preceded(
        tag("///"),
        recognize(opt(not_char('/').and(line_doc_text_no_isolated_cr))),
    )
    .map(Comment::LineDocOuter)
    .parse(input)
}

fn block_doc_text_no_isolated_cr(input: &str) -> IResult<&str, ((), char)> {
    not(tag("*/").or(recognize(isolated_cr)))
        .and(anychar)
        .parse(input)
}

pub fn inner_block_doc(input: &str) -> IResult<&str, Comment> {
    delimited(
        tag("/*!"),
        recognize(block_comment_or_doc).or(recognize(many0(block_doc_text_no_isolated_cr))),
        tag("*/"),
    )
    .map(Comment::BlockDocInner)
    .parse(input)
}

pub fn outer_block_doc(input: &str) -> IResult<&str, Comment> {
    delimited(
        tag("/**"),
        recognize(
            (recognize(not_char('*')).or(recognize(block_comment_or_doc))).and(recognize(many0(
                recognize(block_comment_or_doc).or(recognize(block_doc_text_no_isolated_cr)),
            ))),
        ),
        tag("*/"),
    )
    .map(Comment::BlockDocOuter)
    .parse(input)
}

pub fn block_comment(input: &str) -> IResult<&str, Comment> {
    delimited(
        tag("/*"),
        recognize(
            recognize(none_of("*!"))
                .or(tag("**"))
                .or(recognize(block_comment_or_doc))
                .and(recognize(many0(
                    recognize(block_comment_or_doc).or(recognize(not(tag("*/")).and(anychar))),
                ))),
        ),
        tag("*/"),
    )
    .map(Comment::Block)
    .or(value(Comment::Block(""), tag("/**/")))
    .or(value(Comment::Block("*"), tag("/***/")))
    .parse(input)
}

pub fn block_comment_or_doc(input: &str) -> IResult<&str, Comment> {
    block_comment
        .or(outer_block_doc)
        .or(inner_block_doc)
        .parse(input)
}

pub fn comment(input: &str) -> IResult<&str, Comment> {
    inner_line_doc
        .or(outer_line_doc)
        .or(line_comment)
        .or(inner_block_doc)
        .or(outer_block_doc)
        .or(block_comment)
        .parse(input)
}

pub fn pattern_white_space(input: &str) -> IResult<&str, char> {
    satisfy(|c| {
        let b = c as u32;
        (0x9 <= b && b <= 0xD) // \t \n `vertical tab` `form feed` \r
            || (c == '\u{20}') // `space`
            || (c == '\u{85}') // `next line`
            || (c == '\u{200E}') // `left-to-right mark`
            || (c == '\u{200F}') // `right-to-left mark`
            || (c == '\u{2028}') // `line separator`
            || (c == '\u{2029}') // `paragraph separator`
    })
    .parse(input)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse)]
pub struct Whitespace<'data, const REQ: bool>(pub &'data str);

impl<'data, const REQ: bool> Parsed<&'data str> for Whitespace<'data, REQ> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        if REQ {
            recognize(many1(pattern_white_space)).map(Self).parse(input)
        } else {
            recognize(many0(pattern_white_space)).map(Self).parse(input)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum Authorial<'data> {
    Space(Whitespace<'data, true>),
    Comment(Comment<'data>),
}

impl<'data> From<Comment<'data>> for Authorial<'data> {
    fn from(value: Comment<'data>) -> Self {
        Self::Comment(value)
    }
}

pub fn authorial(input: &str) -> IResult<&str, Authorial> {
    Whitespace::from_parse
        .map(|s| Authorial::Space(s))
        .or(Parser::into(comment))
        .parse(input)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct AuthorialMulti<'data, const REQ: bool = false>(pub Vec<Authorial<'data>>);

// impl<'data, const REQ: bool> Unparse for AuthorialMulti<'data, REQ> {
//     fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.0.unparse(f)
//     }
// }

impl<'data> From<Vec<Authorial<'data>>> for AuthorialMulti<'data, false> {
    #[inline]
    fn from(value: Vec<Authorial<'data>>) -> Self {
        Self(value)
    }
}

impl<'data, const REQ: bool> Parsed<&'data str> for AuthorialMulti<'data, REQ> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        if REQ {
            many1(authorial).map(Self).parse(input)
        } else {
            many0(authorial).map(Self).parse(input)
        }
    }
}

// pub fn commented<'i, Error: ParseError<&'i str>>(p: ()) -> impl Parser<&'i str, (), Error> {
//     todo!()
// }

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse)]
pub struct AuthorialPre<'data, T, const REQ: bool = false>(AuthorialMulti<'data, REQ>, T);

impl<'data, T: Display, const REQ: bool> Display for AuthorialPre<'data, T, REQ> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl<'data, T: Parsed<&'data str>, const REQ: bool> Parsed<&'data str>
    for AuthorialPre<'data, T, REQ>
{
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        AuthorialMulti::<'data, REQ>::from_parse
            .and(T::from_parse)
            .map(|(c, t)| Self(c, t))
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct AuthorialSuf<'data, T, const REQ: bool = false>(T, AuthorialMulti<'data, REQ>);

impl<'data, T: Parsed<&'data str>, const REQ: bool> Parsed<&'data str>
    for AuthorialSuf<'data, T, REQ>
{
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        T::from_parse
            .and(AuthorialMulti::from_parse)
            .map(|(c, t)| Self(c, t))
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct AuthorialDelim<'data, T, const REQ: bool = false>(
    AuthorialMulti<'data, REQ>,
    T,
    AuthorialMulti<'data, REQ>,
);

impl<'data, T: Parsed<&'data str>, const REQ: bool> Parsed<&'data str>
    for AuthorialDelim<'data, T, REQ>
{
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        AuthorialMulti::from_parse
            .and(T::from_parse)
            .and(AuthorialMulti::from_parse)
            .map(|((cp, t), cs)| Self(cp, t, cs))
            .parse(input)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        de::parse::{Comment, Parsed, Whitespace},
        test_parse_complex,
    };

    test_parse_complex!(pattern_white_space;
        s in r#"\p{PATTERN_WHITE_SPACE}"#
        => s.as_str()
        => ""; s.chars().next().unwrap()
    );

    test_parse_complex!(whitespace0, Whitespace::<false>::from_parse;
        s in r#"\p{PATTERN_WHITE_SPACE}*"#
        => s.as_str()
        => ""; Whitespace::<false>(s.as_str())
    );

    test_parse_complex!(whitespace1, Whitespace::<true>::from_parse;
        s in r#"\p{PATTERN_WHITE_SPACE}+"#
        => s.as_str()
        => ""; Whitespace::<true>(s.as_str())
    );

    test_parse_complex!(line_comment;
        text in r#"[^\n]*"#
        => format!("//{text}").as_str()
        => ""; Comment::Line(text.as_str())
    );

    test_parse_complex!(inner_line_doc;
        text in r#"([^\n\r])*"#
        => format!("//!{text}").as_str()
        => ""; Comment::LineDocInner(text.as_str())
    );

    test_parse_complex!(outer_line_doc;
        text in r#"([^/]([^\n\r])*)?"#
        => format!("///{text}").as_str()
        => ""; Comment::LineDocOuter(text.as_str())
    );

    // test_parse_complex!(block_comment;
    //     text in r#"(?s)(([^*]|(\*[^/]))*)"#
    //     => format!("/*{text}*/").as_str()
    //     => ""; Comment::Block(text.as_str())
    // );
}
