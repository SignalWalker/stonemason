use std::fmt::Display;

use nom::{
    bytes::{complete::take_while, streaming::tag},
    character::{
        complete::{anychar, satisfy},
        streaming::none_of,
    },
    combinator::{not, opt, recognize, value},
    error::ParseError,
    multi::many0,
    sequence::{delimited, preceded},
    IResult, Parser,
};

use super::isolated_cr;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Comment<'text> {
    Line(&'text str),
    LineDocInner(&'text str),
    LineDocOuter(&'text str),
    Block(&'text str),
    BlockDocInner(&'text str),
    BlockDocOuter(&'text str),
}

impl<'text> Display for Comment<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Comment::Line(t) => write!(f, "//{t}\n"),
            Comment::LineDocInner(t) => write!(f, "//!{t}\n"),
            Comment::LineDocOuter(t) => write!(f, "///{t}\n"),
            Comment::Block(t) => write!(f, "/*{t}*/"),
            Comment::BlockDocInner(t) => write!(f, "/*!{t}*/"),
            Comment::BlockDocOuter(t) => write!(f, "/**{t}*/"),
        }
    }
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

#[cfg(test)]
pub(crate) mod tests {
    use crate::{de::parse::Comment, test_parse_complex};

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
