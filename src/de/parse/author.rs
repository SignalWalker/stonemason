use super::{IsolatedCr, Many, Parenthesized, ParseError, Parsed, Surrounded};
use crate::de::parse::Unparse;
use nom::{
    character::complete::satisfy,
    combinator::recognize,
    multi::{many0, many1},
    IResult, Parser,
};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

mod comment;
pub use comment::*;

mod space;
pub use space::*;

/// Whitespace1 | Comment
#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, Parsed)]
pub enum Authorial<'data> {
    Space(Whitespace1<&'data str>),
    Comment(Comment<&'data str>),
}

impl<'data> std::fmt::Debug for Authorial<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Space(space) => std::fmt::Debug::fmt(&space, f),
            Self::Comment(comment) => std::fmt::Debug::fmt(&comment, f),
        }
    }
}

/// A sequence of (Whitespace | Comment).
#[derive(Clone, PartialEq, Eq, Hash, Unparse, UnparseDisplay, Parsed)]
#[repr(transparent)]
pub struct AuthorialMany<'data, const REQ: bool = false>(Many<Authorial<'data>, REQ>);
/// Authorial*
pub type Authorial0<'data> = AuthorialMany<'data, false>;
/// Authorial+
pub type Authorial1<'data> = AuthorialMany<'data, true>;

impl<'data, const REQ: bool> std::fmt::Debug for AuthorialMany<'data, REQ> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 .0.is_empty() {
            true => write!(f, "."),
            false => write!(f, "¶"),
        }
    }
}

pub type AuthorialDelim<'data, D, const REQ: bool = false> =
    Surrounded<AuthorialMany<'data, REQ>, D>;
pub type AuthorialParen<'data, D, const REQ: bool = false> =
    Parenthesized<AuthorialDelim<'data, D, REQ>>;

#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        de::parse::{Comment, Parsed, Whitespace},
        test_parse_complex,
    };

    test_parse_complex!(pattern_white_space, crate::de::parse::pattern_white_space::<nom::error::VerboseError<_>>;
        s in r#"\p{PATTERN_WHITE_SPACE}"#
        => s.as_str()
        => ""; s.chars().next().unwrap()
    );

    test_parse_complex!(whitespace0, Whitespace::<false>::from_parse::<nom::error::VerboseError<_>>;
        s in r#"\p{PATTERN_WHITE_SPACE}*"#
        => s.as_str()
        => ""; Whitespace::<false>(s.as_str())
    );

    test_parse_complex!(whitespace1, Whitespace::<true>::from_parse::<nom::error::VerboseError<_>>;
        s in r#"\p{PATTERN_WHITE_SPACE}+"#
        => s.as_str()
        => ""; Whitespace::<true>(s.as_str())
    );

    test_parse_complex!(line_comment, crate::de::parse::line_comment::<nom::error::VerboseError<_>>;
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
