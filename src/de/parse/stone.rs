use super::{
    Authorial0, Authorial1, InnerAttribute, Item, Many0, ParseError, Parsed, Separated0, Token,
    Whitespace1,
};
use crate::de::parse::Unparse;
use nom::character::complete as nchar;
use nom::combinator::all_consuming;
use nom::{
    bytes::complete::{tag, take_while},
    combinator::not,
    sequence::preceded,
    IResult, Parser,
};
use stonemason_proc::{Unparse, UnparseDisplay};

mod module;
pub use module::*;

pub const UTF8BOM: char = '\u{FEFF}';
pub type Utf8Bom = Token<UTF8BOM>;

/// A shebang. May be any string `#!(prg:.*)\n` where `prg` does not begin with `[` (to avoid
/// conflict with inner attribute parsing).
#[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(prefix("#!"))]
pub struct Shebang<'data>(&'data str);

impl std::fmt::Debug for Shebang<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#!{}", self.0)
    }
}

impl<'data> Parsed<&'data str> for Shebang<'data> {
    #[tracing::instrument(name = "Shebang", level = "trace")]
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
        preceded(
            tag("#!").and(not(nchar::char('['))),
            take_while(|c| c != '\n'),
        )
        .map(Shebang)
        .parse(input)
    }
}

#[derive(Debug, Clone, Unparse, UnparseDisplay)]
pub struct Stone<'data> {
    /// UTF-8 byte order mark; ignored.
    utf8bom: Option<Utf8Bom>,
    /// Indicates to the operating system the program with which to execute the headed file.
    /// Optional, and ignored by the compiler.
    shebang: Option<Shebang<'data>>,
    _c1: Authorial0<'data>,
    /// Attributes applied to the Stone itself.
    attrs: Many0<(InnerAttribute<'data>, Authorial0<'data>)>,
    items: Many0<(Item<'data>, Authorial0<'data>)>,
}

impl<'data> Parsed<&'data str> for Stone<'data> {
    #[cfg_attr(
        feature = "tracing-extra",
        tracing::instrument(name = "Stone", level = "debug", err, ret)
    )]
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> IResult<&'data str, Self, Error> {
        all_consuming(
            Option::from_parse
                .and(Option::from_parse)
                .and(Authorial0::from_parse)
                .and(Many0::from_parse)
                .and(Many0::from_parse)
                .map(|((((utf8bom, shebang), _c1), attrs), items)| Self {
                    utf8bom,
                    shebang,
                    _c1,
                    attrs,
                    items,
                }),
        )
        .parse(input)
    }
}
