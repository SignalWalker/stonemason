use std::fmt::Display;

use nom::{
    bytes::complete::{tag, take_while},
    combinator::not,
    sequence::preceded,
    IResult, Parser,
};

use nom::character::complete as nchar;

mod module;
pub use module::*;

pub const UTF8BOM: char = '\u{FEFF}';

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Shebang<'data>(&'data str);

impl<'data> Display for Shebang<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#!{}\n", self.0)
    }
}

pub fn shebang(input: &str) -> IResult<&str, Shebang> {
    preceded(
        tag("#!").and(not(nchar::char('['))),
        take_while(|c| c != '\n'),
    )
    .map(Shebang)
    .parse(input)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Stone<'data> {
    utf8bom: bool,
    shebang: Option<Shebang<'data>>,
}

impl<'data> Display for Stone<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.utf8bom {
            write!(f, "{UTF8BOM}")?;
        }
        if let Some(sb) = self.shebang {
            sb.fmt(f)?;
        }
        todo!()
    }
}
