use nom::{
    combinator::{opt, value, verify},
    multi::{many0, many1},
    IResult, Parser,
};
use stonemason_proc::{Unparse, UnparseDisplay};

use nom::character::complete as nchar;

use super::{Parsed, Unparse};

pub type Delimited<A, Data, B> = (A, Data, B);
pub type Surrounded<Surround, Data> = Delimited<Surround, Data, Surround>;

pub type Parenthesized<Data> = Delimited<Token<'('>, Data, Token<')'>>;
pub type CurlyBraced<Data> = Delimited<Token<'{'>, Data, Token<'}'>>;
pub type SquareBracketed<Data> = Delimited<Token<'['>, Data, Token<']'>>;
pub type AngleBracketed<Data> = Delimited<Token<'<'>, Data, Token<'>'>>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Separated<Data, Separator, const REQ: bool = false> {
    pairs: Vec<(Data, Separator)>,
    end: Option<Data>,
}

pub type Separated0<Data, Separator> = Separated<Data, Separator, false>;
pub type Separated1<Data, Separator> = Separated<Data, Separator, true>;

impl<D: Unparse, S: Unparse, const REQ: bool> Unparse for Separated<D, S, REQ> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (d, s) in &self.pairs {
            d.unparse(f)?;
            s.unparse(f)?;
        }
        if let Some(e) = &self.end {
            e.unparse(f)?;
        }
        Ok(())
    }
}

impl<'i, D: Parsed<&'i str>, S: Parsed<&'i str>, const REQ: bool> Parsed<&'i str>
    for Separated<D, S, REQ>
{
    fn from_parse(input: &'i str) -> nom::IResult<&'i str, Self> {
        if REQ {
            verify(
                many0(D::from_parse.and(S::from_parse)).and(opt(D::from_parse)),
                |(pairs, end)| pairs.len() > 0 || end.is_some(),
            )
            .map(|(pairs, end)| Self { pairs, end })
            .parse(input)
        } else {
            many0(D::from_parse.and(S::from_parse))
                .and(opt(D::from_parse))
                .map(|(pairs, end)| Self { pairs, end })
                .parse(input)
        }
    }
}

/// A sequence of T.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct Many<T, const REQ: bool>(Vec<T>);
/// T*
pub type Many0<T> = Many<T, false>;
/// T+
pub type Many1<T> = Many<T, true>;

impl<I: Clone + nom::InputLength, T: Parsed<I>, const REQ: bool> Parsed<I> for Many<T, REQ> {
    fn from_parse(input: I) -> IResult<I, Self> {
        if REQ {
            nom::multi::many0(T::from_parse).map(Self).parse(input)
        } else {
            nom::multi::many1(T::from_parse).map(Self).parse(input)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Unparse, UnparseDisplay)]
#[unparse({TOKEN.unparse(f)?})]
pub struct Token<const TOKEN: char>;

impl<const C: char> From<Token<C>> for char {
    fn from(_value: Token<C>) -> Self {
        C
    }
}

impl<const C: char> Parsed<&str> for Token<C> {
    fn from_parse(input: &str) -> nom::IResult<&str, Self> {
        value(Self, nchar::char(C)).parse(input)
    }
}

pub mod tag {
    use nom::Parser;

    use crate::de::parse::{Parsed, Unparse};
    use stonemason_proc::{Unparse, UnparseDisplay};

    use nom::combinator::value;

    pub trait Tag {
        fn tag() -> &'static str;
    }

    macro_rules! tag {
        ($i:ident, $tag:literal) => {
            #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
            #[unparse($tag)]
            pub struct $i;

            impl<'i> $crate::de::parse::Parsed<&'i str> for $i {
                fn from_parse(input: &'i str) -> nom::IResult<&'i str, Self> {
                    value(Self, nom::bytes::complete::tag($tag)).parse(input)
                }
            }
        };
        ($($i:ident, $tag:literal);* $(;)?) => {
            $( tag!($i, $tag); )*
        }
    }

    tag!(
        ArrowRight, "->";
        PathSeparator, "::";
    );

    tag!(
        KwMod, "mod";
        KwSelf, "self";
        KwSuper, "super";
        KwStone, "stone";
        KwIn, "in";
        KwPub, "pub";
        KwExtern, "extern";
        KwUse, "use";
    );
}
