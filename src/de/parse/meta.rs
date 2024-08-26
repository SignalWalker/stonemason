use std::marker::PhantomData;

use nom::{
    combinator::{opt, value, verify},
    multi::{many0, many1},
    IResult, Parser,
};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

use nom::character::complete as nchar;

use super::{Authorial0, Authorial1, ParseError, Parsed, Unparse};

pub type Delimited<A, Data, B> = (A, Data, B);
pub type Surrounded<Surround, Data> = Delimited<Surround, Data, Surround>;

pub type Parenthesized<Data> = Delimited<Token<'('>, Data, Token<')'>>;
pub type CurlyBraced<Data> = Delimited<Token<'{'>, Data, Token<'}'>>;
pub type SquareBracketed<Data> = Delimited<Token<'['>, Data, Token<']'>>;
pub type AngleBracketed<Data> = Delimited<Token<'<'>, Data, Token<'>'>>;

/// A sequence of `Data` separated by `Separator`, with an optional terminal `Data`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Separated<Data, Separator, const REQ: bool> {
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
    fn from_parse<Error: ParseError<&'i str>>(
        input: &'i str,
    ) -> nom::IResult<&'i str, Self, Error> {
        if REQ {
            verify(
                many0(D::from_parse.and(S::from_parse)).and(Option::<D>::from_parse),
                |(pairs, end)| !pairs.is_empty() || end.is_some(),
            )
            .map(|(pairs, end)| Self { pairs, end })
            .parse(input)
        } else {
            many0(D::from_parse.and(S::from_parse))
                .and(Option::<D>::from_parse)
                .map(|(pairs, end)| Self { pairs, end })
                .parse(input)
        }
    }
}

// #[derive(Debug, Clone, Eq, PartialEq, Hash)]
// pub struct Interspersed<Data, Separator, const REQ: bool>(Vec<Data>, PhantomData<Separator>);
// pub type Interspersed0<Data, Separator> = Interspersed<Data, Separator, false>;
// pub type Interspersed1<Data, Separator> = Interspersed<Data, Separator, true>;
//
// impl<D: Unparse, S: Unparse + Default, const REQ: bool> Unparse for Interspersed<D, S, REQ> {
//     fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let s = S::default();
//         for d in self.0 {
//             d.unparse(f)?;
//             s.unparse(f)?;
//         }
//     }
// }

/// A sequence of T.
#[derive(Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct Many<T, const REQ: bool>(pub Vec<T>);
/// T*
pub type Many0<T> = Many<T, false>;
/// T+
pub type Many1<T> = Many<T, true>;

impl<T: std::fmt::Debug, const REQ: bool> std::fmt::Debug for Many<T, REQ> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl<I: Clone + nom::InputLength, T: Parsed<I>, const REQ: bool> Parsed<I> for Many<T, REQ> {
    fn from_parse<Error: ParseError<I>>(input: I) -> nom::IResult<I, Self, Error> {
        match REQ {
            true => nom::error::context("Many1", nom::multi::many1(T::from_parse).map(Self))
                .parse(input),
            false => nom::error::context("Many0", nom::multi::many0(T::from_parse).map(Self))
                .parse(input),
        }
    }
}

impl<T, const REQ: bool> Default for Many<T, REQ> {
    fn default() -> Self {
        Self(Vec::default())
    }
}

pub fn eof_map<Input: nom::InputLength + Clone, Output, Error: ParseError<Input>>(
    m: impl Fn() -> Output,
) -> impl Parser<Input, Output, Error> {
    nom::combinator::map::<Input, _, Output, Error, _, _>(nom::combinator::eof, move |_| {
        tracing::trace!("reached end of file");
        m()
    })
}

/// A single character.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Unparse, UnparseDisplay)]
#[unparse({TOKEN.unparse(f)?})]
pub struct Token<const TOKEN: char>;

impl<const C: char> From<Token<C>> for char {
    fn from(_value: Token<C>) -> Self {
        C
    }
}

impl<'data, const C: char> Parsed<&'data str> for Token<C> {
    fn from_parse<Error: ParseError<&'data str>>(
        input: &'data str,
    ) -> nom::IResult<&'data str, Self, Error> {
        nom::error::context("Token", value(Self, nchar::char(C))).parse(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Unparse, UnparseDisplay, Parsed)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

// macro_rules! any_of {
//     ($A:ty, $($T:ty),+) => (
//         $crate::de::parse::Either<A, $(crate::de::parse::Either)>
//     )
// }

pub mod tag {
    use crate::de::parse::Unparse;
    use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

    pub trait Tag {
        fn tag() -> &'static str;
    }

    macro_rules! tag {
        ($i:ident, $tag:literal) => {
            #[derive(Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, Parsed)]
            #[unparse($tag)]
            #[parsed({nom::combinator::value(Self, nom::bytes::complete::tag($tag))})]
            pub struct $i;


            impl std::fmt::Debug for $i {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, $tag)
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
        Ellipses, "...";
        RawTag, "r#";
    );

    tag!(
        InnerAttrOpen, "#![";
        OuterAttrOpen, "#[";
        AttrClose, "]";
    );

    tag!(
        RangeInclusive, "..=";
        RangeExclusive, "..";
    );

    tag!(
        KwMod, "mod";
        KwSelf, "self";
        KwSelfType, "Self";
        KwSuper, "super";
        KwStone, "stone";
        KwMacroStone, "$stone";
        KwIn, "in";
        KwPub, "pub";
        KwExtern, "extern";
        KwUse, "use";
        KwAs, "as";
    );

    tag!(
        KwRef, "ref";
        KwMut, "mut";
        KwStatic, "static";
        KwConst, "const";
        KwAsync, "async";
        KwUnsafe, "unsafe";
        KwWhere, "where";
        KwFor, "for";
        KwFn, "fn";
        KwLet, "let";
        KwElse, "else";
        KwReturn, "return";
    );
}
