use nom::{IResult, Parser};
use stonemason_proc::{Unparse, UnparseDisplay};

use super::{Authorial0, AuthorialDelim, Identifier, Parsed, Separated, Token, Type, Unparse};

pub enum GenericParamInner {
    Type(),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum GenericArgConst<'data> {
    #[unparse({todo!()})]
    Block,
    Literal(&'data Option<Token<'-'>>),
    #[unparse({todo!()})]
    Path,
}

impl<'data> Parsed<&'data str> for GenericArgConst<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        todo!()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum GenericArg<'data> {
    Type(Type<'data>),
    Const(GenericArgConst<'data>),
    Binding {
        id: Identifier<'data>,
        _c1: Authorial0<'data>,
        _eq: Token<'='>,
        _c2: Authorial0<'data>,
        ty: Type<'data>,
    },
}

impl<'data> Parsed<&'data str> for GenericArg<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        todo!()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
#[unparse(delim('<', '>'))]
pub struct GenericArgs<'data> {
    _c1: Authorial0<'data>,
    args: Separated<AuthorialDelim<'data, GenericArg<'data>>, Token<','>>,
    _c2: Authorial0<'data>,
}

impl<'data> Parsed<&'data str> for GenericArgs<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        Token::<'<'>::from_parse
            .and(Authorial0::from_parse)
            .and(Separated::from_parse)
            .and(Authorial0::from_parse)
            .and(Token::<'>'>::from_parse)
            .map(|((((_b1, _c1), args), _c2), _b2)| Self { _c1, args, _c2 })
            .parse(input)
    }
}
