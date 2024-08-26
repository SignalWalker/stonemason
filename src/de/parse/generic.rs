use nom::{IResult, Parser};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

use super::{
    Authorial0, AuthorialDelim, Identifier, ParseError, Parsed, Separated, Separated0, Surrounded,
    Token, Type, Unparse,
};

pub enum GenericParamInner {
    Type(),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, Parsed)]
pub enum GenericArgConst {
    #[unparse({todo!()})]
    #[parsed(todo())]
    Block,
    #[unparse({todo!()})]
    #[parsed(todo())]
    Literal,
    #[unparse({todo!()})]
    #[parsed(todo())]
    Path,
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum GenericArg<'data> {
    Type(Type<'data>),
    Const(GenericArgConst),
    Binding {
        id: Identifier<'data>,
        _eq: AuthorialDelim<'data, Token<'='>>,
        ty: Type<'data>,
    },
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
#[unparse(delim('<', '>'))]
#[parsed(delim('<', '>'))]
pub struct GenericArgs<'data> {
    _c1: Authorial0<'data>,
    args: Separated0<(GenericArg<'data>, Authorial0<'data>), Token<','>>,
    _c2: Authorial0<'data>,
}

impl<'data> std::fmt::Debug for GenericArgs<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.unparse(f)
    }
}
