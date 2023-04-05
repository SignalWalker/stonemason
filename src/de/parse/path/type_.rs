use stonemason_proc::{Unparse, UnparseDisplay};

use crate::de::parse::{AuthorialDelim, GenericArgs, Identifier, Type, Unparse};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum PathIdentSegment<'data> {
    Id(Identifier<'data>),
    #[unparse("super")]
    Super,
    #[unparse("self")]
    Self_,
    #[unparse("Self")]
    SelfType,
    #[unparse("stone")]
    Stone,
    #[unparse("$stone")]
    MacroStone,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, UnparseDisplay)]
pub struct TypePathFn<'data> {
    inputs: Vec<AuthorialDelim<'data, Type<'data>>>,
}

impl<'data> Unparse for TypePathFn<'data> {
    fn unparse(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum TypePathSegmentInner<'data> {
    Generic(GenericArgs<'data>),
    Fn(TypePathFn<'data>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TypePathSegment<'data> {
    id: PathIdentSegment<'data>,
    #[unparse({if self.separator { "::".unparse(f)?; }})]
    separator: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TypePath {}
