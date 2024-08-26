use crate::de::parse::{
    tag::{ArrowRight, KwMacroStone, KwSelf, KwSelfType, KwStone, KwSuper, PathSeparator},
    Authorial0, AuthorialDelim, GenericArgs, Identifier, Separated, Separated0, Token, Type,
};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay, Parsed)]
pub enum PathIdentSegment<'data> {
    Super(KwSuper),
    Self_(KwSelf),
    SelfType(KwSelfType),
    Stone(KwStone),
    MacroStone(KwMacroStone),
    Id(Identifier<'data>),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct TypePathFnOutput<'data> {
    _arrow: (ArrowRight, Authorial0<'data>),
    ty: Type<'data>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct TypePathFn<'data> {
    _open: (Token<'('>, Authorial0<'data>),
    inputs: Separated0<Type<'data>, (Authorial0<'data>, Token<','>)>,
    _close: (Authorial0<'data>, Token<')'>),
    output: Option<TypePathFnOutput<'data>>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum TypePathSegmentInner<'data> {
    Generic(GenericArgs<'data>),
    Fn(TypePathFn<'data>),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct TypePathSegment<'data> {
    id: PathIdentSegment<'data>,
    separator: Option<PathSeparator>,
    inner: Option<TypePathSegmentInner<'data>>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct TypePath<'data> {
    root: Option<(PathSeparator, Authorial0<'data>)>,
    segments: Separated<AuthorialDelim<'data, TypePathSegment<'data>>, PathSeparator, true>,
}
