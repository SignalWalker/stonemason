use super::{AbiSpecifier, BlockExpression};
use crate::de::parse::{
    tag::{
        ArrowRight, Ellipses, KwAsync, KwConst, KwExtern, KwFn, KwFor, KwMut, KwSelf, KwUnsafe,
        KwWhere,
    },
    Authorial0, Authorial1, AuthorialDelim, Either, GenericArgs, Identifier, IdentifierOrKeyword,
    Many0, OuterAttribute, Parenthesized, ParseError, Parsed, PatternNoTopAlt, Separated,
    Separated0, Separated1, Token, Type, TypePath, Unparse,
};
use stonemason_proc::{Parsed, Unparse, UnparseDebug, UnparseDisplay};

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct FunctionQualifiers<'data> {
    const_: Option<(KwConst, Authorial0<'data>)>,
    async_: Option<(KwAsync, Authorial0<'data>)>,
    unsafe_: Option<(KwUnsafe, Authorial0<'data>)>,
    extern_: Option<(KwExtern, Authorial0<'data>, Option<AbiSpecifier<'data>>)>,
}

impl<'data> std::fmt::Debug for FunctionQualifiers<'data> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.unparse(f)
    }
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum FunctionInner<'data> {
    #[unparse(';')]
    #[parsed(';')]
    Empty,
    Block(BlockExpression<'data>),
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
#[unparse(prefix('\''))]
pub enum LifetimeToken<'data> {
    #[unparse('_')]
    #[parsed('_')]
    Anonymous,
    Id(IdentifierOrKeyword<&'data str>),
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
#[unparse(prefix('\''))]
#[parsed(prefix('\''))]
pub struct LifetimeOrLabel<'data>(Identifier<'data>);

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum Lifetime<'data> {
    #[unparse("'_")]
    #[parsed("'_")]
    Anonymous,
    #[unparse("'static")]
    #[parsed("'static")]
    Static,
    Label(LifetimeOrLabel<'data>),
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum ShorthandSelfRef<'data> {
    #[unparse(prefix('&'))]
    #[parsed(prefix('&'))]
    Explicit(Lifetime<'data>),
    #[unparse('&')]
    #[parsed('&')]
    Inferred,
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct ShorthandSelf<'data> {
    ref_: Option<ShorthandSelfRef<'data>>,
    _c1: Authorial0<'data>,
    mut_: Option<(KwMut, Authorial1<'data>)>,
    _self: KwSelf,
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum SelfParamInner<'data> {
    Shorthand(ShorthandSelf<'data>),
    Typed(
        Option<(KwMut, Authorial1<'data>)>,
        KwSelf,
        Authorial0<'data>,
        Token<':'>,
        Authorial0<'data>,
        Type<'data>,
    ),
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct SelfParam<'data> {
    attrs: Many0<(OuterAttribute<'data>, Authorial0<'data>)>,
    inner: SelfParamInner<'data>,
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct FunctionParamPattern<'data> {
    pat: PatternNoTopAlt<'data>,
    _c: AuthorialDelim<'data, Token<':'>>,
    ty: Either<Ellipses, Type<'data>>,
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum FunctionParamInner<'data> {
    #[unparse("...")]
    #[parsed("...")]
    Variadic,
    Pattern(FunctionParamPattern<'data>),
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct FunctionParam<'data> {
    attrs: Many0<(OuterAttribute<'data>, Authorial0<'data>)>,
    inner: FunctionParamInner<'data>,
}

#[derive(Clone, Unparse, UnparseDisplay, UnparseDebug, Parsed)]
pub enum FunctionParameters<'data> {
    SelfOnly(SelfParam<'data>, Option<(Authorial0<'data>, Token<','>)>),
    Multi {
        self_: Option<(SelfParam<'data>, Authorial0<'data>, Token<','>)>,
        parameters: Separated1<FunctionParam<'data>, Token<','>>,
    },
}

#[derive(Clone, Unparse, UnparseDisplay, UnparseDebug, Parsed)]
pub struct FunctionReturnType<'data> {
    _arrow: ArrowRight,
    _c1: Authorial0<'data>,
    ty: Type<'data>,
}

pub type LifetimeBounds<'data> = Separated0<Lifetime<'data>, Token<'+'>>;

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct LifetimeWhereClauseItem<'data> {
    lifetime: Lifetime<'data>,
    _c1: Authorial0<'data>,
    _colon: Token<':'>,
    _c2: Authorial0<'data>,
    bounds: LifetimeBounds<'data>,
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct ForLifetimes<'data> {
    _for: KwFor,
    params: GenericArgs<'data>,
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct TraitBoundInner<'data> {
    maybe: Option<Token<'?'>>,
    for_clause: Option<(ForLifetimes<'data>, Authorial1<'data>)>,
    type_path: TypePath<'data>,
}

pub type TraitBound<'data> = Either<
    TraitBoundInner<'data>,
    Parenthesized<(Authorial0<'data>, TraitBoundInner<'data>, Authorial0<'data>)>,
>;

pub type TypeParamBound<'data> = Either<Lifetime<'data>, TraitBound<'data>>;

pub type TypeParamBounds<'data> = Separated1<TypeParamBound<'data>, Token<'+'>>;

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct TypeBoundsWhereClauseItem<'data> {
    for_clause: Option<(ForLifetimes<'data>, Authorial1<'data>)>,
    ty: Type<'data>,
    _c: AuthorialDelim<'data, Token<':'>>,
    bounds: TypeParamBounds<'data>,
}

pub type WhereClauseItem<'data> =
    Either<LifetimeWhereClauseItem<'data>, TypeBoundsWhereClauseItem<'data>>;

#[derive(Clone, Unparse, UnparseDisplay, UnparseDebug, Parsed)]
pub struct WhereClause<'data> {
    _where: KwWhere,
    clauses: Separated0<WhereClauseItem<'data>, Token<','>>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct Function<'data> {
    qualifiers: FunctionQualifiers<'data>,
    _fn: (KwFn, Authorial1<'data>),
    id: Identifier<'data>,
    _c2: Authorial0<'data>,
    generics: Option<(GenericArgs<'data>, Authorial0<'data>)>,
    params: Parenthesized<(
        Authorial0<'data>,
        Option<(FunctionParameters<'data>, Authorial0<'data>)>,
    )>,
    _c4: Authorial0<'data>,
    return_type: Option<(FunctionReturnType<'data>, Authorial0<'data>)>,
    where_clause: Option<(WhereClause<'data>, Authorial0<'data>)>,
    inner: FunctionInner<'data>,
}
