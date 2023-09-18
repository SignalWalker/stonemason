use stonemason_proc::{Unparse, UnparseDisplay};

use super::{
    tag::{KwExtern, KwIn, KwPub, KwSelf, KwStone, KwSuper, KwUse, PathSeparator},
    Authorial0, Authorial1, Identifier, InnerAttribute, Module, OuterAttribute, Parenthesized,
    Separated, SimplePath, Token, Unparse,
};

#[derive(Unparse, UnparseDisplay)]
pub enum Expression<'data> {
    Block(ExpressionWithBlock<'data>),
    NoBlock(ExpressionWithoutBlock<'data>),
}

#[derive(Unparse, UnparseDisplay)]
#[unparse({todo!()})]
pub enum ExpressionWithBlockInner<'data> {
    Block(BlockExpression<'data>),
    UnsafeBlock,
    Loop,
    If,
    IfLet,
    Match,
}

#[derive(Unparse, UnparseDisplay)]
pub struct ExpressionWithBlock<'data> {
    attributes: Separated<OuterAttribute<'data>, Authorial0<'data>>,
    _c1: Authorial0<'data>,
    inner: ExpressionWithBlockInner<'data>,
}

#[derive(Unparse, UnparseDisplay)]
#[unparse({todo!()})]
pub enum ExpressionWithoutBlockInner<'data> {
    Literal(&'data str),
    Path,
    Operator,
    Grouped,
    Array,
    Await,
    Index,
    Tuple,
    TupleIndexing,
    Struct,
    Call,
    MethodCall,
    Field,
    Closure,
    AsyncBlock,
    Continue,
    Break,
    Range,
    Return,
    Underscore,
    MacroInvocation,
}

#[derive(Unparse, UnparseDisplay)]
pub struct ExpressionWithoutBlock<'data> {
    attributes: Separated<OuterAttribute<'data>, Authorial0<'data>>,
    _c1: Authorial0<'data>,
    inner: ExpressionWithoutBlockInner<'data>,
}

#[derive(Unparse, UnparseDisplay)]
#[unparse(delim('{', '}'))]
pub struct BlockExpression<'data> {
    _c1: Authorial0<'data>,
    attributes: Separated<InnerAttribute<'data>, Authorial0<'data>>,
    statements: &'data str,
    _c2: Authorial0<'data>,
}

#[derive(Unparse, UnparseDisplay)]
pub enum Statement {
    #[unparse(';')]
    Empty,
    #[unparse({todo!()})]
    Item,
    #[unparse({todo!()})]
    Let,
    #[unparse({todo!()})]
    Expression,
    #[unparse({todo!()})]
    MacroInvocation,
}

#[derive(Unparse, UnparseDisplay)]
pub struct Item<'data> {
    attributes: Separated<OuterAttribute<'data>, Authorial0<'data>>,
    inner: ItemInner<'data>,
}

#[derive(Unparse, UnparseDisplay)]
pub enum Visibility<'data> {
    #[unparse("pub")]
    Pub,
    #[unparse(prefix("pub"))]
    PubStone(
        Authorial0<'data>,
        Parenthesized<(Authorial0<'data>, KwStone, Authorial0<'data>)>,
    ),
    #[unparse(prefix("pub"))]
    PubSelf(
        Authorial0<'data>,
        Parenthesized<(Authorial0<'data>, KwSelf, Authorial0<'data>)>,
    ),
    #[unparse(prefix("pub"))]
    PubSuper(
        Authorial0<'data>,
        Parenthesized<(Authorial0<'data>, KwSuper, Authorial0<'data>)>,
    ),
    #[unparse(prefix("pub"))]
    PubIn {
        _c1: Authorial0<'data>,
        _p1: Token<'('>,
        _in: KwIn,
        _c2: Authorial0<'data>,
        path: SimplePath<'data>,
        _c3: Authorial0<'data>,
        _p2: Token<')'>,
    },
}

#[derive(Unparse, UnparseDisplay)]
#[unparse({todo!()})]
pub enum ItemInner<'data> {
    Vis(Visibility<'data>, VisItemInner<'data>),
    Macro,
}

#[derive(Unparse, UnparseDisplay)]
pub enum StoneRef<'data> {
    Identifier(Identifier<'data>),
    #[unparse("self")]
    SelfRef,
}

#[derive(Unparse, UnparseDisplay)]
pub enum AsClauseInner<'data> {
    Identifier(Identifier<'data>),
    #[unparse('_')]
    Infer,
}

#[derive(Unparse, UnparseDisplay)]
#[unparse(prefix("as"))]
pub struct AsClause<'data> {
    _c1: Authorial0<'data>,
    inner: AsClauseInner<'data>,
}

#[derive(Unparse, UnparseDisplay)]
pub struct ExternStone<'data> {
    _extern: KwExtern,
    _c1: Authorial0<'data>,
    _stone: KwStone,
    _c2: Authorial0<'data>,
    ref_: StoneRef<'data>,
    _c3: Authorial0<'data>,
    as_: Option<AsClause<'data>>,
    _c4: Authorial0<'data>,
    _semi: Token<';'>,
}

#[derive(Unparse, UnparseDisplay)]
pub struct UseDeclaration<'data> {
    _use: KwUse,
    _c1: Authorial0<'data>,
    inner: UseTree<'data>,
    _c2: Authorial0<'data>,
    _semi: Token<';'>,
}

#[derive(Unparse, UnparseDisplay)]
pub enum UseTree<'data> {
    Simple {
        path: SimplePath<'data>,
        as_: Option<(Authorial0<'data>, AsClause<'data>)>,
    },
    All {
        inner: Option<(
            Option<SimplePath<'data>>,
            Authorial0<'data>,
            PathSeparator,
            Authorial0<'data>,
        )>,
        _asterisk: Token<'*'>,
    },
    Tree {
        path: Option<(
            Option<SimplePath<'data>>,
            Authorial0<'data>,
            PathSeparator,
            Authorial0<'data>,
        )>,
        _b1: Token<'{'>,
        _c1: Authorial0<'data>,
        trees: Box<Separated<UseTree<'data>, Token<','>>>,
        _c2: Authorial0<'data>,
        _b2: Token<'}'>,
    },
}

#[derive(Unparse, UnparseDisplay)]
#[unparse({todo!()})]
pub enum VisItemInner<'data> {
    Module(Box<Module<'data>>),
    ExternStone(ExternStone<'data>),
    Use(UseDeclaration<'data>),
    Function,
    TypeAlias,
    Struct,
    Enumeration,
    Union,
    ConstantItem,
    StaticItem,
    Trait,
    Implementation,
    ExternBlock,
}
