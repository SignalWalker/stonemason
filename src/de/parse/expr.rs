use super::{
    tag::{
        KwAs, KwElse, KwExtern, KwIn, KwLet, KwMut, KwPub, KwReturn, KwSelf, KwStatic, KwStone,
        KwSuper, KwUse, PathSeparator,
    },
    Authorial0, Authorial1, AuthorialDelim, AuthorialParen, ByteLiteral, ByteStringLiteral,
    CharLiteral, CurlyBraced, Either, FloatLiteral, Identifier, InnerAttribute, IntegerLiteral,
    Many0, Module, OuterAttribute, Parenthesized, ParseError, Parsed, PathInExpression,
    PatternNoTopAlt, QualifiedPathInExpression, RawByteStringLiteral, RawStringLiteral, Separated,
    Separated0, SimplePath, StringLiteral, Token, Type, Unparse,
};
use nom::{bytes::complete::tag, combinator::value, Parser};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

mod function;
pub use function::*;

pub type Expression<'data> = Either<ExpressionWithBlock<'data>, ExpressionWithoutBlock<'data>>;

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum ExpressionWithBlockInner<'data> {
    Block(BlockExpression<'data>),
    // UnsafeBlock,
    // Loop,
    // If,
    // IfLet,
    // Match,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct ExpressionWithBlock<'data> {
    attributes: Many0<(OuterAttribute<'data>, Authorial0<'data>)>,
    inner: ExpressionWithBlockInner<'data>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum LiteralExpression<'data> {
    Char(CharLiteral<'data>),
    String(StringLiteral<'data>),
    RawString(RawStringLiteral<'data>),
    Byte(ByteLiteral<'data>),
    ByteString(ByteStringLiteral<'data>),
    RawByteString(RawByteStringLiteral<'data>),
    Int(IntegerLiteral<'data>),
    Float(FloatLiteral<'data>),
    Bool(bool),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum ExpressionWithoutBlockInner<'data> {
    Literal(LiteralExpression<'data>),
    Return {
        _ret: (KwReturn, Authorial0<'data>),
        #[parsed(cut())]
        expr: Option<Box<Expression<'data>>>,
    },
    Path(Either<PathInExpression<'data>, QualifiedPathInExpression<'data>>),
    // Operator,
    // Grouped(AuthorialParen<'data, Box<Expression<'data>>>),
    // Array,
    // Await,
    // Index,
    // Tuple,
    // TupleIndexing,
    // Struct,
    Call {
        func: Identifier<'data>,
        _p_open: (Authorial0<'data>, Token<'('>, Authorial0<'data>),
        // params: Separated0<(Box<Expression<'data>>, Authorial0<'data>), Token<','>>,
        _p_close: (Authorial0<'data>, Token<')'>),
    },
    // MethodCall,
    // Field,
    // Closure,
    // AsyncBlock,
    // Continue,
    // Break,
    // Range,
    // Underscore,
    // MacroInvocation,
}

// impl<'data> std::fmt::Debug for ExpressionWithoutBlockInner<'data> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.unparse(f)
//     }
// }

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct ExpressionWithoutBlock<'data> {
    attributes: Many0<(OuterAttribute<'data>, Authorial0<'data>)>,
    inner: ExpressionWithoutBlockInner<'data>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct BlockExpression<'data> {
    _open: (Token<'{'>, Authorial0<'data>),
    attributes: Many0<(InnerAttribute<'data>, Authorial0<'data>)>,
    statements: Many0<(Statement<'data>, Authorial0<'data>)>,
    _close: Token<'}'>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum StatementExpr<'data> {
    Block(
        ExpressionWithoutBlock<'data>,
        (Authorial0<'data>, Token<';'>),
    ),
    NoBlock(
        ExpressionWithBlock<'data>,
        Option<(Authorial0<'data>, Token<';'>)>,
    ),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum Statement<'data> {
    #[unparse(';')]
    #[parsed(';')]
    Empty,
    Item(
        Item<'data>,
        #[parsed(cut())] (Authorial0<'data>, Token<';'>),
    ),
    Let {
        attrs: Many0<(OuterAttribute<'data>, Authorial0<'data>)>,
        _let: (KwLet, Authorial1<'data>),
        #[parsed(cut())]
        pattern: PatternNoTopAlt<'data>,
        #[parsed(cut())]
        ty: Option<(
            AuthorialDelim<'data, Token<':'>>,
            Type<'data>,
            Authorial0<'data>,
        )>,
        #[parsed(cut())]
        expr: Option<(
            AuthorialDelim<'data, Token<'='>>,
            Expression<'data>,
            Authorial0<'data>,
            Option<(
                AuthorialDelim<'data, KwElse>,
                BlockExpression<'data>,
                Authorial0<'data>,
            )>,
        )>,
        #[parsed(cut())]
        _semi: Token<';'>,
    },
    Expression(StatementExpr<'data>),
    // MacroInvocation,
}

// impl<'data> std::fmt::Debug for Statement<'data> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.unparse(f)?;
//         match self {
//             Self::Item(..) => write!(f, " // Statement::Item"),
//             Self::Let { .. } => write!(f, " // Statement::Let"),
//             Self::Expression(..) => write!(f, " // Statement::Expression"),
//             _ => Ok(()),
//         }
//     }
// }

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct Item<'data> {
    attributes: Many0<(OuterAttribute<'data>, Authorial0<'data>)>,
    inner: ItemInner<'data>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum Visibility<'data> {
    #[unparse("pub")]
    #[parsed("pub")]
    Pub,
    #[unparse(prefix("pub"))]
    #[parsed(prefix("pub"))]
    PubStone(Authorial0<'data>, AuthorialParen<'data, KwStone>),
    #[unparse(prefix("pub"))]
    #[parsed(prefix("pub"))]
    PubSelf(Authorial0<'data>, AuthorialParen<'data, KwSelf>),
    #[unparse(prefix("pub"))]
    #[parsed(prefix("pub"))]
    PubSuper(Authorial0<'data>, AuthorialParen<'data, KwSuper>),
    #[unparse(prefix("pub"))]
    #[parsed(prefix("pub"))]
    PubIn {
        _c1: Authorial0<'data>,
        inner: AuthorialParen<'data, (KwIn, Authorial1<'data>, SimplePath<'data>)>,
    },
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum ItemInner<'data> {
    Vis(
        Option<(Visibility<'data>, Authorial1<'data>)>,
        VisItemInner<'data>,
    ),
    // #[unparse({todo!()})]
    // #[parsed(todo())]
    // Macro,
}
impl std::fmt::Debug for ItemInner<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vis(vis, inner) => {
                if let Some((vis, _)) = vis.as_ref() {
                    write!(f, "{vis} ")?;
                }
                std::fmt::Debug::fmt(inner, f)
            } // Self::Macro => todo!(),
        }
    }
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum StoneRef<'data> {
    Identifier(Identifier<'data>),
    #[unparse("self")]
    #[parsed("self")]
    SelfRef,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum AsClauseInner<'data> {
    Identifier(Identifier<'data>),
    #[unparse('_')]
    #[parsed('_')]
    Infer,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct AsClause<'data> {
    _as: (KwAs, Authorial1<'data>),
    inner: AsClauseInner<'data>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct ExternStone<'data> {
    _extern: (KwExtern, Authorial1<'data>, KwStone, Authorial1<'data>),
    ref_: StoneRef<'data>,
    _c3: Authorial1<'data>,
    as_: Option<(AsClause<'data>, Authorial0<'data>)>,
    _semi: Token<';'>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub struct UseDeclaration<'data> {
    _use: (KwUse, Authorial1<'data>),
    #[parsed(cut())]
    inner: UseTree<'data>,
    #[parsed(cut())]
    _semi: (Authorial0<'data>, Token<';'>),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
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
        trees: CurlyBraced<AuthorialDelim<'data, Separated0<Box<Self>, Token<','>>>>,
    },
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum AbiSpecifier<'data> {
    Literal(StringLiteral<'data>),
    RawLiteral(RawStringLiteral<'data>),
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct StaticItem<'data> {
    _static: (KwStatic, Authorial1<'data>),
    mut_: Option<(KwMut, Authorial1<'data>)>,
    id: Identifier<'data>,
    _colon: AuthorialDelim<'data, Token<':'>>,
    ty: Type<'data>,
    assignment: Option<(AuthorialDelim<'data, Token<'='>>, Expression<'data>)>,
    _semi: (Authorial0<'data>, Token<';'>),
}

#[derive(Clone, Unparse, UnparseDisplay)]
pub enum ExternVisItemInner<'data> {
    Static(StaticItem<'data>),
    Function(Function<'data>),
}

#[derive(Clone, Unparse, UnparseDisplay)]
pub struct ExternVisItem<'data> {
    vis: Option<(Visibility<'data>, Authorial1<'data>)>,
    inner: ExternVisItemInner<'data>,
}

#[derive(Clone, Unparse, UnparseDisplay)]
pub enum ExternItemInner<'data> {
    #[unparse({todo!()})]
    MacroInvocationSemi,
    VisItem(ExternVisItem<'data>),
}

#[derive(Clone, Unparse, UnparseDisplay)]
pub struct ExternItem<'data> {
    attrs: Many0<(OuterAttribute<'data>, Authorial0<'data>)>,
    inner: ExternItemInner<'data>,
}

#[derive(Clone, Unparse, UnparseDisplay)]
pub struct ExternBlockInner<'data> {
    _c1: Authorial0<'data>,
    attrs: Many0<(InnerAttribute<'data>, Authorial0<'data>)>,
    items: Many0<(ExternItem<'data>, Authorial0<'data>)>,
}

#[derive(Clone, Unparse, UnparseDisplay)]
pub struct ExternBlock<'data> {
    _extern: KwExtern,
    abi: AbiSpecifier<'data>,
    #[unparse(delim('{', _, '}'))]
    inner: ExternBlockInner<'data>,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum VisItemInner<'data> {
    Module(Box<Module<'data>>),
    Function(Function<'data>),
    ExternStone(ExternStone<'data>),
    Use(UseDeclaration<'data>),
    // TypeAlias,
    // Struct,
    // Enumeration,
    // Union,
    // ConstantItem,
    // StaticItem,
    // Trait,
    // Implementation,
    // ExternBlock,
}
