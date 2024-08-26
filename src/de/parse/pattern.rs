use super::{
    tag::{KwMut, KwRef, RangeExclusive, RangeInclusive},
    Authorial0, AuthorialDelim, CharLiteral, Either, Identifier, Token, Unparse,
};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum LiteralPattern<'data> {
    Boolean(bool),
    Char(CharLiteral<'data>),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum PatternWithoutRange<'data> {
    LiteralPattern(LiteralPattern<'data>),
    IdentifierPattern {
        ref_: Option<(KwRef, Authorial0<'data>)>,
        mut_: Option<(KwMut, Authorial0<'data>)>,
        id: Identifier<'data>,
        pat: Option<(
            AuthorialDelim<'data, Token<'@'>>,
            Box<PatternNoTopAlt<'data>>,
        )>,
    },
    // WildcardPattern,
    // RestPattern,
    // ReferencePattern,
    // StructPattern,
    // TupleStructPattern,
    // TuplePattern,
    // GroupedPattern,
    // SlicePattern,
    // PathPattern,
    // MacroInvocation,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum RangePatternBound<'data> {
    Char(CharLiteral<'data>),
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum RangePattern<'data> {
    Inclusive(
        RangePatternBound<'data>,
        RangeInclusive,
        RangePatternBound<'data>,
    ),
    From(RangePatternBound<'data>, RangeExclusive),
    To(RangeInclusive, RangePatternBound<'data>),
}

pub type PatternNoTopAlt<'data> = Either<PatternWithoutRange<'data>, RangePattern<'data>>;
