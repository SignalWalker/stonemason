use nom::{combinator::opt, sequence::delimited, IResult, Parser};
use stonemason_proc::{Unparse, UnparseDisplay};

use super::{AuthorialDelim, AuthorialPre, Parenthesized, Parsed, Token, TypePath, Unparse};

use nom::character::complete as nchar;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum Type<'data> {
    Unbound(TypeUnbound<'data>),
    #[unparse({todo!()})]
    ImplTrait,
    #[unparse({todo!()})]
    TraitObject,
}

impl<'data> Parsed<&'data str> for Type<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        todo!()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum TypeUnbound<'data> {
    #[unparse(delim('(', ')'))]
    Parenthesized(AuthorialDelim<'data, Box<Type<'data>>>),
    #[unparse(prefix("impl"))]
    ImplTraitOneBound(AuthorialPre<'data, TraitBound<'data>, true>),
    #[unparse({todo!()})]
    TraitObjectOneBound,
    #[unparse({todo!()})]
    TypePath,
    #[unparse({todo!()})]
    Tuple,
    #[unparse('!')]
    Never,
    #[unparse({todo!()})]
    RawPointer,
    #[unparse({todo!()})]
    Reference,
    #[unparse({todo!()})]
    Array,
    #[unparse({todo!()})]
    Slice,
    #[unparse({todo!()})]
    Inferred,
    #[unparse({todo!()})]
    QualifiedPathInType,
    #[unparse({todo!()})]
    BareFunction,
    #[unparse({todo!()})]
    MacroInvocation,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TraitBoundInner<'data> {
    not: Option<Token<'?'>>,
    path: Box<TypePath<'data>>,
}

impl<'data> Parsed<&'data str> for TraitBoundInner<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        Option::from_parse
            .and(TypePath::from_parse)
            .map(|(not, path)| Self {
                not,
                path: Box::new(path),
            })
            .parse(input)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum TraitBound<'data> {
    NoParen(TraitBoundInner<'data>),
    Paren(Parenthesized<AuthorialDelim<'data, TraitBoundInner<'data>>>),
}

impl<'data> From<TraitBoundInner<'data>> for TraitBound<'data> {
    fn from(value: TraitBoundInner<'data>) -> Self {
        Self::NoParen(value)
    }
}

impl<'data> From<Parenthesized<AuthorialDelim<'data, TraitBoundInner<'data>>>>
    for TraitBound<'data>
{
    fn from(value: Parenthesized<AuthorialDelim<'data, TraitBoundInner<'data>>>) -> Self {
        Self::Paren(value)
    }
}

impl<'data> Parsed<&'data str> for TraitBound<'data> {
    fn from_parse(input: &'data str) -> IResult<&'data str, Self> {
        Parser::into(TraitBoundInner::from_parse)
            .or(Parser::into(Parenthesized::from_parse))
            .parse(input)
    }
}
