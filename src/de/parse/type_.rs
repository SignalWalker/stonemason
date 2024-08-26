use super::{AuthorialParen, Token, Unparse};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum Type<'data> {
    Unbound(TypeUnbound<'data>),
    // ImplTrait,
    // TraitObject,
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum TypeUnbound<'data> {
    Parenthesized(AuthorialParen<'data, Box<Type<'data>>>),
    // #[unparse(prefix("impl"))]
    // ImplTraitOneBound(AuthorialPre<'data, Box<TraitBound<'data>>, true>),
    // TraitObjectOneBound,
    // TypePath,
    // Tuple,
    Never(Token<'!'>),
    // RawPointer,
    // Reference,
    // Array,
    // Slice,
    // Inferred,
    // QualifiedPathInType,
    // BareFunction,
    // MacroInvocation,
}

// #[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
// pub struct TraitBoundInner<'data> {
//     not: Option<Token<'?'>>,
//     path: Box<TypePath<'data>>,
// }
//
// impl<'data> Parsed<&'data str> for TraitBoundInner<'data> {
//     fn from_parse<Error: ParseError<&'data str>>(
//         input: &'data str,
//     ) -> nom::IResult<&'data str, Self, Error> {
//         Option::from_parse
//             .and(TypePath::from_parse)
//             .map(|(not, path)| Self {
//                 not,
//                 path: Box::new(path),
//             })
//             .parse(input)
//     }
// }
//
// #[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
// pub enum TraitBound<'data> {
//     NoParen(TraitBoundInner<'data>),
//     Paren(Parenthesized<AuthorialDelim<'data, TraitBoundInner<'data>>>),
// }
//
// impl<'data> From<TraitBoundInner<'data>> for TraitBound<'data> {
//     fn from(value: TraitBoundInner<'data>) -> Self {
//         Self::NoParen(value)
//     }
// }
//
// impl<'data> From<Parenthesized<AuthorialDelim<'data, TraitBoundInner<'data>>>>
//     for TraitBound<'data>
// {
//     fn from(value: Parenthesized<AuthorialDelim<'data, TraitBoundInner<'data>>>) -> Self {
//         Self::Paren(value)
//     }
// }
//
// impl<'data> Parsed<&'data str> for TraitBound<'data> {
//     fn from_parse<Error: ParseError<&'data str>>(
//         input: &'data str,
//     ) -> nom::IResult<&'data str, Self, Error> {
//         Parser::into(TraitBoundInner::from_parse)
//             .or(Parser::into(Parenthesized::from_parse))
//             .parse(input)
//     }
// }
