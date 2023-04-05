use stonemason_proc::{Unparse, UnparseDisplay};

use super::{AuthorialDelim, AuthorialPre, TypePath, Unparse};

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum Type<'data> {
    Unbound(TypeUnbound<'data>),
    #[unparse({todo!()})]
    ImplTrait,
    #[unparse({todo!()})]
    TraitObject,
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
    #[unparse({todo!()})]
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

// impl<'data> Display for TypeUnbound<'data> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             TypeUnbound::Parenthesized(t) => write!(f, "({t})"),
//             TypeUnbound::ImplTraitOneBound(b) => write!(f, "impl{b}"),
//             TypeUnbound::TraitObjectOneBound => todo!(),
//             TypeUnbound::TypePath => todo!(),
//             TypeUnbound::Tuple => todo!(),
//             TypeUnbound::Never => todo!(),
//             TypeUnbound::RawPointer => todo!(),
//             TypeUnbound::Reference => todo!(),
//             TypeUnbound::Array => todo!(),
//             TypeUnbound::Slice => todo!(),
//             TypeUnbound::Inferred => todo!(),
//             TypeUnbound::QualifiedPathInType => todo!(),
//             TypeUnbound::BareFunction => todo!(),
//             TypeUnbound::MacroInvocation => todo!(),
//         }
//     }
// }

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct TraitBoundInner {
    #[unparse({if self.not { '?'.unparse(f)?; }})]
    not: bool,
    path: TypePath,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum TraitBound<'data> {
    NoParen(TraitBoundInner),
    #[unparse(delim('(', ')'))]
    Paren(AuthorialDelim<'data, TraitBoundInner>),
}
