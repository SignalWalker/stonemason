use crate::de::parse::{
    tag::KwMod, Authorial0, Authorial1, Identifier, InnerAttribute, Item, Many0, Token,
};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub enum ModuleInner<'data> {
    Extern(Token<';'>),
    #[unparse(delim('{', '}'))]
    #[parsed(delim('{', '}'))]
    Block {
        _c1: Authorial0<'data>,
        attributes: Many0<(InnerAttribute<'data>, Authorial0<'data>)>,
        items: Many0<(Item<'data>, Authorial0<'data>)>,
    },
}

impl std::fmt::Debug for ModuleInner<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Extern(_) => write!(f, ";"),
            Self::Block {
                attributes, items, ..
            } => f
                .debug_struct("Block")
                .field("attributes", attributes)
                .field("items", items)
                .finish_non_exhaustive(),
        }
    }
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct Module<'data> {
    _mod: (KwMod, Authorial1<'data>),
    id: Identifier<'data>,
    _c2: Authorial0<'data>,
    inner: ModuleInner<'data>,
}

impl std::fmt::Debug for Module<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mod {:?} {:?}", self.id, self.inner)
    }
}
