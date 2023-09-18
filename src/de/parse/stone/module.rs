use stonemason_proc::{Unparse, UnparseDisplay};

use crate::de::parse::{
    tag::KwMod, Authorial0, Identifier, InnerAttribute, Item, Many0, Separated, Unparse,
};

#[derive(Unparse, UnparseDisplay)]
pub enum ModuleInner<'data> {
    #[unparse(';')]
    Extern,
    #[unparse(delim('{', '}'))]
    Block {
        attributes: Many0<(Authorial0<'data>, InnerAttribute<'data>)>,
        items: Many0<(Authorial0<'data>, Item<'data>)>,
        _c2: Authorial0<'data>,
    },
}

#[derive(Unparse, UnparseDisplay)]
pub struct Module<'data> {
    _mod: KwMod,
    _c1: Authorial0<'data>,
    id: Identifier<'data>,
    _c2: Authorial0<'data>,
    inner: ModuleInner<'data>,
}
