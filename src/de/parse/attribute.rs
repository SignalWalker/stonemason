use stonemason_proc::{Unparse, UnparseDisplay};

use super::{Authorial0, Expression, SimplePath, Unparse};

#[derive(Unparse, UnparseDisplay)]
#[unparse(delim("#![", ']'))]
pub struct InnerAttribute<'data> {
    _c1: Authorial0<'data>,
    attr: AttributeInner<'data>,
    _c2: Authorial0<'data>,
}

#[derive(Unparse, UnparseDisplay)]
#[unparse(delim("#[", ']'))]
pub struct OuterAttribute<'data> {
    _c1: Authorial0<'data>,
    attr: AttributeInner<'data>,
    _c2: Authorial0<'data>,
}

#[derive(Unparse, UnparseDisplay)]
pub struct AttributeInner<'data> {
    path: SimplePath<'data>,
    input: Option<AttributeInput<'data>>,
}

#[derive(Unparse, UnparseDisplay)]
pub enum AttributeInput<'data> {
    #[unparse({todo!()})]
    Delim,
    #[unparse(prefix('='))]
    Assign(Authorial0<'data>, Box<Expression<'data>>),
}
