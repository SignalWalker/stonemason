use stonemason_proc::{Unparse, UnparseDisplay};

use super::{Type, Unparse};

pub enum GenericParamInner {
    Type(),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub enum GenericArg<'data> {
    Type(Type<'data>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Unparse, UnparseDisplay)]
pub struct GenericArgs<'data> {
    args: Vec<GenericArg<'data>>,
}

