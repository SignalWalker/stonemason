use super::{
    tag::{AttrClose, InnerAttrOpen, OuterAttrOpen},
    Authorial0, Expression, ParseError, Parsed, SimplePath, Token, Unparse,
};
use nom::{
    bytes::complete::tag,
    sequence::{delimited, preceded},
    Parser,
};
use stonemason_proc::{Parsed, Unparse, UnparseDisplay};

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct InnerAttribute<'data> {
    _open: (InnerAttrOpen, Authorial0<'data>),
    attr: AttributeInner<'data>,
    _close: (Authorial0<'data>, AttrClose),
}

impl std::fmt::Debug for InnerAttribute<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#![")?;
        std::fmt::Debug::fmt(&self.attr, f)?;
        write!(f, "]")
    }
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct OuterAttribute<'data> {
    _open: (OuterAttrOpen, Authorial0<'data>),
    attr: AttributeInner<'data>,
    _close: (Authorial0<'data>, AttrClose),
}

impl std::fmt::Debug for OuterAttribute<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#[")?;
        std::fmt::Debug::fmt(&self.attr, f)?;
        write!(f, "]")
    }
}

#[derive(Clone, Unparse, UnparseDisplay, Parsed)]
pub struct AttributeInner<'data> {
    path: SimplePath<'data>,
    input: Option<AttributeInput<'data>>,
}

impl std::fmt::Debug for AttributeInner<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.path, f)?;
        if let Some(input) = self.input.as_ref() {
            std::fmt::Debug::fmt(input, f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Unparse, UnparseDisplay, Parsed)]
pub enum AttributeInput<'data> {
    Assign {
        _eq: (Token<'='>, Authorial0<'data>),
        expr: Box<Expression<'data>>,
    },
    // Delim,
}
