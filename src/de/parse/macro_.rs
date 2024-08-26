use super::{
    Authorial0, Authorial1, Many0, OuterAttribute, RawStringLiteral, StringLiteral, Token,
};
use stonemason_proc::Parsed;


#[derive(Parsed)]
pub enum ParseTest<'data> {
    #[parsed(todo())]
    Empty,
    Boop(Authorial0<'data>, Authorial1<'data>, StringLiteral<'data>),
    Bep{ paren: Token<'('>, attrs: Many0<OuterAttribute<'data>>, paren2: Token<')'> }
}
