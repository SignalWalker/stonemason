use proc_macro2::TokenStream;
use syn::DeriveInput;

pub enum ParseMod {
    Tokens(TokenStream),
}

pub(crate) fn gen_parser(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    todo!()
}
