use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
    Token,
};

#[derive(Debug, thiserror::Error)]
pub enum ModifierError {
    #[error("expected ident, found {0:?}")]
    NotAnIdent(Box<dyn 'static + std::fmt::Debug>),
    #[error("expected {0} arguments, found {1}")]
    ArgumentArity(usize, usize),
    #[error("expected (prefix|suffix|(delim|delimited)|replace), found {0}")]
    UnrecognizedId(String),
}

#[derive(Clone)]
pub(crate) enum ParseExpr {
    Expr(syn::Expr),
    Tokens(TokenStream),
}

impl From<syn::Expr> for ParseExpr {
    fn from(expr: syn::Expr) -> Self {
        Self::Expr(expr)
    }
}

impl syn::parse::Parse for ParseExpr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        syn::Expr::parse(input).map(Self::Expr)
    }
}

impl ToTokens for ParseExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Expr(expr) => match expr {
                syn::Expr::Lit(syn::ExprLit { lit, .. }) => match lit {
                    syn::Lit::Str(s) => {
                        quote_spanned!(lit.span()=>nom::bytes::complete::tag(#s)).to_tokens(tokens)
                    }
                    syn::Lit::ByteStr(_) => todo!("parse literal: bytestr"),
                    syn::Lit::Byte(_) => todo!("parse literal: byte"),
                    syn::Lit::Char(c) => {
                        quote_spanned!(c.span()=>nom::character::complete::char(#c))
                            .to_tokens(tokens)
                    }
                    syn::Lit::Int(_) => todo!("parse literal: int"),
                    syn::Lit::Float(_) => todo!("parse literal: float"),
                    syn::Lit::Bool(_) => todo!("parse literal: bool"),
                    syn::Lit::Verbatim(_) => todo!("parse literal: verbatim"),
                    _ => todo!("unrecognized literal"),
                },
                _ => quote_spanned! {expr.span()=>#expr}.to_tokens(tokens),
            },
            Self::Tokens(t) => t.to_tokens(tokens),
        }
    }
}

#[derive(Default)]
pub enum ParseMod {
    Prefix(ParseExpr, Box<ParseMod>),
    Suffix(Box<ParseMod>, ParseExpr),
    Delimited(ParseExpr, Box<ParseMod>, ParseExpr),
    Replace(ParseExpr),
    ToDo(Option<syn::Expr>),
    Cut(Box<ParseMod>),
    #[default]
    Default,
}

impl ParseMod {
    pub(crate) fn to_tokens(
        &self,
        default: impl FnOnce() -> TokenStream,
        tokens: &mut TokenStream,
    ) {
        match self {
            Self::Prefix(prf, e) => {
                let expr = e.to_token_stream(default);
                quote! {
                    nom::sequence::preceded(#prf, #expr)
                }
                .to_tokens(tokens)
            }
            Self::Suffix(e, sfx) => {
                let expr = e.to_token_stream(default);
                quote! {
                    nom::sequence::terminated(#expr, #sfx)
                }
                .to_tokens(tokens)
            }
            Self::Delimited(bf, e, af) => {
                let expr = e.to_token_stream(default);
                quote! {
                    nom::sequence::delimited(#bf, #expr, #af)
                }
                .to_tokens(tokens)
            }
            Self::ToDo(msg) => quote! { todo!(#msg) }.to_tokens(tokens),
            Self::Replace(e) => quote! {#e}.to_tokens(tokens),
            Self::Cut(inner) => {
                let inner = inner.to_token_stream(default);
                quote! { nom::combinator::cut(#inner) }.to_tokens(tokens)
            }
            Self::Default => {
                default().to_tokens(tokens);
            }
        }
    }

    pub(crate) fn to_token_stream(&self, default: impl FnOnce() -> TokenStream) -> TokenStream {
        let mut res = TokenStream::new();
        self.to_tokens(default, &mut res);
        res
    }
}

impl syn::parse::Parse for ParseMod {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        syn::Expr::parse(input).and_then(Self::try_from)
    }
}

impl TryFrom<syn::ExprCall> for ParseMod {
    type Error = syn::Error;

    fn try_from(mut f: syn::ExprCall) -> Result<Self, Self::Error> {
        let id = match &*f.func {
            syn::Expr::Path(syn::ExprPath { path, .. }) => {
                path.get_ident().map(ToString::to_string).ok_or_else(|| {
                    syn::Error::new(
                        path.span(),
                        ModifierError::NotAnIdent(Box::new(path.clone())),
                    )
                })?
            }
            _ => {
                return Err(syn::Error::new(
                    f.func.span(),
                    ModifierError::NotAnIdent(f.func.clone()),
                ))
            }
        };
        match id.as_str() {
            "prefix" => {
                if f.args.len() != 1 && f.args.len() != 2 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        ModifierError::ArgumentArity(2, f.args.len()),
                    ));
                }
                let prf = f.args.pop().unwrap().into_value();
                match f.args.pop().map(Pair::into_value) {
                    Some(expr) => {
                        Self::try_from(expr).map(|inner| Self::Prefix(prf.into(), Box::new(inner)))
                    }
                    None => Ok(Self::Prefix(prf.into(), Box::new(Self::Default))),
                }
            }
            "suffix" => {
                if f.args.len() != 1 && f.args.len() != 2 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        ModifierError::ArgumentArity(2, f.args.len()),
                    ));
                }
                let arg1 = f.args.pop().unwrap().into_value();
                match f.args.pop().map(Pair::into_value) {
                    Some(sfx) => {
                        Self::try_from(arg1).map(|inner| Self::Suffix(Box::new(inner), sfx.into()))
                    }
                    None => Ok(Self::Suffix(Box::new(Self::Default), arg1.into())),
                }
            }
            "delim" | "delimited" => {
                if f.args.len() != 2 && f.args.len() != 3 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        ModifierError::ArgumentArity(3, f.args.len()),
                    ));
                }
                let arg1 = f.args.pop().unwrap().into_value();
                let arg2 = f.args.pop().unwrap().into_value();
                match f.args.pop().map(Pair::into_value) {
                    Some(sfx) => Self::try_from(arg2)
                        .map(|inner| Self::Delimited(sfx.into(), Box::new(inner), arg1.into())),
                    None => Ok(Self::Delimited(
                        arg2.into(),
                        Box::new(Self::Default),
                        arg1.into(),
                    )),
                }
            }
            "replace" => {
                if f.args.len() != 1 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        ModifierError::ArgumentArity(1, f.args.len()),
                    ));
                }
                let expr = f.args.pop().unwrap().into_value();
                Ok(Self::Replace(expr.into()))
            }
            "todo" => {
                if f.args.len() > 1 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        ModifierError::ArgumentArity(1, f.args.len()),
                    ));
                }
                Ok(Self::ToDo(f.args.pop().map(Pair::into_value)))
            }
            "cut" => {
                if f.args.len() > 1 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        ModifierError::ArgumentArity(1, f.args.len()),
                    ));
                }
                let inner = match f.args.pop().map(Pair::into_value) {
                    Some(i) => Self::try_from(i)?,
                    None => Self::Default,
                };
                Ok(Self::Cut(Box::new(inner)))
            }
            _ => Err(syn::Error::new(
                f.func.span(),
                ModifierError::UnrecognizedId(id.to_owned()),
            )),
        }
    }
}

impl TryFrom<syn::Expr> for ParseMod {
    type Error = syn::Error;

    fn try_from(expr: syn::Expr) -> Result<Self, Self::Error> {
        use syn::Expr;
        match expr {
            Expr::Call(c) => c.try_into(),
            Expr::Infer(_) => Ok(Self::Default),
            _ => Ok(Self::Replace(expr.into())),
        }
    }
}

impl ParseMod {
    fn from_attr(attr: &syn::Attribute) -> Result<Option<Self>, syn::Error> {
        match attr
            .path()
            .get_ident()
            .map(ToString::to_string)
            .as_ref()
            .map(String::as_str)
        {
            Some("parsed") => attr.parse_args::<Self>().map(Some),
            _ => Ok(None),
        }
    }
    fn from_attrs<'attr>(
        attrs: impl IntoIterator<Item = &'attr syn::Attribute>,
    ) -> Result<Option<Self>, syn::Error> {
        for attr in attrs {
            let res = Self::from_attr(attr)?;
            if res.is_some() {
                return Ok(res);
            }
        }
        Ok(None)
    }
    pub(super) fn from_attrs_or_def<'attr>(
        attrs: impl IntoIterator<Item = &'attr syn::Attribute>,
    ) -> Result<Self, syn::Error> {
        Self::from_attrs(attrs).map(Option::unwrap_or_default)
    }
    pub(super) fn from_fields<'field>(
        fields: &'field Punctuated<syn::Field, Token![,]>,
    ) -> Result<Vec<Self>, syn::Error> {
        let mut res = Vec::with_capacity(fields.len());
        for field in fields {
            res.push(Self::from_attrs_or_def(&field.attrs)?);
        }
        Ok(res)
    }
}
