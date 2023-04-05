use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
    DeriveInput, Expr, ExprCall, Token,
};

mod struct_;
pub(crate) use struct_::*;
mod enum_;
pub(crate) use enum_::*;

#[derive(Clone)]
pub(crate) enum UnparseExpr {
    Expr(syn::Expr),
    Tokens(TokenStream),
}

impl ToTokens for UnparseExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Expr(expr) => match expr {
                syn::Expr::Block(b) => quote_spanned! {b.span()=>#b}.to_tokens(tokens),
                _ => quote_spanned! {expr.span()=>#expr.unparse(f)?}.to_tokens(tokens),
            },
            Self::Tokens(t) => t.to_tokens(tokens),
        }
    }
}

impl From<syn::Expr> for UnparseExpr {
    fn from(value: syn::Expr) -> Self {
        Self::Expr(value)
    }
}

impl Parse for UnparseExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        syn::Expr::parse(input).map(UnparseExpr::Expr)
    }
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum UnparseError {
    #[error("expected ident, found {0:?}")]
    NotAnIdent(Box<dyn 'static + std::fmt::Debug>),
    #[error("expected {0} arguments, found {1}")]
    ArgumentArity(usize, usize),
    #[error("expected (prefix|suffix|(delim|delimited)|replace), found {0}")]
    UnrecognizedId(String),
    // #[error("expected function call, block, inference, or literal expression; found {0:?}")]
    // UnrecognizedExpr(syn::Expr),
    // #[error("expected only one #[unparse(...)] attribute")]
    // MultipleDefinitions,
    // #[error("expected #[unparse(...)] attribute")]
    // UnrecognizedAttribute,
}

#[derive(Default)]
pub(crate) enum UnparseMod {
    Prefix(UnparseExpr, Box<UnparseMod>),
    Suffix(Box<UnparseMod>, UnparseExpr),
    Delimited(UnparseExpr, Box<UnparseMod>, UnparseExpr),
    Replace(UnparseExpr),
    #[default]
    Default,
}

impl UnparseMod {
    pub(crate) fn to_tokens(
        &self,
        default: impl FnOnce() -> UnparseExpr,
        tokens: &mut TokenStream,
    ) {
        match self {
            UnparseMod::Prefix(prf, e) => {
                let expr = e.to_token_stream(default);
                quote! {
                    #prf;
                    #expr;
                }
                .to_tokens(tokens)
            }
            UnparseMod::Suffix(e, sfx) => {
                let expr = e.to_token_stream(default);
                quote! {
                    #sfx;
                    #expr;
                }
                .to_tokens(tokens)
            }
            UnparseMod::Delimited(bf, e, af) => {
                let expr = e.to_token_stream(default);
                quote! {
                    #bf;
                    #expr;
                    #af;
                }
                .to_tokens(tokens)
            }
            UnparseMod::Replace(e) => quote! {#e;}.to_tokens(tokens),
            UnparseMod::Default => {
                let expr = default();
                quote! {#expr;}.to_tokens(tokens)
            }
        }
    }

    pub(crate) fn to_token_stream(&self, default: impl FnOnce() -> UnparseExpr) -> TokenStream {
        let mut res = TokenStream::new();
        self.to_tokens(default, &mut res);
        res
    }
}

impl Parse for UnparseMod {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        syn::Expr::parse(input).and_then(Self::try_from)
    }
}

impl TryFrom<ExprCall> for UnparseMod {
    type Error = syn::Error;

    fn try_from(mut f: ExprCall) -> Result<Self, Self::Error> {
        let id = match &*f.func {
            syn::Expr::Path(syn::ExprPath { path, .. }) => {
                path.get_ident().map(ToString::to_string).ok_or_else(|| {
                    syn::Error::new(
                        path.span(),
                        UnparseError::NotAnIdent(Box::new(path.clone())),
                    )
                })?
            }
            _ => {
                return Err(syn::Error::new(
                    f.func.span(),
                    UnparseError::NotAnIdent(f.func.clone()),
                ))
            }
        };
        match id.as_str() {
            "prefix" => {
                if f.args.len() != 1 && f.args.len() != 2 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        UnparseError::ArgumentArity(2, f.args.len()),
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
                        UnparseError::ArgumentArity(2, f.args.len()),
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
                        UnparseError::ArgumentArity(3, f.args.len()),
                    ));
                }
                let arg1 = f.args.pop().unwrap().into_value();
                let arg2 = f.args.pop().unwrap().into_value();
                match f.args.pop().map(Pair::into_value) {
                    Some(sfx) => Self::try_from(arg2)
                        .map(|inner| Self::Delimited(arg1.into(), Box::new(inner), sfx.into())),
                    None => Ok(Self::Delimited(
                        arg1.into(),
                        Box::new(Self::Default),
                        arg2.into(),
                    )),
                }
            }
            "replace" => {
                if f.args.len() != 1 {
                    return Err(syn::Error::new(
                        f.args.span(),
                        UnparseError::ArgumentArity(1, f.args.len()),
                    ));
                }
                let expr = f.args.pop().unwrap().into_value();
                Ok(Self::Replace(expr.into()))
            }
            _ => Err(syn::Error::new(
                f.func.span(),
                UnparseError::UnrecognizedId(id.to_owned()),
            )),
        }
    }
}

impl TryFrom<Expr> for UnparseMod {
    type Error = syn::Error;

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Call(c) => c.try_into(),
            Expr::Infer(_) => Ok(Self::Default),
            _ => Ok(Self::Replace(expr.into())),
            // _ => Err(syn::Error::new(
            //     expr.span(),
            //     UnparseError::UnrecognizedExpr(expr),
            // )),
        }
    }
}

impl UnparseMod {
    fn from_attr(attr: &syn::Attribute) -> Result<Option<Self>, syn::Error> {
        match attr
            .path()
            .get_ident()
            .map(ToString::to_string)
            .as_ref()
            .map(String::as_str)
        {
            Some("unparse") => attr.parse_args::<Self>().map(Some),
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

    fn from_attrs_or_def<'attr>(
        attrs: impl IntoIterator<Item = &'attr syn::Attribute>,
    ) -> Result<Self, syn::Error> {
        Self::from_attrs(attrs).map(Option::unwrap_or_default)
    }

    fn from_fields<'field>(
        fields: &'field Punctuated<syn::Field, Token![,]>,
    ) -> Result<Vec<Self>, syn::Error> {
        let mut res = Vec::with_capacity(fields.len());
        for field in fields {
            res.push(Self::from_attrs_or_def(&field.attrs)?);
        }
        Ok(res)
    }
}

pub(crate) enum UnparseInner {
    Struct(UnparseStruct),
    Enum(UnparseEnum),
    // Union,
}

impl ToTokens for UnparseInner {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Struct(s) => s.to_tokens(tokens),
            Self::Enum(e) => e.to_tokens(tokens),
            // Self::Union => todo!(),
        }
    }
}

impl TryFrom<&syn::Data> for UnparseInner {
    type Error = syn::Error;

    fn try_from(data: &syn::Data) -> Result<Self, Self::Error> {
        match data {
            syn::Data::Struct(s) => s.try_into().map(Self::Struct),
            syn::Data::Enum(e) => e.try_into().map(Self::Enum),
            syn::Data::Union(_u) => todo!(),
        }
    }
}

pub(crate) struct Unparse {
    mod_: UnparseMod,
    inner: UnparseInner,
}

impl ToTokens for Unparse {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let inner = &self.inner;
        self.mod_.to_tokens(
            || UnparseExpr::Tokens(inner.to_token_stream()).into(),
            tokens,
        )
    }
}

impl TryFrom<&DeriveInput> for Unparse {
    type Error = syn::Error;

    fn try_from(input: &DeriveInput) -> Result<Self, Self::Error> {
        Ok(Self {
            mod_: UnparseMod::from_attrs_or_def(&input.attrs)?,
            inner: UnparseInner::try_from(&input.data)?,
        })
    }
}

pub(crate) fn gen_unparser(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    Unparse::try_from(input).map(|r| r.to_token_stream())
}
