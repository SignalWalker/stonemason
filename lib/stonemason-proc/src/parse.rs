use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{punctuated::Punctuated, spanned::Spanned, DeriveInput, ExprCall, Fields, Ident, Token};

mod modifier;
pub(crate) use modifier::*;

mod struct_;
pub(crate) use struct_::*;

mod enum_;
pub(crate) use enum_::*;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("expected ident, found {0:?}")]
    NotAnIdent(Box<dyn 'static + std::fmt::Debug>),
}

pub(crate) struct ParseField<const ENUM: bool> {
    ty: syn::Type,
    id: Option<syn::Ident>,
    m: ParseMod,
}

impl<const ENUM: bool> ParseField<ENUM> {
    fn from_fields<'f>(
        fields: impl IntoIterator<Item = &'f syn::Field>,
    ) -> Result<Vec<Self>, syn::Error> {
        let mut fields = fields.into_iter();
        let mut res = Vec::with_capacity({
            let size = fields.size_hint();
            size.1.unwrap_or(size.0)
        });
        for field in fields {
            res.push(Self::try_from(field)?);
        }
        Ok(res)
    }
}

impl<const ENUM: bool> ToTokens for ParseField<ENUM> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let span = self.ty.span();
        let synerror = |msg: &'static str| -> Result<TokenStream, syn::Error> {
            Err(syn::Error::new(self.ty.span(), msg))
        };
        let ty: Result<TokenStream, syn::Error> = match &self.ty {
            syn::Type::Array(_) => synerror("parse array"),
            syn::Type::BareFn(_) => synerror("parse barefn"),
            syn::Type::Group(_) => synerror("parse group"),
            syn::Type::ImplTrait(_) => synerror("parse impltrait"),
            syn::Type::Infer(_) => synerror("parse infer"),
            syn::Type::Macro(_) => synerror("parse macro"),
            syn::Type::Never(_) => synerror("parse never"),
            syn::Type::Paren(_) => synerror("parse paren"),
            syn::Type::Path(syn::TypePath { path, .. }) => path
                .segments
                .first()
                .map(|s| s.ident.to_token_stream())
                .ok_or(syn::Error::new(self.ty.span(), "no segments in typepath")),
            syn::Type::Ptr(_) => synerror("parse ptr"),
            syn::Type::Reference(_) => synerror("parse reference"),
            syn::Type::Slice(_) => synerror("parse slice"),
            syn::Type::TraitObject(_) => synerror("parse traitobject"),
            syn::Type::Tuple(t) => Ok(quote_spanned! {span=><#t>}),
            syn::Type::Verbatim(_) => synerror("parse verbatim"),
            _ => synerror("unrecognized type"),
        };
        self.m.to_tokens(
            || {
                let ty = ty.unwrap_or_else(syn::Error::into_compile_error);
                syn::parse2(quote_spanned! {span=>#ty::from_parse})
                    .unwrap_or_else(syn::Error::into_compile_error)
            },
            tokens,
        )
    }
}

impl<const ENUM: bool> TryFrom<&syn::Field> for ParseField<ENUM> {
    type Error = syn::Error;

    fn try_from(field: &syn::Field) -> Result<Self, Self::Error> {
        Ok(Self {
            ty: field.ty.clone(),
            id: field.ident.clone(),
            m: ParseMod::from_attrs_or_def(&field.attrs)?,
        })
    }
}

pub(crate) enum ParseInner {
    Struct(ParseStruct<false>),
    Enum(ParseEnum),
}

impl ToTokens for ParseInner {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Struct(s) => s.to_tokens(tokens),
            Self::Enum(e) => e.to_tokens(tokens),
            // Self::Union => todo!(),
        }
    }
}

impl TryFrom<&syn::Data> for ParseInner {
    type Error = syn::Error;

    fn try_from(data: &syn::Data) -> Result<Self, Self::Error> {
        match data {
            syn::Data::Struct(s) => s.try_into().map(Self::Struct),
            syn::Data::Enum(e) => e.try_into().map(Self::Enum),
            syn::Data::Union(_u) => todo!("#[derive(Parsed)] for unions"),
        }
    }
}

pub(crate) struct Parse {
    mod_: ParseMod,
    inner: ParseInner,
}

impl ToTokens for Parse {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let inner = &self.inner;
        self.mod_.to_tokens(
            || ParseExpr::Tokens(inner.to_token_stream()).to_token_stream(),
            tokens,
        )
    }
}

impl TryFrom<&DeriveInput> for Parse {
    type Error = syn::Error;

    fn try_from(input: &DeriveInput) -> Result<Self, Self::Error> {
        Ok(Self {
            mod_: ParseMod::from_attrs_or_def(&input.attrs)?,
            inner: ParseInner::try_from(&input.data)?,
        })
    }
}

pub(crate) fn gen_parser(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    Parse::try_from(input).map(|r| r.to_token_stream())
}
