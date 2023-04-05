use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Fields, Ident};

use super::{UnparseError, UnparseExpr, UnparseMod};

pub(crate) struct UnparseEnumField {
    id: Ident,
    m: UnparseMod,
}

impl TryFrom<&syn::Field> for UnparseEnumField {
    type Error = syn::Error;
    fn try_from(field: &syn::Field) -> Result<Self, Self::Error> {
        let id = field.ident.clone().ok_or_else(|| {
            syn::Error::new(
                field.span(),
                UnparseError::NotAnIdent(Box::new(field.clone())),
            )
        })?;
        Ok(Self {
            id,
            m: UnparseMod::from_attrs_or_def(&field.attrs)?,
        })
    }
}

impl ToTokens for UnparseEnumField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let id = &self.id;
        self.m
            .to_tokens(|| syn::parse2(quote! {#id}).unwrap(), tokens)
    }
}

impl UnparseEnumField {
    fn from_fields(fields: &syn::FieldsNamed) -> Result<Vec<Self>, syn::Error> {
        let mut res = Vec::with_capacity(fields.named.len());
        for field in &fields.named {
            res.push(Self::try_from(field)?);
        }
        Ok(res)
    }
}

pub(crate) enum UnparseEnumVariantInner {
    Named(Vec<UnparseEnumField>),
    Unnamed(Vec<UnparseMod>),
    Unit,
}

pub(crate) struct UnparseEnumVariant {
    mod_: UnparseMod,
    id: Ident,
    inner: UnparseEnumVariantInner,
}

impl TryFrom<&syn::Variant> for UnparseEnumVariant {
    type Error = syn::Error;
    fn try_from(var: &syn::Variant) -> Result<Self, Self::Error> {
        Ok(Self {
            mod_: UnparseMod::from_attrs_or_def(&var.attrs)?,
            id: var.ident.clone(),
            inner: match &var.fields {
                Fields::Named(fields) => {
                    UnparseEnumField::from_fields(&fields).map(UnparseEnumVariantInner::Named)?
                }
                Fields::Unnamed(fields) => UnparseMod::from_fields(&fields.unnamed)
                    .map(UnparseEnumVariantInner::Unnamed)?,
                Fields::Unit => UnparseEnumVariantInner::Unit,
            },
        })
    }
}

impl ToTokens for UnparseEnumVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let id = &self.id;
        let (pattern, inner) = match &self.inner {
            UnparseEnumVariantInner::Named(fields) => {
                let (names, inner): (Vec<Ident>, Vec<TokenStream>) = fields
                    .iter()
                    .map(|f| (f.id.clone(), f.to_token_stream()))
                    .unzip();
                (quote! { {#(#names,)*} }, quote! {#(#inner;)*})
            }
            UnparseEnumVariantInner::Unnamed(mods) => {
                let (names, inner): (Vec<Ident>, Vec<TokenStream>) = mods
                    .iter()
                    .enumerate()
                    .map(|(i, m)| {
                        let id = Ident::new(&format!("__self_{i}"), Span::call_site());
                        (
                            id.clone(),
                            m.to_token_stream(|| syn::parse2(quote! {#id}).unwrap()),
                        )
                    })
                    .unzip();
                (quote! { (#(#names,)*) }, quote! { #(#inner;)* })
            }
            UnparseEnumVariantInner::Unit => (
                quote! {},
                quote! {compile_error!("must provide #[unparse(...)] for unit enum variant")},
            ),
        };
        let inner_mod = self.mod_.to_token_stream(|| UnparseExpr::Tokens(inner));
        quote! {
            Self::#id #pattern => {
                #inner_mod
            }
        }
        .to_tokens(tokens)
    }
}

pub(crate) struct UnparseEnum {
    variants: Vec<UnparseEnumVariant>,
}

impl ToTokens for UnparseEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vars = &self.variants;
        quote! {
            match self {
                #(#vars)*
            }
        }
        .to_tokens(tokens)
    }
}

impl TryFrom<&syn::DataEnum> for UnparseEnum {
    type Error = syn::Error;
    fn try_from(data: &syn::DataEnum) -> Result<Self, Self::Error> {
        Ok(Self {
            variants: {
                let mut res = Vec::with_capacity(data.variants.len());
                for var in &data.variants {
                    res.push(UnparseEnumVariant::try_from(var)?);
                }
                res
            },
        })
    }
}
