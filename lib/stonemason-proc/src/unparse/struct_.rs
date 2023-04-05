use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, DataStruct, Fields, Ident};

use super::{UnparseError, UnparseMod};

pub(crate) struct UnparseStructField {
    id: Ident,
    m: UnparseMod,
}

impl TryFrom<&syn::Field> for UnparseStructField {
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

impl ToTokens for UnparseStructField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let id = &self.id;
        self.m
            .to_tokens(|| syn::parse2(quote! {self.#id}).unwrap(), tokens)
    }
}

impl UnparseStructField {
    fn from_fields(fields: &syn::FieldsNamed) -> Result<Vec<Self>, syn::Error> {
        let mut res = Vec::with_capacity(fields.named.len());
        for field in &fields.named {
            res.push(Self::try_from(field)?);
        }
        Ok(res)
    }
}

pub(crate) enum UnparseStruct {
    Named(Vec<UnparseStructField>),
    Unnamed(Vec<UnparseMod>),
    Unit,
}

impl ToTokens for UnparseStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            UnparseStruct::Named(fields) => quote! {
                #(#fields;)*
            }
            .to_tokens(tokens),
            UnparseStruct::Unnamed(mods) => {
                let inner = mods.iter().enumerate().map(|(i, m)| {
                    let i = syn::Index {
                        index: i as u32,
                        span: Span::call_site(),
                    };
                    m.to_token_stream(|| syn::parse2(quote! {self.#i}).unwrap())
                });
                quote! {#(#inner;)*}.to_tokens(tokens)
            }
            UnparseStruct::Unit => {
                quote! {compile_error!("must provide #[unparse(...)] for unit struct")}
                    .to_tokens(tokens)
            }
        }
    }
}

impl TryFrom<&DataStruct> for UnparseStruct {
    type Error = syn::Error;

    fn try_from(data: &DataStruct) -> Result<Self, Self::Error> {
        match &data.fields {
            Fields::Named(fields) => UnparseStructField::from_fields(&fields).map(Self::Named),
            Fields::Unnamed(fields) => UnparseMod::from_fields(&fields.unnamed).map(Self::Unnamed),
            Fields::Unit => Ok(Self::Unit),
        }
    }
}
