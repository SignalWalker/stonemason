use super::{ParseExpr, ParseField, ParseMod, ParseStruct};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{spanned::Spanned, Fields, Ident};

pub(crate) struct ParseEnumVariant {
    span: Span,
    mod_: ParseMod,
    inner: ParseStruct<true>,
}

impl ToTokens for ParseEnumVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let inner = &self.inner;
        let mut res = self
            .mod_
            .to_token_stream(|| quote_spanned!(self.span=>#inner));
        match (inner, &self.mod_) {
            (ParseStruct::Unit(span, p), ParseMod::Replace(_)) => {
                res = quote_spanned!(*span=>nom::Parser::map(#res, |_| #p));
            }
            (_, ParseMod::ToDo(msg)) => {
                let msg = msg
                    .as_ref()
                    .map_or_else(|| quote!("parse enum variant"), ToTokens::to_token_stream);
                res = quote_spanned!(self.span=>|_| todo!(#msg))
            }
            (_, ParseMod::Replace(_)) => todo!("parser replacement for non-unit enum variants"),
            _ => {}
        }
        res.to_tokens(tokens);
    }
}

impl ParseEnumVariant {
    fn try_from_syn(var: &syn::Variant) -> Result<Self, syn::Error> {
        let id = &var.ident;
        let path = syn::parse2(quote! { Self::#id })?;
        Ok(Self {
            mod_: ParseMod::from_attrs_or_def(&var.attrs)?,
            span: var.span(),
            inner: match &var.fields {
                Fields::Named(fields) if !fields.named.is_empty() => {
                    ParseField::from_fields(&fields.named)
                        .map(|f| ParseStruct::Composite(path, true, f))?
                }
                Fields::Unnamed(fields) if !fields.unnamed.is_empty() => {
                    ParseField::from_fields(&fields.unnamed)
                        .map(|f| ParseStruct::Composite(path, false, f))?
                }
                _ => ParseStruct::Unit(var.span(), path),
            },
        })
    }
}

pub(crate) struct ParseEnum(Vec<ParseEnumVariant>);

impl ToTokens for ParseEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vars = &self.0;
        let mut result = Option::<TokenStream>::None;

        for var in vars {
            result = match result {
                None => Some(var.to_token_stream()),
                Some(r) => Some(quote! {nom::Parser::or(#r, #var)}),
            };
        }

        result
            .unwrap_or_else(|| quote! {compile_error!("???")})
            .to_tokens(tokens);
    }
}

impl TryFrom<&syn::DataEnum> for ParseEnum {
    type Error = syn::Error;

    fn try_from(data: &syn::DataEnum) -> Result<Self, Self::Error> {
        Ok(Self({
            let mut res = Vec::with_capacity(data.variants.len());
            for var in &data.variants {
                res.push(ParseEnumVariant::try_from_syn(var)?);
            }
            res
        }))
    }
}
