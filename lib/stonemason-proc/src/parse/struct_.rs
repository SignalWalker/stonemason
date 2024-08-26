use super::{ParseField, ParseMod};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{spanned::Spanned, Fields};

pub(crate) enum ParseStruct<const ENUM: bool> {
    Composite(syn::TypePath, bool, Vec<ParseField<ENUM>>),
    Unit(Span, syn::TypePath),
}

impl<const ENUM: bool> ToTokens for ParseStruct<ENUM>
where
    ParseField<ENUM>: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Composite(type_path, named, fields) => {
                let mut map_pat = Option::<TokenStream>::None;
                let mut map_idents = Vec::<syn::Ident>::new();
                let mut inner = Option::<TokenStream>::None;
                for (i, field) in fields.iter().enumerate() {
                    let map_id = match field.id.as_ref() {
                        Some(id) => id.clone(),
                        None => syn::Ident::new(format!("field_{i}").as_str(), field.ty.span()),
                    };
                    map_idents.push(map_id.clone());
                    map_pat = match map_pat {
                        Some(m) => Some(quote! {(#m, #map_id)}),
                        None => Some(quote! {#map_id}),
                    };
                    inner = match inner {
                        Some(i) => Some(quote! {nom::Parser::and(#i, #field)}),
                        None => Some(field.to_token_stream()),
                    }
                }
                let map_expr = if *named {
                    quote! { #type_path { #(#map_idents,)* } }
                } else {
                    quote! { #type_path(#(#map_idents,)*) }
                };
                let inner = inner
                    .unwrap_or_else(|| quote!(compile_error!("parsed struct must have fields")));
                quote! { nom::Parser::map(#inner, |#map_pat| #map_expr) }.to_tokens(tokens)
            }
            Self::Unit(span, _) => {
                quote_spanned! {*span=>compile_error!("must provide #[parsed(...)] for unit type")}
                    .to_tokens(tokens)
            }
        }
    }
}

impl<const ENUM: bool> TryFrom<&syn::DataStruct> for ParseStruct<ENUM> {
    type Error = syn::Error;

    fn try_from(data: &syn::DataStruct) -> Result<Self, Self::Error> {
        let type_path = syn::parse2(quote! {Self})?;
        match &data.fields {
            Fields::Named(fields) if !fields.named.is_empty() => {
                ParseField::from_fields(&fields.named).map(|f| Self::Composite(type_path, true, f))
            }
            Fields::Unnamed(fields) if !fields.unnamed.is_empty() => {
                ParseField::from_fields(&fields.unnamed)
                    .map(|f| Self::Composite(type_path, false, f))
            }
            _ => Ok(Self::Unit(Span::call_site(), type_path)),
        }
    }
}
