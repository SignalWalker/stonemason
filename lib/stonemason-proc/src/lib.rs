use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, DeriveInput, GenericParam, Generics, LifetimeParam,
    TypeParamBound,
};

mod unparse;
use unparse::*;

mod parse;
use parse::*;

#[proc_macro_derive(Parsed, attributes(parsed))]
pub fn derive_parsed(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    let (lifetime, lifetime_is_artificial) = match input.generics.lifetimes().next() {
        Some(lt) => (lt.lifetime.clone(), false),
        None => (
            syn::Lifetime::new("'__lt", proc_macro2::Span::call_site()),
            true,
        ),
    };

    let parsed: syn::TypePath = parse_quote!(crate::de::parse::Parsed);
    let parse_error: syn::TypePath = parse_quote!(crate::de::parse::ParseError);

    add_trait_bounds(&parse_quote!(#parsed<&#lifetime str>), &mut input.generics);

    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clauses) = input.generics.split_for_impl();
    let impl_generics = match lifetime_is_artificial {
        true => {
            let mut gen = input.generics.clone();
            gen.params.insert(
                0,
                GenericParam::Lifetime(LifetimeParam::new(lifetime.clone())),
            );
            gen.split_for_impl().0.to_token_stream()
        }
        false => impl_generics.to_token_stream(),
    };

    let parse = gen_parser(&input).unwrap_or_else(syn::Error::into_compile_error);

    let expanded = quote!(
        #[automatically_derived]
        impl #impl_generics #parsed<&#lifetime str> for #ident #ty_generics #where_clauses {
            fn from_parse<Error: #parse_error<&#lifetime str>>(input: &#lifetime str) -> nom::IResult<&#lifetime str, Self, Error> {
                nom::Parser::parse(&mut nom::error::context(::std::stringify!(#ident), #parse), input)
            }
        }
    );
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(Unparse, attributes(unparse))]
pub fn derive_unparse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    let unparse_ty: syn::TypePath = parse_quote!(crate::de::parse::Unparse);

    add_trait_bounds(&parse_quote!(#unparse_ty), &mut input.generics);
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clauses) = input.generics.split_for_impl();

    let unparse = gen_unparser(&input).unwrap_or_else(syn::Error::into_compile_error);

    let expanded = quote!(
        #[automatically_derived]
        impl #impl_generics #unparse_ty for #ident #ty_generics #where_clauses {
            fn unparse(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                #unparse
                Ok(())
            }
        }
    );
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(UnparseDisplay)]
pub fn derive_display_from_unparse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    let unparse_ty: syn::TypePath = parse_quote!(crate::de::parse::Unparse);

    add_trait_bounds(&parse_quote!(#unparse_ty), &mut input.generics);
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clauses) = input.generics.split_for_impl();

    let expanded = quote!(
        #[automatically_derived]
        impl #impl_generics ::std::fmt::Display for #ident #ty_generics #where_clauses {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                use #unparse_ty;
                self.unparse(f)
            }
        }
    );
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(UnparseDebug)]
pub fn derive_debug_from_unparse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    let unparse_ty: syn::TypePath = parse_quote!(crate::de::parse::Unparse);

    add_trait_bounds(&parse_quote!(#unparse_ty), &mut input.generics);
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clauses) = input.generics.split_for_impl();

    let expanded = quote!(
        #[automatically_derived]
        impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clauses {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                self.unparse(f)
            }
        }
    );
    proc_macro::TokenStream::from(expanded)
}

pub(crate) fn add_trait_bounds(bound: &TypeParamBound, generics: &mut Generics) {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(bound.clone());
        }
    }
}
