use quote::quote;
use syn::{parse_macro_input, parse_quote, DeriveInput, GenericParam, Generics, TypeParamBound};

mod unparse;
use unparse::*;

mod parse;
use parse::*;

#[proc_macro_derive(Parsed, attributes(parsed))]
pub fn derive_parsed(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    add_trait_bounds(&parse_quote!(Parsed<Input>), &mut input.generics);
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clauses) = input.generics.split_for_impl();

    let parse = gen_parser(&input)
        .map_err(syn::Error::into_compile_error)
        .unwrap();

    let expanded = quote!(
        #[automatically_derived]
        impl #impl_generics Parsed<Input> for #ident #ty_generics #where_clauses {
            fn from_parse(input: Input) -> nom::IResult<Input, #ident #ty_generics> {
                #parse
            }
        }
    );
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(Unparse, attributes(unparse))]
pub fn derive_unparse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    add_trait_bounds(&parse_quote!(Unparse), &mut input.generics);
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clauses) = input.generics.split_for_impl();

    let unparse = gen_unparser(&input)
        .map_err(syn::Error::into_compile_error)
        .unwrap();

    let expanded = quote!(
        #[automatically_derived]
        impl #impl_generics Unparse for #ident #ty_generics #where_clauses {
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

    add_trait_bounds(&parse_quote!(Unparse), &mut input.generics);
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clauses) = input.generics.split_for_impl();

    let expanded = quote!(
        #[automatically_derived]
        impl #impl_generics ::std::fmt::Display for #ident #ty_generics #where_clauses {
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
