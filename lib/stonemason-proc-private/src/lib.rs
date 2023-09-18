use proc_macro2::{Span, TokenStream};
use syn::{parse_quote, GenericParam, Ident, Type, TypeParam, TypeParamBound};

use std::{collections::VecDeque, fmt::Write};

use quote::{quote, ToTokens};

const ALPHA: &[char] = &[
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

#[proc_macro]
pub fn impl_unparse_for_tuple(_tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    (1..ALPHA.len())
        .map(|i| &ALPHA[0..i])
        .fold(String::new(), |mut res, idents| {
            let mut params_a = String::new();
            let mut params_b = String::new();
            let mut fmts = String::new();
            for (i, id) in idents.iter().enumerate() {
                write!(&mut params_a, "{id}: Unparse, ").unwrap();
                write!(&mut params_b, "{id}, ").unwrap();
                write!(&mut fmts, "self.{i}.unparse(f)?;\n").unwrap();
            }
            write!(
                &mut res,
                r#"impl<{params_a}> Unparse for ({params_b}) {{
                    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
                        {fmts}
                        Ok(())
                    }}
                }}"#
            )
            .unwrap();
            res
        })
        .parse()
        .unwrap()
}

#[proc_macro]
pub fn impl_parsed_for_tuple(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut res = TokenStream::new();
    let param_bound: TypeParamBound = parse_quote!(Parsed<Input>);
    for idents in (1..=ALPHA.len()).into_iter().map(|i| &ALPHA[0..i]) {
        let mut impl_generics: Vec<GenericParam> = vec![parse_quote!(Input: Clone)];
        let mut ty_generics: Vec<Type> = Vec::with_capacity(idents.len());
        let mut parses: Vec<Ident> = Vec::with_capacity(idents.len());
        let mut map_pat: Option<TokenStream> = None;
        let mut map_expr: Vec<Ident> = Vec::with_capacity(idents.len());
        for id in idents {
            let ident = Ident::new(id.to_string().as_str(), Span::call_site());
            impl_generics.push(GenericParam::Type({
                let mut res = TypeParam::from(ident.clone());
                res.bounds.push(param_bound.clone());
                res
            }));
            if *id != 'A' {
                parses.push(ident.clone());
            }
            ty_generics.push(parse_quote!(#ident));
            let map_id = Ident::new(id.to_lowercase().to_string().as_str(), Span::call_site());
            map_pat = {
                let map_id = map_id.clone();
                match map_pat {
                    Some(m) => Some(quote!((#m, #map_id))),
                    None => Some(quote!(#map_id)),
                }
            };
            map_expr.push(map_id);
        }
        quote! {
            #[automatically_derived]
            impl < #(#impl_generics),* > Parsed<Input> for ( #(#ty_generics,)* ) {
                fn from_parse(input: Input) -> nom::IResult<Input, Self> {
                    A::from_parse #(.and(#parses::from_parse))* .map(|#map_pat| (#(#map_expr,)*)).parse(input)
                }
            }
        }
        .to_tokens(&mut res);
    }
    res.into()
}
