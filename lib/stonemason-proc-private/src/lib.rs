use proc_macro::TokenStream;

use std::fmt::Write;

const ALPHA: &[char] = &[
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

#[proc_macro]
pub fn impl_unparse_for_tuple(_tokens: TokenStream) -> TokenStream {
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
