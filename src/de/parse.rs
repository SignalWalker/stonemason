use nom::{IResult, Parser};

mod identifier;
pub use identifier::*;
mod literal;
pub use literal::*;
mod author;
pub use author::*;

mod stone;
pub use stone::*;

mod path;
pub use path::*;

mod attribute;
pub use attribute::*;

mod generic;
pub use generic::*;

mod type_;
pub use type_::*;

mod macro_;
pub use macro_::*;

mod meta;
pub use meta::*;

mod expr;
pub use expr::*;

/// Types that can be parsed from an input `Input`.
pub trait Parsed<Input>
where
    Self: Sized,
{
    /// Parse `input` into an instance of `Self`.
    #[cfg(not(feature = "nightly"))]
    fn from_parse(input: Input) -> IResult<Input, Self>;
    // i don't remember why this needed to be nightly-only
    #[cfg(feature = "nightly")]
    type Error = nom::error::Error<Input>;
    #[cfg(feature = "nightly")]
    fn from_parse(input: Input) -> IResult<Input, Self, Self::Error>;
}

/// Parse `Input` to `Output`, collapsing errors to `None`.
impl<I: Clone, T: Parsed<I>> Parsed<I> for Option<T> {
    fn from_parse(input: I) -> IResult<I, Self> {
        nom::combinator::opt(T::from_parse).parse(input)
    }
}

/// Parse `Input` to `Output`, boxing the result.
impl<I: Clone, T: Parsed<I>> Parsed<I> for Box<T> {
    fn from_parse(input: I) -> IResult<I, Self> {
        T::from_parse.map(Box::new).parse(input)
    }
}

/// Parse `Input` to `Output`, discarding the result.
impl<Input: Clone, Output: Parsed<Input>> Parsed<Input> for std::marker::PhantomData<Output> {
    fn from_parse(input: Input) -> nom::IResult<Input, Self> {
        nom::combinator::value(Self, Output::from_parse).parse(input)
    }
}

/// Like [std::fmt::Display], but specifically returns the unmodified parsed input of a [Parsed] type.
pub trait Unparse {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

#[cfg(feature = "nightly")]
pub trait ToUnparsed {
    fn to_unparsed(&self) -> String;
}

#[cfg(feature = "nightly")]
impl<T: Unparse + ?Sized> ToUnparsed for T {
    /// this is pretty much taken verbatim from the rust std lib code for ToString
    #[inline]
    fn to_unparsed(&self) -> String {
        let mut buf = String::new();
        let mut formatter = core::fmt::Formatter::new(&mut buf);
        self.unparse(&mut formatter)
            .expect("an Unparse implementation returned an error unexpectedly");
        buf
    }
}

stonemason_proc_private::impl_unparse_for_tuple!();
stonemason_proc_private::impl_parsed_for_tuple!();

macro_rules! impl_unparse_for_display {
    ($($t:ty),+) => {
        $(
            impl Unparse for $t {
                fn unparse(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    <$t as ::std::fmt::Display>::fmt(self, f)
                }
            }
        )+
    }
}

macro_rules! impl_unparse_for_iter {
    ($t:ident; $($i:ty),+) => {
        $(
            impl<$t: Unparse> Unparse for $i {
                fn unparse(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    for t in self {
                        t.unparse(f)?;
                    }
                    Ok(())
                }
            }
        )+
    }
}

impl_unparse_for_iter!(T; Vec<T>, [T]);
// impl_unparse_for_display!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, f32, f64);
impl_unparse_for_display!(str, String, char);

impl<T: Unparse> Unparse for Box<T> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (**self).unparse(f)
    }
}

impl<T: Unparse> Unparse for Option<T> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self {
            s.unparse(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[macro_export]
    macro_rules! test_parse {
        ($f:ident, $generator:expr; $input:ident => $exp_left:expr; $exp_res:expr) => {
            proptest::proptest! {
                #[test]
                fn $f($input in $generator) {
                    assert_eq!($crate::de::parse::$f(&$input), Ok(($exp_left, $exp_res)))
                }
            }
        };
        ($f:ident, $generator:expr; $input:ident => $expected:expr) => {
            test_parse! { $f, $generator; $input => ""; $expected }
        };
        ($f:ident, $generator:expr) => {
            test_parse! { $f, $generator; input => input.as_str() }
        };
    }

    #[macro_export]
    macro_rules! test_parse_complex {
        ($n:ident, $f:expr; $($arg:ident in $generator:expr),+ => $mk_input:expr => $exp_left:expr; $exp_res:expr) => {
            proptest::proptest! {
                #[test]
                fn $n($($arg in $generator),+) {
                    assert_eq!($f($mk_input), Ok(($exp_left, $exp_res)))
                }
            }
        };
        ($f:ident; $($arg:ident in $generator:expr),+ => $mk_input:expr => $exp_left:expr; $exp_res:expr) => {
            test_parse_complex!($f, $crate::de::parse::$f; $($arg in $generator),+ => $mk_input => $exp_left; $exp_res);
            // proptest::proptest! {
            //     #[test]
            //     fn $f($($arg in $generator),+) {
            //         assert_eq!($crate::de::parse::$f($mk_input), Ok(($exp_left, $exp_res)))
            //     }
            // }
        };
    }
}
