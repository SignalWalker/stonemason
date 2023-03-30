mod identifier;
pub use identifier::*;
mod literal;
pub use literal::*;
mod comment;
pub use comment::*;

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
