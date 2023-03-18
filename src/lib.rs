#[derive(pest_derive::Parser)]
#[grammar = "./stn.pest"]
pub struct StnParser;

#[cfg(test)]
mod tests {
    use proptest::string::string_regex;

    use super::{Rule, StnParser};

    macro_rules! parses_to {
        ($input:expr, $rules:tt :: $rule:tt, [ $( $names:ident $calls:tt ),* $(,)* ]) => {
            {
                use pest::consumes_to;
                pest::parses_to!(parser: StnParser,
                    input: $input,
                    rule: $rules :: $rule,
                    tokens: [ $( $names $calls ),* ]
                )
            }
        };
    }

    macro_rules! fails_with {
        ($input:expr, $rules:tt :: $rule:tt, $positives:expr, $negatives:expr, $pos:expr) => {{
            pest::fails_with!(
                parser: StnParser,
                input: $input,
                rule: $rules::$rule,
                positives: $positives,
                negatives: $negatives,
                pos: $pos
            )
        }};
    }

    const IDENTIFIER_OR_KEYWORD: &str = r#"(\p{XID_Start}\p{XID_Continue}*)|(_\p{XID_Continue}+)"#;
    lazy_static::lazy_static! {
        static ref RAW_IDENTIFIER: String = format!("r#({IDENTIFIER_OR_KEYWORD})");
        static ref NON_KEYWORD_IDENTIFIER: String = format!("({IDENTIFIER_OR_KEYWORD})");
        static ref IDENTIFIER: String = format!("({})|({})", *NON_KEYWORD_IDENTIFIER, *RAW_IDENTIFIER);

        static ref SUFFIX: String = format!("({IDENTIFIER_OR_KEYWORD})");
        static ref SUFFIX_NO_E: String = format!(r#"[\p{{XID_Start}}&&[^eE]]({})"#, *SUFFIX);
    }

    // IDENTIFIER

    proptest::proptest! {
        #[test]
        fn identifier_or_keyword(input in IDENTIFIER_OR_KEYWORD) {
            parses_to!(&input, Rule::IdentifierOrKeyword, [IdentifierOrKeyword(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn raw_identifier(input in string_regex(&RAW_IDENTIFIER).unwrap()) {
            parses_to!(&input, Rule::RawIdentifier, [RawIdentifier(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn non_keyword_identifier(input in string_regex(&NON_KEYWORD_IDENTIFIER).unwrap()) {
            parses_to!(&input, Rule::NonKeywordIdentifier, [NonKeywordIdentifier(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn identifier(input in string_regex(&IDENTIFIER).unwrap()) {
            parses_to!(&input, Rule::Identifier, [Identifier(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn isolated_cr(c in r#"\r[^\n]?"#) {
            parses_to!(&c, Rule::IsolatedCr, [IsolatedCr(0, 1, [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn suffix(input in string_regex(&SUFFIX).unwrap()) {
            parses_to!(&input, Rule::Suffix, [Suffix(0, input.len(), [])]);
        }
    }

    // LITERAL

    // // NUMERIC

    const BIN_LITERAL: &str = r#"0b[01_]*[01][01_]*"#;
    const OCT_LITERAL: &str = r#"0o[0-7_]*[0-7][0-7_]*"#;
    const DEC_LITERAL: &str = r#"[[:digit:]][0-9_]*"#;
    const HEX_LITERAL: &str = r#"0x[0-9a-fA-F_]*[[:xdigit:]][0-9a-fA-F_]*"#;
    lazy_static::lazy_static! {
        static ref INTEGER_LITERAL: String = format!("(({DEC_LITERAL})|({BIN_LITERAL})|({OCT_LITERAL})|({HEX_LITERAL}))({})?", *SUFFIX_NO_E);

        static ref FLOAT_EXPONENT: String = format!("[eE][+-]?_*({DEC_LITERAL})");
        static ref FLOAT_LITERAL: String = format!(r#"(({DEC_LITERAL})(\.({DEC_LITERAL}))?({})({})?)|(({DEC_LITERAL})\.({DEC_LITERAL})({}))|(({DEC_LITERAL})\.[^._\p{{XID_Start}}]?)"#, *FLOAT_EXPONENT, *SUFFIX, *SUFFIX_NO_E);
    }

    proptest::proptest! {
        #[test]
        fn bin_literal(input in BIN_LITERAL) {
            parses_to!(&input, Rule::BinLiteral, [BinLiteral(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn oct_literal(input in OCT_LITERAL) {
            parses_to!(&input, Rule::OctLiteral, [OctLiteral(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn dec_literal(input in DEC_LITERAL) {
            parses_to!(&input, Rule::DecLiteral, [DecLiteral(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn hex_literal(input in HEX_LITERAL) {
            parses_to!(&input, Rule::HexLiteral, [HexLiteral(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn int_literal(input in string_regex(&INTEGER_LITERAL).unwrap()) {
            parses_to!(&input, Rule::IntegerLiteral, [IntegerLiteral(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn float_exponent(input in string_regex(&FLOAT_EXPONENT).unwrap()) {
            parses_to!(&input, Rule::FloatExponent, [FloatExponent(0, input.len(), [])]);
        }
    }

    proptest::proptest! {
        #[test]
        fn float_literal(input in string_regex(&FLOAT_LITERAL).unwrap()) {
            parses_to!(&input, Rule::FloatLiteral, [FloatLiteral(0, input.len(), [])]);
        }
    }

    const CHAR_LITERAL: &str = r#"'(([^'\\\n\r\t])|((\\')|(\\"))|(\\x[0-7][[:xdigit:]])|(\\n)|(\\r)|(\\t)|(\\\\)|(\\0)|(\\u\{[[:xdigit:]]{1,6}\}))'"#;

    proptest::proptest! {
        #[test]
        fn char_literal(c in CHAR_LITERAL) {
            parses_to!(&c, Rule::CharLiteral, [CharLiteral(0, c.len(), [])]);
        }
    }

    // proptest::proptest! {
    //     #[test]
    //     fn string_literal(c in r#"'(([^'\\\n\r\t])|((\\')|(\\"))|(\\x[0-7][0-9a-fA-F])|(\\n)|(\\r)|(\\t)|(\\\\)|(\\0)|(\\u\{[0-9a-fA-F]{1,6}\}))'"#) {
    //         parses_to!(&c, Rule::CharLiteral, [CharLiteral(0, c.len(), [])]);
    //     }
    // }
}
