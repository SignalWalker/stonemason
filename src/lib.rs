#[derive(pest_derive::Parser)]
#[grammar = "./stn.pest"]
pub struct StnParser;

pub const STRICT_KEYWORDS: &[&str] = &["mod", "dyn"];
pub const RESERVED_KEYWORDS: &[&str] = &["macro"];

#[cfg(test)]
mod tests {
    use proptest::{strategy::Strategy, string::string_regex};

    use super::{Rule, StnParser};
    use crate::{RESERVED_KEYWORDS, STRICT_KEYWORDS};

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

    const IDENTIFIER_OR_KEYWORD: &str = r#"(\p{XID_Start}\p{XID_Continue}*)|(_\p{XID_Continue}+)"#;
    const SUFFIX: &str = r#"\p{XID_Start}\p{XID_Continue}*"#;
    const SUFFIX_NO_E: &str = r#"[\p{XID_Start}&&[^eE]]\p{XID_Continue}*"#;
    lazy_static::lazy_static! {
        static ref RAW_IDENTIFIER: String = format!("r#({IDENTIFIER_OR_KEYWORD})");
        static ref NON_KEYWORD_IDENTIFIER: String = format!("({IDENTIFIER_OR_KEYWORD})");
        static ref IDENTIFIER: String = format!("({})|({})", *NON_KEYWORD_IDENTIFIER, *RAW_IDENTIFIER);
    }
    // IDENTIFIER

    proptest::proptest! {
        #[test]
        fn identifier_or_keyword(input in IDENTIFIER_OR_KEYWORD) {
            parses_to!(&input, Rule::IdentifierOrKeyword, [IdentifierOrKeyword(0, input.len(), [])]);
        }

        #[test]
        fn raw_identifier(input in string_regex(&RAW_IDENTIFIER).unwrap()) {
            parses_to!(&input, Rule::RawIdentifier, [RawIdentifier(0, input.len(), [])]);
        }

        #[test]
        fn non_keyword_identifier(input in string_regex(&NON_KEYWORD_IDENTIFIER).unwrap()
            .prop_filter("non-keyword identifiers must not be strict or reserved keywords", |i| {
                !STRICT_KEYWORDS.iter().any(|kw| i == kw) && !RESERVED_KEYWORDS.iter().any(|kw| i == kw)
            })
        ) {
            parses_to!(&input, Rule::NonKeywordIdentifier, [NonKeywordIdentifier(0, input.len(), [])]);
        }

        #[test]
        fn identifier(input in string_regex(&IDENTIFIER).unwrap()
            .prop_filter("non-keyword identifiers must not be strict or reserved keywords", |i| {
                !STRICT_KEYWORDS.iter().any(|kw| i == kw) && !RESERVED_KEYWORDS.iter().any(|kw| i == kw)
            })
        ) {
            parses_to!(&input, Rule::Identifier, [Identifier(0, input.len(), [])]);
        }

        #[test]
        fn isolated_cr(c in r#"\r[^\n]?"#) {
            parses_to!(&c, Rule::IsolatedCr, [IsolatedCr(0, 1, [])]);
        }

        #[test]
        fn suffix(input in SUFFIX) {
            parses_to!(&input, Rule::Suffix, [Suffix(0, input.len(), [])]);
        }

        #[test]
        fn suffix_no_e(input in SUFFIX_NO_E) {
            parses_to!(&input, Rule::SuffixNoE, [SuffixNoE(0, input.len(), [])]);
        }
    }

    // LITERAL

    // // NUMERIC

    const BIN_LITERAL: &str = r#"0b[01_]*[01][01_]*"#;
    const OCT_LITERAL: &str = r#"0o[0-7_]*[0-7][0-7_]*"#;
    const DEC_LITERAL: &str = r#"[[:digit:]][0-9_]*"#;
    const HEX_LITERAL: &str = r#"0x[0-9a-fA-F_]*[[:xdigit:]][0-9a-fA-F_]*"#;
    lazy_static::lazy_static! {
        static ref INTEGER_LITERAL: String = format!("(({DEC_LITERAL})|({BIN_LITERAL})|({OCT_LITERAL})|({HEX_LITERAL}))({SUFFIX_NO_E})?");

        static ref FLOAT_EXPONENT: String = format!("[eE][+-]?_*({DEC_LITERAL})");
        static ref FLOAT_LITERAL: String = format!(r#"(({DEC_LITERAL})(\.({DEC_LITERAL}))?({})({SUFFIX})?)|(({DEC_LITERAL})\.({DEC_LITERAL})({SUFFIX_NO_E}))|(({DEC_LITERAL})\.)"#, *FLOAT_EXPONENT);
    }

    proptest::proptest! {
        #[test]
        fn bin_literal(input in BIN_LITERAL) {
            parses_to!(&input, Rule::BinLiteral, [BinLiteral(0, input.len(), [])]);
        }

        #[test]
        fn oct_literal(input in OCT_LITERAL) {
            parses_to!(&input, Rule::OctLiteral, [OctLiteral(0, input.len(), [])]);
        }

        #[test]
        fn dec_literal(input in DEC_LITERAL) {
            parses_to!(&input, Rule::DecLiteral, [DecLiteral(0, input.len(), [])]);
        }

        #[test]
        fn hex_literal(input in HEX_LITERAL) {
            parses_to!(&input, Rule::HexLiteral, [HexLiteral(0, input.len(), [])]);
        }

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

        #[test]
        fn float_literal(input in string_regex(&FLOAT_LITERAL).unwrap()) {
            parses_to!(&input, Rule::FloatLiteral, [FloatLiteral(0, input.len(), [])]);
        }
    }

    const QUOTE_ESCAPE: &str = r#"\\'|\\""#;
    const ASCII_ESCAPE: &str = r#"(\\x[0-7][[:xdigit:]])|(\\n)|(\\r)|(\\t)|(\\\\)|(\\0)"#;
    const UNICODE_ESCAPE: &str = r#"\\u\{[[:xdigit:]]{1,6}\}"#;
    lazy_static::lazy_static! {
        static ref CHAR_LITERAL: String = format!(r#"'(([^'\\\n\r\t])|({QUOTE_ESCAPE})|({ASCII_ESCAPE})|({UNICODE_ESCAPE}))'({SUFFIX})?"#);
    }

    proptest::proptest! {
        #[test]
        fn quote_escape(input in QUOTE_ESCAPE) {
            parses_to!(&input, Rule::QuoteEscape, [QuoteEscape(0, input.len(), [])]);
        }

        #[test]
        fn ascii_escape(input in ASCII_ESCAPE) {
            parses_to!(&input, Rule::AsciiEscape, [AsciiEscape(0, input.len(), [])]);
        }

        #[test]
        fn unicode_escape(input in UNICODE_ESCAPE) {
            parses_to!(&input, Rule::UnicodeEscape, [UnicodeEscape(0, input.len(), [])]);
        }

        #[test]
        fn char_literal(input in string_regex(&CHAR_LITERAL).unwrap()) {
            parses_to!(&input, Rule::CharLiteral, [CharLiteral(0, input.len(), [])]);
        }
    }

    const ASCII_FOR_CHAR: &str = r#"[[:ascii:]&&[^'\\\n\r\t]]"#;
    const BYTE_ESCAPE: &str =
        r#"(\\x[[:xdigit:]][[:xdigit:]])|(\\n)|(\\r)|(\\t)|(\\\\)|(\\0)|(\\')|(\\")"#;
    lazy_static::lazy_static! {
        static ref BYTE_LITERAL: String = format!(r#"b'(({ASCII_FOR_CHAR})|({BYTE_ESCAPE}))'({SUFFIX})?"#);
    }
    proptest::proptest! {
        #[test]
        fn ascii_for_char(input in ASCII_FOR_CHAR) {
            parses_to!(&input, Rule::AsciiForChar, [AsciiForChar(0, input.len(), [])]);
        }

        #[test]
        fn byte_escape(input in BYTE_ESCAPE) {
            parses_to!(&input, Rule::ByteEscape, [ByteEscape(0, input.len(), [])]);
        }

        #[test]
        fn byte_literal(input in string_regex(&BYTE_LITERAL).unwrap()) {
            parses_to!(&input, Rule::ByteLiteral, [ByteLiteral(0, input.len(), [])]);
        }
    }

    const STRING_CONTINUE: &str = r#"\\\n"#;
    lazy_static::lazy_static! {
        static ref STRING_LITERAL: String = format!(r#""(([^"\\\r])|({QUOTE_ESCAPE})|({ASCII_ESCAPE})|({UNICODE_ESCAPE})|({STRING_CONTINUE}))*"({SUFFIX})?"#);
    }

    proptest::proptest! {
        #[test]
        fn string_continue(input in STRING_CONTINUE) {
            parses_to!(&input, Rule::StringContinue, [StringContinue(0, input.len(), [])]);
        }

        #[test]
        fn string_literal(input in string_regex(&STRING_LITERAL).unwrap()) {
            parses_to!(&input, Rule::StringLiteral, [StringLiteral(0, input.len(), [])]);
        }
    }

    const ASCII_FOR_STRING: &str = r#"[\p{ASCII}&&[^"\\\r]]"#;
    lazy_static::lazy_static! {
        static ref BYTE_STRING_LITERAL: String = format!(r#"b"(({ASCII_FOR_STRING})|({BYTE_ESCAPE})|({STRING_CONTINUE}))*"({SUFFIX})?"#);
    }

    proptest::proptest! {
        #[test]
        fn ascii_for_string(input in ASCII_FOR_STRING) {
            parses_to!(&input, Rule::AsciiForString, [AsciiForString(0, input.len(), [])]);
        }

        #[test]
        fn byte_string_literal(input in string_regex(&BYTE_STRING_LITERAL).unwrap()) {
            parses_to!(&input, Rule::ByteStringLiteral, [ByteStringLiteral(0, input.len(), [])]);
        }
    }

    const LINE_COMMENT: &str = r#"(//([^/!\n]|(//))[^\n]*)|(//)"#;
    const INNER_LINE_DOC: &str = r#"//![^\n\r]*"#;
    const OUTER_LINE_DOC: &str = r#"///([^/][^\n\r]*)?"#;
    const WHITESPACE: &str = r#"\p{Pat_WS}"#;
    proptest::proptest! {
        #[test]
        fn line_comment(input in LINE_COMMENT) {
            parses_to!(&input, Rule::LineComment, [LineComment(0, input.len(), [])]);
        }

        #[test]
        fn inner_line_doc(input in INNER_LINE_DOC) {
            parses_to!(&input, Rule::InnerLineDoc, [InnerLineDoc(0, input.len(), [])]);
        }

        #[test]
        fn outer_line_doc(input in OUTER_LINE_DOC) {
            parses_to!(&input, Rule::OuterLineDoc, [OuterLineDoc(0, input.len(), [])]);
        }

        #[test]
        fn whitespace(input in WHITESPACE) {
            parses_to!(&input, Rule::WHITESPACE, []);
        }
    }

    // proptest::proptest! {
    //     #[test]
    //     fn string_literal(c in r#"'(([^'\\\n\r\t])|((\\')|(\\"))|(\\x[0-7][0-9a-fA-F])|(\\n)|(\\r)|(\\t)|(\\\\)|(\\0)|(\\u\{[0-9a-fA-F]{1,6}\}))'"#) {
    //         parses_to!(&c, Rule::CharLiteral, [CharLiteral(0, c.len(), [])]);
    //     }
    // }
}
