use nom::{
    bytes::complete::{tag, take_while, take_while1},
    character::complete::satisfy,
    combinator::{eof, opt, peek, recognize, value},
    error::{ErrorKind, ParseError},
    AsChar, IResult, InputTakeAtPosition, Parser,
};

use nom::character::complete as nchar;
use unicode_ident::{is_xid_continue, is_xid_start};

use super::suffix;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum IntegerType {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct IntegerLiteral<'data> {
    ty: IntegerType,
    src: &'data str,
    suffix: Option<&'data str>,
}

#[inline]
pub fn is_bin_digit(d: impl AsChar) -> bool {
    let d = d.as_char();
    d == '0' || d == '1'
}

#[inline]
pub fn is_bin_char(d: impl AsChar + Copy) -> bool {
    is_bin_digit(d) || d.as_char() == '_'
}

#[inline]
pub fn is_oct_char(d: impl AsChar + Copy) -> bool {
    d.is_oct_digit() || d.as_char() == '_'
}

#[inline]
pub fn is_dec_char(d: impl AsChar + Copy) -> bool {
    d.is_dec_digit() || d.as_char() == '_'
}

#[inline]
pub fn is_hex_char(d: impl AsChar + Copy) -> bool {
    d.is_hex_digit() || d.as_char() == '_'
}

pub fn bin_digit0<T: InputTakeAtPosition, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| !is_bin_digit(item))
}

pub fn bin_digit1<T: InputTakeAtPosition, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(|item| !is_bin_digit(item), ErrorKind::Digit)
}

pub fn numeric_suffix(input: &str) -> IResult<&str, &str> {
    recognize(
        satisfy(|c| c != 'e' && c != 'E' && is_xid_start(c))
            .and(take_while(is_xid_continue))
            .or(nchar::char('_').and(take_while1(is_xid_continue))),
    )(input)
}

pub fn num_literal<'i, Error: ParseError<&'i str>>(
    is_digit: impl 'i + Copy + Fn(char) -> bool,
) -> impl Parser<&'i str, &'i str, Error> {
    recognize(
        take_while(|c| c == '_')
            .and(satisfy(is_digit))
            .and(take_while(move |c| c == '_' || is_digit(c))),
    )
}

pub fn num_literal_no_prefix<'i, Error: ParseError<&'i str>>(
    is_digit: impl 'i + Copy + Fn(char) -> bool,
) -> impl Parser<&'i str, &'i str, Error> {
    recognize(satisfy(is_digit).and(take_while(move |c| c == '_' || is_digit(c))))
}

pub fn prefixed_numeric<'i, Error: ParseError<&'i str>>(
    prefix: &'static str,
    ty: IntegerType,
    is_digit: impl 'i + Copy + Fn(char) -> bool,
) -> impl Parser<&'i str, IntegerLiteral<'i>, Error> {
    tag(prefix)
        .and(num_literal(is_digit))
        .map(move |(_, src)| IntegerLiteral {
            ty,
            src,
            suffix: None,
        })
}

pub fn bin_literal(input: &str) -> IResult<&str, IntegerLiteral> {
    prefixed_numeric("0b", IntegerType::Bin, is_bin_digit).parse(input)
}

pub fn oct_literal(input: &str) -> IResult<&str, IntegerLiteral> {
    prefixed_numeric("0o", IntegerType::Oct, AsChar::is_oct_digit).parse(input)
}

pub fn hex_literal(input: &str) -> IResult<&str, IntegerLiteral> {
    prefixed_numeric("0x", IntegerType::Hex, AsChar::is_hex_digit).parse(input)
}
pub fn dec_literal(input: &str) -> IResult<&str, IntegerLiteral> {
    num_literal_no_prefix(AsChar::is_dec_digit)
        .map(|src| IntegerLiteral {
            ty: IntegerType::Dec,
            src,
            suffix: None,
        })
        .parse(input)
}
//
pub fn int_literal(input: &str) -> IResult<&str, IntegerLiteral> {
    hex_literal
        .or(oct_literal)
        .or(bin_literal)
        .or(dec_literal)
        .and(opt(numeric_suffix))
        .map(|(mut lit, suf)| {
            lit.suffix = suf;
            lit
        })
        .parse(input)
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub struct FloatExponent<'data> {
    sign: Option<bool>,
    value: &'data str,
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub struct FloatLiteral<'data> {
    int: &'data str,
    frac: Option<&'data str>,
    exp: Option<FloatExponent<'data>>,
    suffix: Option<&'data str>,
}

pub fn float_exponent(input: &str) -> IResult<&str, FloatExponent> {
    satisfy(|c| c == 'e' || c == 'E')
        .and(opt(
            value(true, nchar::char('+')).or(value(false, nchar::char('-')))
        ))
        .and(take_while(|c| c == '_'))
        .and(recognize(dec_literal))
        .map(|(((_, sign), _), value)| FloatExponent { sign, value })
        .parse(input)
}

pub fn float_literal(input: &str) -> IResult<&str, FloatLiteral> {
    (recognize(dec_literal)
        .and(opt(nchar::char('.').and(recognize(dec_literal))))
        .and(float_exponent)
        .and(opt(suffix))
        .map(|(((int, frac), exp), suffix)| FloatLiteral {
            int,
            frac: frac.map(|(_, frac)| frac),
            exp: Some(exp),
            suffix,
        }))
    .or(recognize(dec_literal)
        .and(nchar::char('.'))
        .and(recognize(dec_literal))
        .and(opt(numeric_suffix))
        .map(|(((int, _), frac), suffix)| FloatLiteral {
            int,
            frac: Some(frac),
            exp: None,
            suffix,
        }))
    .or(recognize(dec_literal)
        .and(nchar::char('.'))
        .and(eof.or(peek(recognize(satisfy(|c| {
            c != '.' && c != '_' && !is_xid_start(c)
        })))))
        .map(|((int, _), _)| FloatLiteral {
            int,
            frac: None,
            exp: None,
            suffix: None,
        }))
    .parse(input)
}

#[cfg(test)]
pub(crate) mod tests {
    use proptest::string::string_regex;

    use crate::{
        de::parse::{float_literal, FloatExponent, FloatLiteral, IntegerLiteral, IntegerType},
        test_parse, test_parse_complex,
    };

    const BIN_LITERAL: &str = r#"0b[01_]*[01]+[01_]*"#;
    const OCT_LITERAL: &str = r#"0o[0-7_]*[0-7]+[0-7_]*"#;
    const DEC_LITERAL: &str = r#"[[:digit:]]+[[:digit:]_]*"#;
    const HEX_LITERAL: &str = r#"0x[[:xdigit:]_]*[[:xdigit:]]+[[:xdigit:]_]*"#;

    lazy_static::lazy_static! {
        static ref INT_LITERAL: String = format!(r#"({DEC_LITERAL})|({BIN_LITERAL})|({OCT_LITERAL})|({HEX_LITERAL})"#);
    }

    const NUMERIC_SUFFIX: &str =
        r#"([\p{XID_Start}&&[^eE]]\p{XID_Continue}*)|(_\p{XID_Continue}+)"#;

    test_parse!(numeric_suffix, NUMERIC_SUFFIX);

    test_parse!(bin_literal, BIN_LITERAL; input => IntegerLiteral { ty: IntegerType::Bin, src: &input[2..], suffix: None });
    test_parse!(oct_literal, OCT_LITERAL; input => IntegerLiteral { ty: IntegerType::Oct, src: &input[2..], suffix: None });
    test_parse!(hex_literal, HEX_LITERAL; input => IntegerLiteral { ty: IntegerType::Hex, src: &input[2..], suffix: None });
    test_parse!(dec_literal, DEC_LITERAL; input => IntegerLiteral { ty: IntegerType::Dec, src: &input, suffix: None });

    test_parse_complex!(int_literal;
        value in string_regex(&INT_LITERAL).unwrap()
        => &format!("{value}") => ""; {
            let (ty, src) = match value.get(0..2) {
                Some("0b") => (IntegerType::Bin, &value[2..]),
                Some("0o") => (IntegerType::Oct, &value[2..]),
                Some("0x") => (IntegerType::Hex, &value[2..]),
                _ => (IntegerType::Dec, value.as_str()),
            }; IntegerLiteral {
            ty,
            src,
            suffix: None // TODO
        }
        }
    );

    // const FLOAT_EXPONENT: &str = r#"[eE][+-]?_*[[:digit:]]+[[:digit:]_]*"#;
    test_parse_complex!(float_exponent;
        prefix in "[eE]",
        sign in "[+-]?",
        filler in "_*",
        value in DEC_LITERAL
        => &format!("{prefix}{sign}{filler}{value}")
        => ""; FloatExponent {
            sign: match sign.as_str() {
                "+" => Some(true),
                "-" => Some(false),
                "" => None,
                _ => unreachable!()
            },
            value: value.as_str()
        }
    );

    test_parse_complex!(float_literal_a, float_literal;
        int in DEC_LITERAL,
        frac in string_regex(&format!(r#"(\.({DEC_LITERAL}))?"#)).unwrap(),
        exp_prefix in "[eE]",
        exp_sign in "[+-]?",
        exp_filler in "_*",
        exp_value in DEC_LITERAL
        => &format!("{int}{frac}{exp_prefix}{exp_sign}{exp_filler}{exp_value}")
        => ""; FloatLiteral {
            int: int.as_str(),
            frac: match frac.as_str() {
                "" => None,
                _ => Some(&frac[1..])
            },
            exp: Some(FloatExponent {
                sign: match exp_sign.as_str() {
                    "+" => Some(true),
                    "-" => Some(false),
                    "" => None,
                    _ => unreachable!()
                },
                value: exp_value.as_str()
            }),
            suffix: None // TODO
        }
    );

    test_parse_complex!(float_literal_b, float_literal;
        int in DEC_LITERAL,
        frac in DEC_LITERAL
        => &format!("{int}.{frac}")
        => ""; FloatLiteral {
            int: int.as_str(),
            frac: Some(frac.as_str()),
            exp: None,
            suffix: None // TODO
        }
    );

    test_parse_complex!(float_literal_c, float_literal;
        int in DEC_LITERAL,
        end in r#"([^[:digit:]._\p{XID_Start}])?"#
        => &format!("{int}.{end}")
        => end.as_str(); FloatLiteral {
            int: int.as_str(),
            frac: None,
            exp: None,
            suffix: None // TODO
        }
    );
}
