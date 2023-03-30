use nom::{
    branch::alt,
    bytes::streaming::{is_not, tag},
    character::streaming::none_of,
    sequence::{pair, tuple},
    IResult,
};

// pub fn line_comment(s: &str) -> IResult<&str, &str> {
//     let res = alt((
//         pair(
//             tag("//"),
//             tuple((alt((none_of("/!\n"), tag("//"))), is_not("\n"))),
//         ),
//         tag("//"),
//     ))(s);
//     todo!()
// }
