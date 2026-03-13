use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, char, one_of};
use nom::combinator::{map, not, peek, recognize, value};
use nom::multi::{count, fold_many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

use crate::lexer::SyntaxError;

impl nom::error::ParseError<&str> for SyntaxError {
    fn from_error_kind(input: &str, _kind: nom::error::ErrorKind) -> Self {
        Self::new(format!("Invalid input: {}", input))
    }

    fn append(_input: &str, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

fn octal_digit(input: &str) -> IResult<&str, char, SyntaxError> {
    one_of("01234567")(input)
}

fn decimal_digit(input: &str) -> IResult<&str, char, SyntaxError> {
    one_of("0123456789")(input)
}

fn hex_digit(input: &str) -> IResult<&str, char, SyntaxError> {
    one_of("0123456789abcdefABCDEF")(input)
}

fn line_terminator(input: &str) -> IResult<&str, char, SyntaxError> {
    one_of("\n\r\u{2028}\u{2029}")(input)
}

pub mod string_literal {
    use super::*;

    pub fn parse(input: &str) -> IResult<&str, String, SyntaxError> {
        map(
            alt((
                delimited(char('"'), double_string_characters, char('"')),
                delimited(char('\''), single_string_characters, char('\'')),
            )),
            |chars| chars.iter().collect(),
        )(input)
    }

    fn double_string_characters(input: &str) -> IResult<&str, Vec<char>, SyntaxError> {
        fold_many0(double_string_character, Vec::new, |mut acc, item| {
            if let Some(c) = item {
                acc.push(c);
            }
            acc
        })(input)
    }

    fn single_string_characters(input: &str) -> IResult<&str, Vec<char>, SyntaxError> {
        fold_many0(single_string_character, Vec::new, |mut acc, item| {
            if let Some(c) = item {
                acc.push(c);
            }
            acc
        })(input)
    }

    fn double_string_character(input: &str) -> IResult<&str, Option<char>, SyntaxError> {
        let parse = alt((
            preceded(not(alt((one_of("\"\\"), line_terminator))), anychar),
            char('\u{2028}'), // <LS>
            char('\u{2029}'), // <PS>
            preceded(char('\\'), escape_sequence),
        ));
        alt((map(parse, |c| Some(c)), line_continuation))(input)
    }

    fn single_string_character(input: &str) -> IResult<&str, Option<char>, SyntaxError> {
        let parse = alt((
            preceded(not(alt((one_of("'\\"), line_terminator))), anychar),
            char('\u{2028}'), // <LS>
            char('\u{2029}'), // <PS>
            preceded(char('\\'), escape_sequence),
        ));
        alt((map(parse, |c| Some(c)), line_continuation))(input)
    }

    fn line_continuation(input: &str) -> IResult<&str, Option<char>, SyntaxError> {
        map(preceded(char('\\'), line_terminator_sequence), |_| None)(input)
    }

    fn line_terminator_sequence(input: &str) -> IResult<&str, &str, SyntaxError> {
        alt((
            tag("\n"),
            terminated(tag("\r"), not(char('\n'))),
            tag("\u{2028}"),
            tag("\u{2029}"),
            tag("\r\n"),
        ))(input)
    }

    fn escape_sequence(input: &str) -> IResult<&str, char, SyntaxError> {
        alt((
            character_escape_sequence,
            terminated(char('0'), not(decimal_digit)),
            legacy_octal_escape_sequence,
            // TODO:
            // NonOctalDecimalEscapeSequence
            // HexEscapeSequence
            unicode_escape_sequence,
        ))(input)
    }

    fn character_escape_sequence(input: &str) -> IResult<&str, char, SyntaxError> {
        alt((single_escape_character, non_escape_character))(input)
    }

    fn single_escape_character(input: &str) -> IResult<&str, char, SyntaxError> {
        // one_of("'\"\\bfnrtv")(input)
        alt((
            char('\''),
            char('"'),
            char('\\'),
            value('\u{0008}', char('b')),
            value('\u{000C}', char('f')),
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{000B}', char('v')),
        ))(input)
    }

    fn non_escape_character(input: &str) -> IResult<&str, char, SyntaxError> {
        preceded(not(alt((escape_character, line_terminator))), anychar)(input)
    }

    fn escape_character(input: &str) -> IResult<&str, char, SyntaxError> {
        alt((single_escape_character, decimal_digit, char('x'), char('u')))(input)
    }

    fn legacy_octal_escape_sequence(input: &str) -> IResult<&str, char, SyntaxError> {
        alt((
            terminated(char('0'), peek(one_of("89"))),
            terminated(non_zero_octal_digit, not(octal_digit)),
            terminated(
                map(
                    pair(one_of("0123"), octal_digit),
                    |(c1, c2): (char, char)| {
                        let i1 = c1.to_digit(8).unwrap();
                        let i2 = c2.to_digit(8).unwrap();
                        char::from_u32(i1 * 8 + i2).unwrap()
                    },
                ),
                not(octal_digit),
            ),
            map(
                pair(one_of("4567"), octal_digit),
                |(c1, c2): (char, char)| {
                    let i1 = c1.to_digit(8).unwrap();
                    let i2 = c2.to_digit(8).unwrap();
                    char::from_u32(i1 * 8 + i2).unwrap()
                },
            ),
            map(
                tuple((one_of("0123"), octal_digit, octal_digit)),
                |(c1, c2, c3)| {
                    let i1 = c1.to_digit(8).unwrap();
                    let i2 = c2.to_digit(8).unwrap();
                    let i3 = c3.to_digit(8).unwrap();
                    char::from_u32(i1 * 64 + i2 * 8 + i3).unwrap()
                },
            ),
        ))(input)
    }

    fn non_zero_octal_digit(input: &str) -> IResult<&str, char, SyntaxError> {
        one_of("1234567")(input)
    }

    fn unicode_escape_sequence(input: &str) -> IResult<&str, char, SyntaxError> {
        alt((
            preceded(char('u'), hex_4_digits),
            delimited(tag("u{"), template_literal::code_point, char('}')),
        ))(input)
    }

    fn hex_4_digits(input: &str) -> IResult<&str, char, SyntaxError> {
        map(recognize(count(hex_digit, 4)), |digits| {
            let i = u32::from_str_radix(digits, 16).unwrap();
            char::from_u32(i).unwrap()
        })(input)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_unicode_escape_sequence() {
            assert_eq!(unicode_escape_sequence("u0041"), Ok(("", 'A')));
            assert_eq!(unicode_escape_sequence("u{1F4A9}"), Ok(("", '💩')));
        }

        #[test]
        fn test_hex_4_digits() {
            assert_eq!(hex_4_digits("0041"), Ok(("", 'A')));
        }

        #[test]
        fn test_line_continuation() {
            assert_eq!(line_continuation("\\\n"), Ok(("", None)));
            assert_eq!(line_continuation("\\\r"), Ok(("", None)));
            assert_eq!(line_continuation("\\\r\n"), Ok(("", None)));
        }

        #[test]
        fn test_string_with_line_continuation() {
            assert_eq!(
                parse("\"hello\\\nworld\""),
                Ok(("", "helloworld".to_string()))
            );
        }

        #[test]
        fn test_legacy_octal_escape_sequence() {
            assert_eq!(legacy_octal_escape_sequence("08"), Ok(("8", '0')));
            assert_eq!(legacy_octal_escape_sequence("1abc"), Ok(("abc", '1')));
            assert_eq!(
                legacy_octal_escape_sequence("37abc"),
                Ok(("abc", '\u{01F}'))
            );
            assert_eq!(legacy_octal_escape_sequence("43"), Ok(("", '\u{23}')));
            assert_eq!(
                legacy_octal_escape_sequence("345abc"),
                Ok(("abc", '\u{E5}'))
            );
        }
    }
}

mod template_literal {
    use super::*;

    pub fn code_point(input: &str) -> IResult<&str, char, SyntaxError> {
        recognize(many1(hex_digit))(input).and_then(|(output, digits)| {
            let i = u32::from_str_radix(digits, 16).unwrap();
            if i > 0x10FFFF {
                return Err(nom::Err::Failure(
                    SyntaxError::new("Undefined Unicode code-point".to_string())
                        .with_detail(input.len(), digits.len()),
                ));
            }
            Ok((output, char::from_u32(i).unwrap()))
        })
    }
}
