use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, char, one_of};
use nom::combinator::{map, not, recognize, value};
use nom::multi::{count, many0, many1};
use nom::sequence::{delimited, preceded, terminated};

fn decimal_digit(input: &str) -> IResult<&str, char> {
    one_of("0123456789")(input)
}

fn hex_digit(input: &str) -> IResult<&str, char> {
    one_of("0123456789abcdefABCDEF")(input)
}

fn line_terminator(input: &str) -> IResult<&str, char> {
    one_of("\n\r\u{2028}\u{2029}")(input)
}

pub mod string_literal {

    use super::*;

    pub fn parse(input: &str) -> IResult<&str, String> {
        map(
            alt((
                delimited(char('"'), double_string_characters, char('"')),
                delimited(char('\''), single_string_characters, char('\'')),
            )),
            |chars| chars.iter().collect(),
        )(input)
    }

    fn double_string_characters(input: &str) -> IResult<&str, Vec<char>> {
        many0(double_string_character)(input)
    }

    fn single_string_characters(input: &str) -> IResult<&str, Vec<char>> {
        many0(single_string_character)(input)
    }

    fn double_string_character(input: &str) -> IResult<&str, char> {
        alt((
            preceded(not(alt((one_of("\"\\"), line_terminator))), anychar),
            char('\u{2028}'), // <LS>
            char('\u{2029}'), // <PS>
            preceded(char('\\'), escape_sequence),
            // TODO
            // LineContinuation
        ))(input)
    }

    fn single_string_character(input: &str) -> IResult<&str, char> {
        alt((
            preceded(not(alt((one_of("'\\"), line_terminator))), anychar),
            char('\u{2028}'), // <LS>
            char('\u{2029}'), // <PS>
            preceded(char('\\'), escape_sequence),
            // TODO:
            // LineContinuation
        ))(input)
    }

    fn escape_sequence(input: &str) -> IResult<&str, char> {
        alt((
            character_escape_sequence,
            terminated(char('0'), not(decimal_digit)),
            // TODO:
            // LegacyOctalEscapeSequence
            // NonOctalDecimalEscapeSequence
            // HexEscapeSequence
            unicode_escape_sequence,
        ))(input)
    }

    fn character_escape_sequence(input: &str) -> IResult<&str, char> {
        alt((single_escape_character, non_escape_character))(input)
    }

    fn single_escape_character(input: &str) -> IResult<&str, char> {
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

    fn non_escape_character(input: &str) -> IResult<&str, char> {
        preceded(not(alt((escape_character, line_terminator))), anychar)(input)
    }

    fn escape_character(input: &str) -> IResult<&str, char> {
        alt((single_escape_character, decimal_digit, char('x'), char('u')))(input)
    }

    fn unicode_escape_sequence(input: &str) -> IResult<&str, char> {
        alt((
            preceded(char('u'), hex_4_digits),
            delimited(tag("u{"), code_point, char('}')),
        ))(input)
    }

    fn hex_4_digits(input: &str) -> IResult<&str, char> {
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
    }
}

// Template Literal Lexical Components
fn code_point(input: &str) -> IResult<&str, char> {
    map(recognize(many1(hex_digit)), |digits| {
        let i = u32::from_str_radix(digits, 16).unwrap();
        if i > 0x10FFFF {
            // TODO: error handling
            panic!("Invalid code point");
        }
        char::from_u32(i).unwrap()
    })(input)
}
