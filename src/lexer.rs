use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_while, take_while1},
    character::complete::{anychar, char, one_of},
    combinator::{map, opt, recognize},
    sequence::{delimited, pair},
};
use thiserror::Error;

use crate::ast::{Position, SourceLocation};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Null,
    Boolean(bool),
    Number(f64),
    StringLit(String),
    Ident(String),

    // Keywords
    Var,
    Let,
    Const,
    If,
    Else,
    Function,
    Return,

    // Operators
    Plus,  // +
    Minus, // -
    Star,  // *
    Slash, // /

    Semi,   // ;
    LBrace, // {
    RBrace, // }
    LParen, // (
    RParen, // )
    Colon,  // :
    Comma,  // ,
    Assign, // =

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
    pub has_line_break: bool, // True if there's a LineTerminator after this token
}

#[derive(Debug, Error)]
#[error("Invalid token: {0}")]
pub struct LexerError(String);

pub struct Lexer {
    pos: Position,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            pos: Position::new(1, 0),
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Result<Vec<Token>, LexerError> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut remaining = input;
        let mut finished = false;

        while !finished {
            remaining = self.skip_whitespace(remaining);

            if let Ok((rest, _)) = self.parse_line_terminator(remaining) {
                self.pos.line += 1;
                self.pos.column = 0;
                remaining = rest;
                if let Some(token) = tokens.last_mut() {
                    token.has_line_break = true;
                }
                continue;
            }

            let start = self.pos;
            let (rest, kind) = self.parse_token(remaining)?;
            let end = self.pos;
            remaining = rest;

            if kind == TokenKind::Eof {
                finished = true;
            }

            tokens.push(Token {
                kind,
                loc: SourceLocation { start, end },
                has_line_break: false,
            });
        }

        Ok(tokens)
    }

    fn skip_whitespace<'a>(&mut self, input: &'a str) -> &'a str {
        let is_whitespace_but_not_line_terminator =
            |c: char| c.is_whitespace() && c != '\n' && c != '\r';

        take_while::<_, &str, ()>(is_whitespace_but_not_line_terminator)(input).map_or(
            input,
            |(rest, sp)| {
                self.pos.column += sp.len();
                rest
            },
        )
    }

    fn parse_token<'a>(&mut self, input: &'a str) -> Result<(&'a str, TokenKind), LexerError> {
        if input.is_empty() {
            return Ok((input, TokenKind::Eof));
        }

        if input.chars().next().unwrap().is_ascii_digit() {
            return self
                .parse_number(input)
                .map_err(|_| LexerError(input.to_string()));
        }

        if input.starts_with('"') || input.starts_with('\'') {
            return self
                .parse_string(input)
                .map_err(|_| LexerError(input.to_string()));
        }

        if input.chars().next().unwrap().is_alphabetic()
            || input.starts_with('_')
            || input.starts_with('$')
        {
            return self
                .parse_identifier(input)
                .map_err(|_| LexerError(input.to_string()));
        }

        let ch = input.chars().next().unwrap();
        let (input, kind) = match ch {
            '+' => (&input[1..], TokenKind::Plus),
            '-' => (&input[1..], TokenKind::Minus),
            '*' => (&input[1..], TokenKind::Star),
            '/' => (&input[1..], TokenKind::Slash),
            ';' => (&input[1..], TokenKind::Semi),
            '{' => (&input[1..], TokenKind::LBrace),
            '}' => (&input[1..], TokenKind::RBrace),
            '(' => (&input[1..], TokenKind::LParen),
            ')' => (&input[1..], TokenKind::RParen),
            ':' => (&input[1..], TokenKind::Colon),
            ',' => (&input[1..], TokenKind::Comma),
            '=' => (&input[1..], TokenKind::Assign),
            c => return Err(LexerError(c.to_string())),
        };
        self.pos.column += 1;

        Ok((input, kind))
    }

    fn parse_line_terminator<'a>(&mut self, input: &'a str) -> IResult<&'a str, &'a str> {
        let crlf = tag("\r\n");
        let lf = tag("\n");
        let cr = tag("\r");
        alt((crlf, lf, cr))(input)
    }

    fn parse_number<'a>(&mut self, input: &'a str) -> IResult<&'a str, TokenKind> {
        let integer = take_while1(|c: char| c.is_ascii_digit());
        let fractional = opt(pair(char('.'), take_while(|c: char| c.is_ascii_digit())));
        let number = recognize(pair(integer, fractional));

        map(number, |num_str: &str| {
            self.pos.column += num_str.len();
            TokenKind::Number(num_str.parse().unwrap())
        })(input)
    }

    fn parse_string<'a>(&mut self, input: &'a str) -> IResult<&'a str, TokenKind> {
        let double_quoted = delimited(
            char('"'),
            opt(escaped(is_not("\"\\"), '\\', anychar))
                .map(|s: Option<&str>| s.unwrap_or("").to_string()),
            char('"'),
        );

        let single_quoted = delimited(
            char('\''),
            opt(escaped(is_not("'\\"), '\\', anychar))
                .map(|s: Option<&str>| s.unwrap_or("").to_string()),
            char('\''),
        );

        map(alt((double_quoted, single_quoted)), |s: String| {
            self.pos.column += s.len();
            TokenKind::StringLit(s)
        })(input)
    }

    fn parse_identifier<'a>(&mut self, input: &'a str) -> IResult<&'a str, TokenKind> {
        let first = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$");
        let rest = take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '$');

        map(recognize(pair(first, rest)), |ident: &str| {
            self.pos.column += ident.len();
            match ident {
                "null" => TokenKind::Null,
                "true" => TokenKind::Boolean(true),
                "false" => TokenKind::Boolean(false),
                "var" => TokenKind::Var,
                "let" => TokenKind::Let,
                "const" => TokenKind::Const,
                "if" => TokenKind::If,
                "else" => TokenKind::Else,
                "function" => TokenKind::Function,
                "return" => TokenKind::Return,
                _ => TokenKind::Ident(ident.to_string()),
            }
        })(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl From<(usize, usize)> for Position {
        fn from(value: (usize, usize)) -> Self {
            Self {
                line: value.0,
                column: value.1,
            }
        }
    }

    fn new_token(kind: TokenKind, start: (usize, usize), end: (usize, usize)) -> Token {
        Token {
            kind,
            loc: SourceLocation {
                start: start.into(),
                end: end.into(),
            },
            has_line_break: false,
        }
    }

    #[test]
    fn test_variable() {
        let input = "var num = 100;";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                new_token(TokenKind::Var, (1, 0), (1, 3)),
                new_token(TokenKind::Ident("num".to_string()), (1, 4), (1, 7)),
                new_token(TokenKind::Assign, (1, 8), (1, 9)),
                new_token(TokenKind::Number(100.0), (1, 10), (1, 13)),
                new_token(TokenKind::Semi, (1, 13), (1, 14)),
                new_token(TokenKind::Eof, (1, 14), (1, 14)),
            ]
        )
    }

    #[test]
    fn test_numbers() {
        let input = "42 3.14 0.5 100";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                new_token(TokenKind::Number(42.0), (1, 0), (1, 2)),
                new_token(TokenKind::Number(3.14), (1, 3), (1, 7)),
                new_token(TokenKind::Number(0.5), (1, 8), (1, 11)),
                new_token(TokenKind::Number(100.0), (1, 12), (1, 15)),
                new_token(TokenKind::Eof, (1, 15), (1, 15)),
            ]
        )
    }

    #[test]
    fn test_strings() {
        let input = r#""hello" 'world' "test \"quoted\"" 'it\'s'"#;
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                new_token(TokenKind::StringLit("hello".to_string()), (1, 0), (1, 5)),
                new_token(TokenKind::StringLit("world".to_string()), (1, 6), (1, 11)),
                new_token(
                    TokenKind::StringLit("test \\\"quoted\\\"".to_string()),
                    (1, 12),
                    (1, 27)
                ),
                new_token(TokenKind::StringLit("it\\'s".to_string()), (1, 28), (1, 33)),
                new_token(TokenKind::Eof, (1, 33), (1, 33)),
            ]
        )
    }

    #[test]
    fn test_identifiers() {
        let input = "foo bar _private $special var123";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                new_token(TokenKind::Ident("foo".to_string()), (1, 0), (1, 3)),
                new_token(TokenKind::Ident("bar".to_string()), (1, 4), (1, 7)),
                new_token(TokenKind::Ident("_private".to_string()), (1, 8), (1, 16)),
                new_token(TokenKind::Ident("$special".to_string()), (1, 17), (1, 25)),
                new_token(TokenKind::Ident("var123".to_string()), (1, 26), (1, 32)),
                new_token(TokenKind::Eof, (1, 32), (1, 32)),
            ]
        )
    }

    #[test]
    fn test_keywords() {
        let input = "var let const if else function return null true false";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                new_token(TokenKind::Var, (1, 0), (1, 3)),
                new_token(TokenKind::Let, (1, 4), (1, 7)),
                new_token(TokenKind::Const, (1, 8), (1, 13)),
                new_token(TokenKind::If, (1, 14), (1, 16)),
                new_token(TokenKind::Else, (1, 17), (1, 21)),
                new_token(TokenKind::Function, (1, 22), (1, 30)),
                new_token(TokenKind::Return, (1, 31), (1, 37)),
                new_token(TokenKind::Null, (1, 38), (1, 42)),
                new_token(TokenKind::Boolean(true), (1, 43), (1, 47)),
                new_token(TokenKind::Boolean(false), (1, 48), (1, 53)),
                new_token(TokenKind::Eof, (1, 53), (1, 53)),
            ]
        )
    }

    #[test]
    fn test_operators() {
        let input = "+ - * /";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                new_token(TokenKind::Plus, (1, 0), (1, 1)),
                new_token(TokenKind::Minus, (1, 2), (1, 3)),
                new_token(TokenKind::Star, (1, 4), (1, 5)),
                new_token(TokenKind::Slash, (1, 6), (1, 7)),
                new_token(TokenKind::Eof, (1, 7), (1, 7)),
            ]
        )
    }

    #[test]
    fn test_punctuation() {
        let input = "; { } ( ) : , =";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                new_token(TokenKind::Semi, (1, 0), (1, 1)),
                new_token(TokenKind::LBrace, (1, 2), (1, 3)),
                new_token(TokenKind::RBrace, (1, 4), (1, 5)),
                new_token(TokenKind::LParen, (1, 6), (1, 7)),
                new_token(TokenKind::RParen, (1, 8), (1, 9)),
                new_token(TokenKind::Colon, (1, 10), (1, 11)),
                new_token(TokenKind::Comma, (1, 12), (1, 13)),
                new_token(TokenKind::Assign, (1, 14), (1, 15)),
                new_token(TokenKind::Eof, (1, 15), (1, 15)),
            ]
        )
    }

    #[test]
    fn test_position_tracking_multiline() {
        let input = "var x = 1;\nvar y = 2;";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].loc.start.line, 1);
        assert_eq!(tokens[0].loc.start.column, 0);
        assert_eq!(tokens[5].loc.start.line, 2);
        assert_eq!(tokens[5].loc.start.column, 0);
        // Check that has_line_break is set correctly
        assert!(tokens[4].has_line_break); // Semi has line break after it
        assert!(!tokens[5].has_line_break); // Second var doesn't
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_whitespace_only() {
        let input = "   \t   ";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_has_line_break_single_newline() {
        let input = "a\nb";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident("a".to_string()));
        assert!(tokens[0].has_line_break);
        assert_eq!(tokens[1].kind, TokenKind::Ident("b".to_string()));
        assert!(!tokens[1].has_line_break);
    }

    #[test]
    fn test_has_line_break_carriage_return() {
        let input = "a\rb";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident("a".to_string()));
        assert!(tokens[0].has_line_break);
        assert_eq!(tokens[1].kind, TokenKind::Ident("b".to_string()));
    }

    #[test]
    fn test_has_line_break_crlf() {
        let input = "a\r\nb";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident("a".to_string()));
        assert!(tokens[0].has_line_break);
        assert_eq!(tokens[1].kind, TokenKind::Ident("b".to_string()));
    }

    #[test]
    fn test_has_line_break_multiple_newlines() {
        let input = "a\n\nb";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident("a".to_string()));
        assert!(tokens[0].has_line_break);
        assert_eq!(tokens[1].kind, TokenKind::Ident("b".to_string()));
    }

    #[test]
    fn test_has_line_break_no_line_break() {
        let input = "a b";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident("a".to_string()));
        assert!(!tokens[0].has_line_break);
        assert_eq!(tokens[1].kind, TokenKind::Ident("b".to_string()));
    }

    #[test]
    fn test_has_line_break_with_semicolon() {
        let input = "var x = 1;\nvar y = 2;";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[4].kind, TokenKind::Semi);
        assert!(tokens[4].has_line_break);
    }

    #[test]
    fn test_has_line_break_return_statement() {
        let input = "return\n42";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Return);
        assert!(tokens[0].has_line_break);
        assert_eq!(tokens[1].kind, TokenKind::Number(42.0));
    }

    #[test]
    fn test_line_position_reset() {
        let input = "a\nb";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].loc.start.line, 1);
        assert_eq!(tokens[0].loc.end.line, 1);
        assert_eq!(tokens[1].loc.start.line, 2);
        assert_eq!(tokens[1].loc.end.line, 2);
    }

    #[test]
    fn test_mixed_whitespace_and_newlines() {
        let input = "a  \n  b";
        let tokens = Lexer::new().tokenize(input).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident("a".to_string()));
        assert!(tokens[0].has_line_break);
        assert_eq!(tokens[1].kind, TokenKind::Ident("b".to_string()));
    }

    #[test]
    fn test_error_invalid_character() {
        let input = "@";
        let result = Lexer::new().tokenize(input);
        assert!(result.is_err());
    }
}
