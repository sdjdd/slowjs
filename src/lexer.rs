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

    Semi,           // ;
    LBrace,         // {
    RBrace,         // }
    LParen,         // (
    RParen,         // )
    Colon,          // :
    Comma,          // ,
    Assign,         // =
    LineTerminator, // \r | \n | \r\n

    Eof,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
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
        let mut remaining = self.skip_whitespace(input);

        while !remaining.is_empty() {
            let start = self.pos;
            let (rest, kind) = self.parse_token(remaining)?;
            let end = self.pos;
            remaining = self.skip_whitespace(rest);

            if kind == TokenKind::LineTerminator {
                match tokens.last() {
                    Some(last) => match last.kind {
                        TokenKind::Return => {
                            // Keep LineTerminator
                        }
                        _ => continue,
                    },
                    None => continue,
                }
            }

            tokens.push(Token {
                kind,
                loc: SourceLocation { start, end },
            });
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            loc: SourceLocation {
                start: self.pos,
                end: self.pos,
            },
        });

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

        if let Ok((rest, tk)) = self.parse_line_terminator(input) {
            return Ok((rest, tk));
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

    fn parse_line_terminator<'a>(&mut self, input: &'a str) -> IResult<&'a str, TokenKind> {
        let crlf = tag("\r\n");
        let lf = tag("\n");
        let cr = tag("\r");
        map(alt((crlf, lf, cr)), |_| {
            self.pos.column = 0;
            self.pos.line += 1;
            TokenKind::LineTerminator
        })(input)
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
}
