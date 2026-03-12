use nom::{
    Finish, IResult,
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{digit0, digit1, one_of},
    combinator::{map, recognize, value},
    sequence::{pair, tuple},
};
use thiserror::Error;

use crate::ast::{Position, SourceLocation};

mod parse;
use parse::string_literal;

#[derive(Debug, Error, PartialEq)]
#[error("SyntaxError: {message}")]
pub struct SyntaxError {
    pub message: String,
    /// Source position in bytes
    pub pos: usize,
    /// Invalid source length in bytes
    pub len: usize,

    remains_len: usize,
}

impl SyntaxError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            pos: 0,
            len: 0,
            remains_len: 0,
        }
    }

    pub fn with_remains_data(mut self, remains_len: usize, len: usize) -> Self {
        self.remains_len = remains_len;
        self.len = len;
        self
    }
}

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
    New,
    This,
    Throw,
    Try,
    Catch,
    Finally,

    // Operators
    Plus,  // +
    Minus, // -
    Star,  // *
    Slash, // /

    // Assignment
    PlusAssign,  // +=
    MinusAssign, // -=
    StarAssign,  // *=
    SlashAssign, // /=

    // Comparison
    Eq,        // ==
    NotEq,     // !=
    Less,      // <
    LessEq,    // <=
    Greater,   // >
    GreaterEq, // >=

    // Logical
    Bang,       // !
    LogicalAnd, // &&
    LogicalOr,  // ||

    // Instanceof
    Instanceof, // instanceof

    Semi,     // ;
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
    LParen,   // (
    RParen,   // )
    Colon,    // :
    Comma,    // ,
    Assign,   // =
    Dot,      // .

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
    /// True if there's a LineTerminator after this token
    pub has_line_break: bool,
}

pub struct Lexer {
    pos: Position,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            pos: Position::new(1, 0),
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Result<Vec<Token>, SyntaxError> {
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
            let (rest, kind) = self.parse_token(remaining).map_err(|e| SyntaxError {
                pos: input.len() - e.remains_len,
                ..e
            })?;
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

    fn parse_token<'a>(&mut self, input: &'a str) -> Result<(&'a str, TokenKind), SyntaxError> {
        if input.is_empty() {
            return Ok((input, TokenKind::Eof));
        }

        if let Ok(v) = self.parse_number(input) {
            return Ok(v);
        }

        if input.starts_with('"') || input.starts_with('\'') {
            return self.parse_string(input).finish();
        }

        let ch = input.chars().next().unwrap();

        if ch.is_alphabetic() || input.starts_with('_') || input.starts_with('$') {
            return self.parse_identifier(input).map_err(|_| {
                SyntaxError::new(format!("Invalid token {}", ch))
                    .with_remains_data(input.len(), ch.len_utf8())
            });
        }

        if let Ok((rest, kind)) = self.parse_binary_op(input) {
            return Ok((rest, kind));
        }

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
            '<' => (&input[1..], TokenKind::Less),
            '>' => (&input[1..], TokenKind::Greater),
            '.' => (&input[1..], TokenKind::Dot),
            '[' => (&input[1..], TokenKind::LBracket),
            ']' => (&input[1..], TokenKind::RBracket),
            '!' => (&input[1..], TokenKind::Bang),
            c => {
                return Err(SyntaxError::new(format!("Invalid token {}", c))
                    .with_remains_data(input.len(), c.len_utf8()));
            }
        };
        self.pos.column += 1;

        Ok((input, kind))
    }

    fn parse_binary_op<'a>(&mut self, input: &'a str) -> IResult<&'a str, TokenKind> {
        // Comparison
        let eq = value(TokenKind::Eq, tag("=="));
        let neq = value(TokenKind::NotEq, tag("!="));
        let le = value(TokenKind::LessEq, tag("<="));
        let ge = value(TokenKind::GreaterEq, tag(">="));

        // Assignment
        let add_assign = value(TokenKind::PlusAssign, tag("+="));
        let sub_assign = value(TokenKind::MinusAssign, tag("-="));
        let mul_assign = value(TokenKind::StarAssign, tag("*="));
        let div_assign = value(TokenKind::SlashAssign, tag("/="));

        // Logical
        let and = value(TokenKind::LogicalAnd, tag("&&"));
        let or = value(TokenKind::LogicalOr, tag("||"));

        let parse = alt((
            eq, neq, le, ge, add_assign, sub_assign, mul_assign, div_assign, and, or,
        ));

        map(parse, |kind| {
            self.pos.column += 2;
            kind
        })(input)
    }

    fn parse_line_terminator<'a>(&mut self, input: &'a str) -> IResult<&'a str, &'a str> {
        let crlf = tag("\r\n");
        let lf = tag("\n");
        let cr = tag("\r");
        alt((crlf, lf, cr))(input)
    }

    fn parse_number<'a>(&mut self, input: &'a str) -> IResult<&'a str, TokenKind> {
        let number = alt((
            recognize(tuple((digit1, tag("."), digit0))),
            recognize(tuple((digit0, tag("."), digit1))),
            digit1,
        ));

        map(number, |num_str: &str| {
            self.pos.column += num_str.len();
            TokenKind::Number(num_str.parse().unwrap())
        })(input)
    }

    fn parse_string<'a>(&mut self, input: &'a str) -> IResult<&'a str, TokenKind, SyntaxError> {
        map(string_literal::parse, |s: String| {
            self.pos.column += s.len() + 2; // 2 for quotes
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
                "new" => TokenKind::New,
                "this" => TokenKind::This,
                "throw" => TokenKind::Throw,
                "try" => TokenKind::Try,
                "catch" => TokenKind::Catch,
                "finally" => TokenKind::Finally,
                "instanceof" => TokenKind::Instanceof,
                _ => TokenKind::Ident(ident.to_string()),
            }
        })(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new();
        lexer.tokenize(input).unwrap()
    }

    #[test]
    fn test_null() {
        let token = tokenize("null");
        assert_eq!(token[0].kind, TokenKind::Null);
    }

    #[test]
    fn test_boolean() {
        let token = tokenize("true false");
        assert_eq!(token[0].kind, TokenKind::Boolean(true));
        assert_eq!(token[1].kind, TokenKind::Boolean(false));
    }

    #[test]
    fn test_number() {
        let token = tokenize("123 3.14 100. .100");
        assert_eq!(token[0].kind, TokenKind::Number(123.0));
        assert_eq!(token[1].kind, TokenKind::Number(3.14));
        assert_eq!(token[2].kind, TokenKind::Number(100.0));
        assert_eq!(token[3].kind, TokenKind::Number(0.100));
    }

    #[test]
    fn test_string() {
        let token = tokenize(r#" "hello" "\"" "" "#);
        assert_eq!(token[0].kind, TokenKind::StringLit("hello".to_string()));
        assert_eq!(token[1].kind, TokenKind::StringLit("\"".to_string()));
        assert_eq!(token[2].kind, TokenKind::StringLit("".to_string()));

        let token = tokenize(r#" 'hello' '\'' '' "#);
        assert_eq!(token[0].kind, TokenKind::StringLit("hello".to_string()));
        assert_eq!(token[1].kind, TokenKind::StringLit("'".to_string()));
        assert_eq!(token[2].kind, TokenKind::StringLit("".to_string()));
    }
}
