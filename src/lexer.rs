use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{escaped, is_not, take_while, take_while1},
    character::complete::{anychar, char, one_of},
    combinator::{map, opt, recognize},
    sequence::{delimited, pair},
};

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Null,
    Boolean(bool),
    Number(f64),
    StringLit(String),

    Semi, // ;

    Eof,
    Unknown,
}

pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Token {
        Self { kind }
    }
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedToken(String),
    Eof,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedToken(t) => write!(f, "Unexpected token: {t}"),
            LexerError::Eof => write!(f, "Unexpected end of input"),
        }
    }
}

impl std::error::Error for LexerError {}

pub fn tokenize(input: &str) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut remaining = input.trim_start();

    while !remaining.is_empty() {
        let (rest, kind) = parse_token(remaining)?;
        tokens.push(Token::new(kind));
        remaining = rest.trim_start();
    }

    tokens.push(Token::new(TokenKind::Eof));
    Ok(tokens)
}

fn parse_token(input: &str) -> Result<(&str, TokenKind), LexerError> {
    let input = input.trim_start();

    if input.is_empty() {
        return Err(LexerError::Eof);
    }

    if input.chars().next().unwrap().is_ascii_digit() {
        return parse_number(input)
            .map_err(|_| LexerError::UnexpectedToken(input.chars().next().unwrap().to_string()));
    }

    if input.starts_with('"') || input.starts_with('\'') {
        return parse_string(input).map_err(|_| LexerError::UnexpectedToken(input.to_string()));
    }

    if input.chars().next().unwrap().is_alphabetic()
        || input.starts_with('_')
        || input.starts_with('$')
    {
        return parse_identifier(input).map_err(|_| LexerError::Eof);
    }

    match input.chars().next().unwrap() {
        ';' => Ok((input[1..].trim_start(), TokenKind::Semi)),
        c => Err(LexerError::UnexpectedToken(c.to_string())),
    }
}

fn parse_number(input: &str) -> IResult<&str, TokenKind> {
    let integer_part = take_while1(|c: char| c.is_ascii_digit());
    let fractional_part = opt(pair(char('.'), take_while(|c: char| c.is_ascii_digit())));
    let number = recognize(pair(integer_part, fractional_part));

    map(number, |num_str: &str| {
        TokenKind::Number(num_str.parse().unwrap())
    })(input)
}

fn parse_string(input: &str) -> IResult<&str, TokenKind> {
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
        TokenKind::StringLit(s)
    })(input)
}

fn parse_identifier(input: &str) -> IResult<&str, TokenKind> {
    let first = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$");
    let rest = take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '$');

    map(recognize(pair(first, rest)), |ident: &str| match ident {
        "null" => TokenKind::Null,
        "true" => TokenKind::Boolean(true),
        "false" => TokenKind::Boolean(false),
        _ => TokenKind::Unknown,
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null_literal() {
        let tokens = tokenize("null").unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Null);
    }

    #[test]
    fn test_string_literal() {
        let tokens = tokenize(r#""hello""#).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::StringLit("hello".to_string()));
    }
}
