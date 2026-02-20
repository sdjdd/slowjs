use nom::{
    IResult,
    bytes::complete::take_while,
    character::complete::one_of,
    combinator::{map, recognize},
    sequence::pair,
};

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Null,

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
    UnexpectedCharacter(char),
    Eof,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedCharacter(c) => write!(f, "Unexpected character: '{}'", c),
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

    if input.chars().next().unwrap().is_alphabetic()
        || input.starts_with('_')
        || input.starts_with('$')
    {
        return parse_identifier(input).map_err(|_| LexerError::Eof);
    }

    match input.chars().next().unwrap() {
        ';' => Ok((input[1..].trim_start(), TokenKind::Semi)),
        c => Err(LexerError::UnexpectedCharacter(c)),
    }
}

fn parse_identifier(input: &str) -> IResult<&str, TokenKind> {
    let first = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$");
    let rest = take_while(|c: char| c.is_alphanumeric() || c == '_' || c == '$');

    map(recognize(pair(first, rest)), |ident: &str| match ident {
        "null" => TokenKind::Null,
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
}
