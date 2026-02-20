use crate::{
    ast::{BinaryOperator, Expression, Literal, Program, Statement},
    lexer::{LexerError, Token, TokenKind},
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: Option<String>,
        found: String,
    },
    UnexpectedEof,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                if let Some(expected) = expected {
                    write!(
                        f,
                        "Unexpected token: expected {}, found {}",
                        expected, found
                    )
                } else {
                    write!(f, "Unexpected token: {}", found)
                }
            }
            ParseError::UnexpectedEof => write!(f, "Unexpected EOF"),
        }
    }
}

impl std::error::Error for ParseError {}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        match err {
            LexerError::UnexpectedToken(t) => ParseError::UnexpectedToken {
                expected: None,
                found: t.to_string(),
            },
            LexerError::Eof => ParseError::UnexpectedEof,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn current(&self) -> Result<&TokenKind, ParseError> {
        self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .ok_or(ParseError::UnexpectedEof)
    }

    pub fn advance(&mut self) -> Result<(), ParseError> {
        if self.pos >= self.tokens.len() {
            return Err(ParseError::UnexpectedEof);
        }
        self.pos += 1;
        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut body = Vec::new();

        while !matches!(self.current()?, TokenKind::Eof) {
            // Skip semicolons between statements
            if matches!(self.current()?, TokenKind::Semi) {
                self.advance()?;
                continue;
            }
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        Ok(Program { body })
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression()?;

        // Consume optional semicolon
        if matches!(self.current()?, TokenKind::Semi) {
            self.advance()?;
        }

        Ok(Statement::Expression(expr))
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_infix_expr(0)
    }

    fn parse_infix_expr(&mut self, precedence: usize) -> Result<Expression, ParseError> {
        let mut left = self.parse_primary()?;

        loop {
            let current_kind = match self.current() {
                Ok(kind) => kind,
                Err(ParseError::UnexpectedEof) => break,
                Err(e) => return Err(e),
            };

            let (op, prec) = match get_operator_precedence(current_kind) {
                Some(result) => result,
                None => break,
            };

            if prec < precedence {
                break;
            }

            self.advance()?;

            if matches!(self.current(), Ok(TokenKind::Eof)) {
                // No right-hand
                return Err(ParseError::UnexpectedEof);
            }

            let right = self.parse_infix_expr(prec + 1)?;

            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    pub fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.current()? {
            TokenKind::Null => {
                self.advance()?;
                Ok(Expression::Literal(Literal::Null))
            }
            TokenKind::Boolean(b) => {
                let b = *b;
                self.advance()?;
                Ok(Expression::Literal(Literal::Boolean(b)))
            }
            TokenKind::Number(n) => {
                let n = *n;
                self.advance()?;
                Ok(Expression::Literal(Literal::Number(n)))
            }
            TokenKind::StringLit(s) => {
                let s = s.clone();
                self.advance()?;
                Ok(Expression::Literal(Literal::String(s)))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: None,
                found: format!("{:?}", self.current()?),
            }),
        }
    }
}

fn get_operator_precedence(kind: &TokenKind) -> Option<(BinaryOperator, usize)> {
    match kind {
        TokenKind::Plus => Some((BinaryOperator::Add, 5)),
        TokenKind::Minus => Some((BinaryOperator::Subtract, 5)),
        TokenKind::Star => Some((BinaryOperator::Multiply, 6)),
        TokenKind::Slash => Some((BinaryOperator::Divide, 6)),
        _ => None,
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}
