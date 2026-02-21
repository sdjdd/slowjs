use crate::{
    ast::{BinaryOperator, Expression, Identifier, Literal, Program, Statement},
    lexer::{LexerError, Token, TokenKind},
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token: {found:?}")]
    UnexpectedToken {
        #[allow(unused)]
        expected: Option<TokenKind>,
        found: TokenKind,
    },
    #[error(transparent)]
    Lexer(#[from] LexerError),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> &TokenKind {
        self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::Eof)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        let current = self.current();
        let matches = std::mem::discriminant(current) == std::mem::discriminant(&expected);

        if matches {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: Some(expected),
                found: current.clone(),
            })
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut body = Vec::new();

        while !matches!(self.current(), TokenKind::Eof) {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        Ok(Program { body })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current() {
            TokenKind::Semi => {
                self.advance();
                Ok(Statement::EmptyStatement)
            }
            TokenKind::LBrace => self.parse_block_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenKind::LBrace)?;

        let mut body = Vec::new();
        while !matches!(self.current(), TokenKind::RBrace | TokenKind::Eof) {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Statement::new_block(body))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression()?;
        Ok(Statement::new_expression(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_infix_expr(0)
    }

    fn parse_infix_expr(&mut self, precedence: usize) -> Result<Expression, ParseError> {
        let mut left = self.parse_primary()?;

        loop {
            let current_kind = match self.current() {
                TokenKind::Eof => break,
                kind => kind,
            };

            let (op, prec) = match get_operator_precedence(current_kind) {
                Some(result) => result,
                None => break,
            };

            if prec < precedence {
                break;
            }

            self.advance();

            let right = self.parse_infix_expr(prec + 1)?;

            left = Expression::new_binary(op, left, right);
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.current() {
            TokenKind::Null => {
                self.advance();
                Ok(Expression::Literal(Literal::Null))
            }
            TokenKind::Boolean(b) => {
                let b = *b;
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(b)))
            }
            TokenKind::Number(n) => {
                let n = *n;
                self.advance();
                Ok(Expression::Literal(Literal::Number(n)))
            }
            TokenKind::StringLit(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expression::Literal(Literal::String(s)))
            }
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                Ok(Expression::Identifier(Identifier { name }))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: None,
                found: self.current().clone(),
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
