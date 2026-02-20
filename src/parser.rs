use crate::{
    ast::{Expression, Program, Statement},
    features::FeatureRegistry,
    lexer::{Token, TokenKind},
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken { expected: String, found: String },
    UnexpectedEof,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected token: expected {}, found {}",
                    expected, found
                )
            }
            ParseError::UnexpectedEof => write!(f, "Unexpected end of input"),
        }
    }
}

impl std::error::Error for ParseError {}

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

    pub fn parse_program(&mut self, features: &FeatureRegistry) -> Result<Program, ParseError> {
        let mut body = Vec::new();

        while !matches!(self.current()?, TokenKind::Eof) {
            // Skip semicolons between statements
            if matches!(self.current()?, TokenKind::Semi) {
                self.advance()?;
                continue;
            }
            let stmt = self.parse_statement(features)?;
            body.push(stmt);
        }

        Ok(Program { body })
    }

    pub fn parse_statement(&mut self, features: &FeatureRegistry) -> Result<Statement, ParseError> {
        if let Some(stmt) = features.parse_statement(self)? {
            return Ok(stmt);
        }

        let expr = self.parse_expression(features)?;

        // Consume optional semicolon
        if matches!(self.current()?, TokenKind::Semi) {
            self.advance()?;
        }

        Ok(Statement::Expression(expr))
    }

    pub fn parse_expression(
        &mut self,
        features: &FeatureRegistry,
    ) -> Result<Expression, ParseError> {
        self.parse_primary(features)
    }

    pub fn parse_primary(&mut self, features: &FeatureRegistry) -> Result<Expression, ParseError> {
        if let Some(expr) = features.parse_primary(self)? {
            return Ok(expr);
        }

        Err(ParseError::UnexpectedToken {
            expected: "expression".to_string(),
            found: format!("{:?}", self.current()?),
        })
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let features = FeatureRegistry::new();
    let mut parser = Parser::new(tokens);
    parser.parse_program(&features)
}
