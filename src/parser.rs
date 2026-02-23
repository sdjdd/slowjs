use crate::{
    ast::{
        self, BinaryOperator, BlockStatement, CallExpression, Declaration, Expression,
        ExpressionStatement, FunctionDeclaration, Identifier, IfStatement, Literal,
        ObjectExpression, Pattern, Program, Property, PropertyKey, PropertyKind, ReturnStatement,
        Statement, VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
    },
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
            Err(self.unexpected())
        }
    }

    fn unexpected(&self) -> ParseError {
        ParseError::UnexpectedToken {
            expected: None,
            found: self.current().clone(),
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
            TokenKind::LBrace => self.parse_block_statement(),
            TokenKind::Var => self.parse_variable_statement(),
            TokenKind::Semi => {
                self.advance();
                Ok(Statement::EmptyStatement)
            }
            TokenKind::If => Ok(Statement::IfStatement(self.parse_if_statement()?)),
            TokenKind::Function => Ok(Statement::Declaration(Declaration::FunctionDeclaration(
                self.parse_function_declaration()?,
            ))),
            TokenKind::Return => Ok(Statement::ReturnStatement(self.parse_return_statement()?)),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenKind::LBrace)?;

        let mut body = Vec::new();
        while !matches!(self.current(), TokenKind::RBrace) {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Statement::BlockStatement(BlockStatement { body }))
    }

    fn parse_variable_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(TokenKind::Var)?;

        let mut declarations = Vec::new();

        loop {
            declarations.push(self.parse_variable_declration()?);

            match self.current() {
                TokenKind::Comma => {
                    self.advance();
                    continue;
                }
                _ => break,
            }
        }

        self.expect(TokenKind::Semi)?;

        Ok(Statement::Declaration(Declaration::VariableDeclaration(
            VariableDeclaration {
                declarations,
                kind: VariableDeclarationKind::Var,
            },
        )))
    }

    fn parse_variable_declration(&mut self) -> Result<VariableDeclarator, ParseError> {
        match self.current() {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                Ok(VariableDeclarator {
                    id: Identifier { name },
                    init: if self.current() == &TokenKind::Assign {
                        Some(self.parse_initializer()?)
                    } else {
                        None
                    },
                })
            }
            _ => Err(self.unexpected()),
        }
    }

    fn parse_initializer(&mut self) -> Result<Expression, ParseError> {
        self.expect(TokenKind::Assign)?;
        self.parse_primary()
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        if matches!(self.current(), TokenKind::LBrace) {
            return Err(self.unexpected());
        }

        let expression = self.parse_expression()?;
        self.expect(TokenKind::Semi)?;

        Ok(Statement::ExpressionStatement(ExpressionStatement {
            expression,
        }))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_call_expression()
    }

    fn parse_call_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_additive_expression()?;

        // handle chained call
        loop {
            if matches!(self.current(), TokenKind::LParen) {
                expr = self.parse_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        self.expect(TokenKind::LParen)?;

        let args = self.parse_arguments()?;

        self.expect(TokenKind::RParen)?;

        Ok(Expression::CallExpression(CallExpression {
            callee: Box::new(callee),
            arguments: args,
        }))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args = Vec::new();

        // No arguments
        if matches!(self.current(), TokenKind::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);

            if matches!(self.current(), TokenKind::RParen) {
                break;
            }

            self.expect(TokenKind::Comma)?;

            // Trailing comma
            if matches!(self.current(), TokenKind::RParen) {
                break;
            }
        }

        Ok(args)
    }

    fn parse_additive_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_multiplicative_expression()?;

        loop {
            let op = match self.current() {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Subtract,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative_expression()?;
            left = Expression::new_binary(op, left, right);
        }

        Ok(left)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_primary()?;

        loop {
            let op = match self.current() {
                TokenKind::Star => BinaryOperator::Multiply,
                TokenKind::Slash => BinaryOperator::Divide,
                _ => break,
            };
            self.advance();
            let right = self.parse_primary()?;
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
            TokenKind::Function => self.parse_function_expression(),
            TokenKind::LBrace => self.parse_object_expression(),
            TokenKind::LParen => self.parse_paren_expression(),
            _ => Err(self.unexpected()),
        }
    }

    fn parse_paren_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect(TokenKind::LParen)?;
        let expr = self.parse_expression()?;
        self.expect(TokenKind::RParen)?;
        Ok(expr)
    }

    fn parse_object_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect(TokenKind::LBrace)?;

        let mut properties = Vec::new();

        // Handle empty object {}
        if matches!(self.current(), TokenKind::RBrace) {
            self.advance();
            return Ok(Expression::ObjectExpression(ObjectExpression {
                properties,
            }));
        }

        loop {
            let property = self.parse_property()?;
            properties.push(property);

            if matches!(self.current(), TokenKind::RBrace) {
                break;
            }

            self.expect(TokenKind::Comma)?;

            // Handle trailing comma
            if matches!(self.current(), TokenKind::RBrace) {
                break;
            }
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Expression::ObjectExpression(ObjectExpression {
            properties,
        }))
    }

    fn parse_property(&mut self) -> Result<Property, ParseError> {
        // Parse key (identifier or literal)
        let key = match self.current() {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                PropertyKey::Identifier(Identifier { name })
            }
            TokenKind::StringLit(s) => {
                let s = s.clone();
                self.advance();
                PropertyKey::Literal(Literal::String(s))
            }
            TokenKind::Number(n) => {
                let n = *n;
                self.advance();
                PropertyKey::Literal(Literal::Number(n))
            }
            _ => {
                return Err(self.unexpected());
            }
        };

        self.expect(TokenKind::Colon)?;

        let value = self.parse_expression()?;

        Ok(Property {
            key,
            value,
            kind: PropertyKind::Init,
        })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement, ParseError> {
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LParen)?;
        let test = self.parse_expression()?;
        self.expect(TokenKind::RParen)?;
        let consequent = self.parse_statement()?;
        let alternate = if matches!(self.current(), TokenKind::Else) {
            self.advance();
            Some(self.parse_statement()?)
        } else {
            None
        };
        Ok(IfStatement {
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternate: alternate.map(Box::new),
        })
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
        self.expect(TokenKind::Function)?;

        let name = match self.current() {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => return Err(self.unexpected()),
        };

        self.expect(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let body = self.parse_function_body()?;
        self.expect(TokenKind::RBrace)?;

        Ok(FunctionDeclaration {
            id: Identifier { name },
            params,
            body,
        })
    }

    fn parse_function_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect(TokenKind::Function)?;

        let name = if matches!(self.current(), TokenKind::Ident(_)) {
            let name = match self.current() {
                TokenKind::Ident(n) => n.clone(),
                _ => unreachable!(),
            };
            self.advance();
            Some(Identifier { name })
        } else {
            None
        };

        self.expect(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let body = self.parse_function_body()?;
        self.expect(TokenKind::RBrace)?;

        Ok(Expression::FunctionExpression(ast::FunctionExpression {
            id: name,
            params,
            body,
        }))
    }

    fn parse_params(&mut self) -> Result<Vec<Pattern>, ParseError> {
        let mut params = Vec::new();

        // Handle empty params
        if matches!(self.current(), TokenKind::RParen) {
            return Ok(params);
        }

        loop {
            match self.current() {
                TokenKind::Ident(name) => {
                    let name = name.clone();
                    self.advance();
                    params.push(Pattern::Identifier(Identifier { name }));
                }
                _ => return Err(self.unexpected()),
            }

            if matches!(self.current(), TokenKind::RParen) {
                break;
            }

            self.expect(TokenKind::Comma)?;

            // Handle trailing comma
            if matches!(self.current(), TokenKind::RParen) {
                break;
            }
        }

        Ok(params)
    }

    fn parse_function_body(&mut self) -> Result<BlockStatement, ParseError> {
        let mut body = Vec::new();

        while !matches!(self.current(), TokenKind::RBrace) {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        Ok(BlockStatement { body })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        self.expect(TokenKind::Return)?;

        if matches!(
            self.current(),
            TokenKind::Semi | TokenKind::RBrace | TokenKind::LineTerminator
        ) {
            return Ok(ReturnStatement { argument: None });
        }

        let argument = self.parse_expression()?;
        self.expect(TokenKind::Semi)?;

        Ok(ReturnStatement {
            argument: Some(argument),
        })
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}
