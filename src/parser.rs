use crate::ast::*;
use crate::lexer::{LexerError, Token, TokenKind};
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
        &self.tokens[self.pos].kind
    }

    fn current_loc(&self) -> SourceLocation {
        self.tokens[self.pos].loc
    }

    /// Check if previous token had a line break after it (for ASI)
    fn prev_had_line_break(&self) -> bool {
        if self.pos == 0 {
            return false;
        }
        self.tokens[self.pos - 1].has_line_break
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn can_insert_semicolon(&self) -> bool {
        matches!(
            self.current(),
            TokenKind::RBrace | TokenKind::Eof | TokenKind::Semi
        ) || self.prev_had_line_break()
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        let current = self.current();
        let matches = std::mem::discriminant(current) == std::mem::discriminant(&expected);

        if matches {
            self.advance();
            return Ok(());
        }
        if expected == TokenKind::Semi && self.can_insert_semicolon() {
            return Ok(());
        }
        Err(self.unexpected())
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
            let item = self.parse_program_body_item()?;
            body.push(item);
        }

        Ok(Program { body })
    }

    fn parse_program_body_item(&mut self) -> Result<StatementOrDirective, ParseError> {
        // Check directive
        if let TokenKind::StringLit(s) = self.current() {
            let s = s.clone();
            let pos_before = self.pos;
            self.advance();
            if let TokenKind::Semi = self.current() {
                self.advance();
                return Ok(StatementOrDirective::Directive(Directive {
                    expression: Literal::String(s.clone()),
                    directive: s,
                }));
            }
            self.pos = pos_before;
        }

        let stmt = self.parse_statement()?;
        Ok(StatementOrDirective::Statement(stmt))
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
        let start_pos = self.current_loc().start;
        self.expect(TokenKind::LBrace)?;

        let mut body = Vec::new();
        while !matches!(self.current(), TokenKind::RBrace) {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        let end_pos = self.current_loc().end;
        self.expect(TokenKind::RBrace)?;

        Ok(Statement::BlockStatement(BlockStatement {
            body,
            loc: Some(SourceLocation::new(start_pos, end_pos)),
        }))
    }

    fn parse_variable_statement(&mut self) -> Result<Statement, ParseError> {
        let start_pos = self.current_loc().start;
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

        let end_pos = self.current_loc().end;
        self.expect(TokenKind::Semi)?;

        Ok(Statement::Declaration(Declaration::VariableDeclaration(
            VariableDeclaration {
                declarations,
                kind: VariableDeclarationKind::Var,
                loc: Some(SourceLocation::new(start_pos, end_pos)),
            },
        )))
    }

    fn parse_variable_declration(&mut self) -> Result<VariableDeclarator, ParseError> {
        let mut loc = self.current_loc();
        match self.current() {
            TokenKind::Ident(name) => {
                let name = name.clone();
                let name_loc = loc;
                self.advance();

                let init = if self.current() == &TokenKind::Assign {
                    let init = self.parse_initializer()?;
                    loc.end = self.current_loc().start;
                    Some(init)
                } else {
                    None
                };

                Ok(VariableDeclarator {
                    id: Pattern::Identifier(Identifier {
                        name,
                        loc: Some(name_loc),
                    }),
                    init,
                    loc: Some(loc),
                })
            }
            _ => Err(self.unexpected()),
        }
    }

    fn parse_initializer(&mut self) -> Result<Expression, ParseError> {
        self.expect(TokenKind::Assign)?;
        self.parse_expression()
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        if matches!(self.current(), TokenKind::LBrace) {
            return Err(self.unexpected());
        }

        let start_pos = self.current_loc().start;
        let expression = self.parse_expression()?;
        let end_pos = self.current_loc().end;
        self.expect(TokenKind::Semi)?;

        Ok(Statement::ExpressionStatement(ExpressionStatement {
            expression,
            loc: Some(SourceLocation::new(start_pos, end_pos)),
        }))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_call_expression()
    }

    fn parse_call_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_equality_expression()?;

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
        let start_pos = self.current_loc().start;
        self.expect(TokenKind::LParen)?;

        let args = self.parse_arguments()?;

        let end_pos = self.current_loc().end;
        self.expect(TokenKind::RParen)?;

        Ok(Expression::CallExpression(CallExpression {
            callee: Box::new(callee),
            arguments: args,
            loc: Some(SourceLocation::new(start_pos, end_pos)),
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

    fn parse_relational_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_additive_expression()?;

        loop {
            let op = match self.current() {
                TokenKind::Less => BinaryOperator::LessThan,
                TokenKind::LessEq => BinaryOperator::LessThanEq,
                TokenKind::Greater => BinaryOperator::GreaterThan,
                TokenKind::GreaterEq => BinaryOperator::GreaterThanEq,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive_expression()?;
            left = Expression::new_binary(op, left, right);
        }

        Ok(left)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_relational_expression()?;

        loop {
            let op = match self.current() {
                TokenKind::Eq => BinaryOperator::Equal,
                TokenKind::NotEq => BinaryOperator::NotEqual,
                _ => break,
            };
            self.advance();
            let right = self.parse_relational_expression()?;
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
                let loc = Some(self.current_loc());
                self.advance();
                Ok(Expression::Identifier(Identifier { name, loc }))
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
        let mut loc = self.current_loc();
        self.expect(TokenKind::LBrace)?;

        let mut properties = Vec::new();

        // Handle empty object {}
        if matches!(self.current(), TokenKind::RBrace) {
            loc.end = self.current_loc().end;
            self.advance();
            return Ok(Expression::ObjectExpression(ObjectExpression {
                properties,
                loc: Some(loc),
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

        loc.end = self.current_loc().end;
        self.expect(TokenKind::RBrace)?;

        Ok(Expression::ObjectExpression(ObjectExpression {
            properties,
            loc: Some(loc),
        }))
    }

    fn parse_property(&mut self) -> Result<Property, ParseError> {
        let mut loc = self.current_loc();
        // Parse key (identifier or literal)
        let key = match self.current() {
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                PropertyKey::Identifier(Identifier {
                    name,
                    loc: Some(loc),
                })
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

        loc.end = self.current_loc().end;
        self.expect(TokenKind::Colon)?;

        let value = self.parse_expression()?;

        Ok(Property {
            key,
            value,
            kind: PropertyKind::Init,
            loc: Some(loc),
        })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement, ParseError> {
        let mut loc = self.current_loc();
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
        loc.end = self.current_loc().start;
        Ok(IfStatement {
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternate: alternate.map(Box::new),
            loc: Some(loc),
        })
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
        let mut loc = self.current_loc();
        self.expect(TokenKind::Function)?;

        let (name, name_loc) = match self.current() {
            TokenKind::Ident(name) => {
                let name = name.clone();
                let loc = self.current_loc();
                self.advance();
                (name, loc)
            }
            _ => return Err(self.unexpected()),
        };

        self.expect(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let body = self.parse_function_body()?;
        loc.end = self.current_loc().end;
        self.expect(TokenKind::RBrace)?;

        Ok(FunctionDeclaration {
            id: Identifier {
                name,
                loc: Some(name_loc),
            },
            params,
            body,
            loc: Some(loc),
        })
    }

    fn parse_function_expression(&mut self) -> Result<Expression, ParseError> {
        let mut loc = self.current_loc();
        self.expect(TokenKind::Function)?;

        let name = if matches!(self.current(), TokenKind::Ident(_)) {
            let name = match self.current() {
                TokenKind::Ident(n) => n.clone(),
                _ => unreachable!(),
            };
            let loc = Some(self.current_loc());
            self.advance();
            Some(Identifier { name, loc })
        } else {
            None
        };

        self.expect(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::LBrace)?;
        let body = self.parse_function_body()?;
        loc.end = self.current_loc().end;
        self.expect(TokenKind::RBrace)?;

        Ok(Expression::FunctionExpression(FunctionExpression {
            id: name,
            params,
            body,
            loc: Some(loc),
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
                    let loc = Some(self.current_loc());
                    self.advance();
                    params.push(Pattern::Identifier(Identifier { name, loc }));
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

    fn parse_function_body(&mut self) -> Result<FunctionBody, ParseError> {
        let mut body = Vec::new();

        while !matches!(self.current(), TokenKind::RBrace) {
            let stmt = self.parse_program_body_item()?;
            body.push(stmt);
        }

        Ok(body)
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        let mut loc = self.current_loc();
        self.expect(TokenKind::Return)?;

        if self.can_insert_semicolon() {
            self.expect(TokenKind::Semi)?;
            return Ok(ReturnStatement {
                argument: None,
                loc: Some(loc),
            });
        }

        let argument = self.parse_expression()?;
        loc.end = self.current_loc().end;
        self.expect(TokenKind::Semi)?;

        Ok(ReturnStatement {
            argument: Some(argument),
            loc: Some(loc),
        })
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse_tokens(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new();
        lexer.tokenize(input).unwrap()
    }

    #[test]
    fn test_parse_variable_statement() {
        let mut parser = Parser::new(parse_tokens("var num = 100;"));
        let stmt = parser.parse_variable_statement().unwrap();
        assert_eq!(
            stmt,
            Statement::Declaration(Declaration::VariableDeclaration(VariableDeclaration {
                declarations: vec![VariableDeclarator {
                    id: Pattern::Identifier(Identifier {
                        name: "num".to_string(),
                        loc: Some(SourceLocation {
                            start: Position { line: 1, column: 4 },
                            end: Position { line: 1, column: 7 },
                        }),
                    }),
                    init: Some(Expression::Literal(Literal::Number(100.0))),
                    loc: Some(SourceLocation {
                        start: Position { line: 1, column: 4 },
                        end: Position {
                            line: 1,
                            column: 13
                        },
                    })
                }],
                kind: VariableDeclarationKind::Var,
                loc: Some(SourceLocation {
                    start: Position { line: 1, column: 0 },
                    end: Position {
                        line: 1,
                        column: 14
                    },
                })
            }))
        )
    }
}
