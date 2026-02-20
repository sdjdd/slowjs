use crate::{
    ast::{Expression, Literal, Value},
    eval::Context,
    features::{EvalResult, Feature},
    lexer::TokenKind,
};

pub struct LiteralFeature;

impl Feature for LiteralFeature {
    fn parse_primary(
        parser: &mut crate::parser::Parser,
    ) -> super::ParseResult<Option<crate::ast::Expression>> {
        match parser.current() {
            Ok(TokenKind::Null) => {
                parser.advance()?;
                Ok(Some(Expression::Literal(Literal::Null)))
            }
            Ok(TokenKind::StringLit(s)) => {
                let s = s.clone();
                parser.advance()?;
                Ok(Some(Expression::Literal(Literal::String(s))))
            }
            _ => Ok(None),
        }
    }

    fn eval_literal(literal: &Literal, _ctx: &mut Context) -> EvalResult<Option<Value>> {
        match literal {
            Literal::Null => Ok(Some(Value::Null)),
            Literal::String(s) => Ok(Some(Value::String(s.clone()))),
        }
    }
}
