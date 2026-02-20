pub mod literal;

use crate::{
    ast::{Expression, Literal, Statement, Value},
    eval::{Context, EvalError},
    parser::{ParseError, Parser},
};

pub type ParseResult<T> = Result<T, ParseError>;

/// Result type for evaluation
pub type EvalResult<T> = Result<T, EvalError>;

pub trait Feature {
    fn parse_statement(_parser: &mut Parser) -> ParseResult<Option<Statement>> {
        Ok(None)
    }

    fn parse_primary(_parser: &mut Parser) -> ParseResult<Option<Expression>> {
        Ok(None)
    }

    fn eval_statement(_stmt: &Statement, _ctx: &mut Context) -> EvalResult<Option<Value>> {
        Ok(None)
    }

    fn eval_literal(_literal: &Literal, _ctx: &mut Context) -> EvalResult<Option<Value>> {
        Ok(None)
    }
}

type StatementParser = fn(&mut Parser) -> ParseResult<Option<Statement>>;

type PrimaryParser = fn(&mut Parser) -> ParseResult<Option<Expression>>;

type StatementEvaluator = fn(&Statement, &mut Context) -> EvalResult<Option<Value>>;

type LiteralEvaluator = fn(&Literal, &mut Context) -> EvalResult<Option<Value>>;

#[derive(Clone)]
struct FeatureRef {
    parse_statement: StatementParser,
    parse_primary: PrimaryParser,
    eval_statement: StatementEvaluator,
    eval_literal: LiteralEvaluator,
}

const ALL_FEATURES: &[FeatureRef] = &[FeatureRef {
    parse_statement: literal::LiteralFeature::parse_statement,
    parse_primary: literal::LiteralFeature::parse_primary,
    eval_statement: literal::LiteralFeature::eval_statement,
    eval_literal: literal::LiteralFeature::eval_literal,
}];

pub struct FeatureRegistry {
    features: Vec<FeatureRef>,
}

impl FeatureRegistry {
    pub fn new() -> Self {
        Self {
            features: ALL_FEATURES.to_vec(),
        }
    }

    pub fn parse_statement(&self, parser: &mut Parser) -> ParseResult<Option<Statement>> {
        for feature in &self.features {
            if let Some(stmt) = (feature.parse_statement)(parser)? {
                return Ok(Some(stmt));
            }
        }
        Ok(None)
    }

    pub fn parse_primary(&self, parser: &mut Parser) -> ParseResult<Option<Expression>> {
        for feature in &self.features {
            if let Some(expr) = (feature.parse_primary)(parser)? {
                return Ok(Some(expr));
            }
        }
        Ok(None)
    }

    pub fn eval_statement(&self, stmt: &Statement, ctx: &mut Context) -> EvalResult<Option<Value>> {
        for feature in &self.features {
            if let Some(value) = (feature.eval_statement)(stmt, ctx)? {
                return Ok(Some(value));
            }
        }
        Ok(None)
    }

    pub fn eval_literal(&self, literal: &Literal, ctx: &mut Context) -> EvalResult<Option<Value>> {
        for feature in &self.features {
            if let Some(value) = (feature.eval_literal)(literal, ctx)? {
                return Ok(Some(value));
            }
        }
        Ok(None)
    }
}
