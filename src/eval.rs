use crate::{
    ast::{Expression, Literal, Program, Statement, Value},
    features::FeatureRegistry,
};

#[derive(Debug)]
pub enum EvalError {
    UnsupportedLiteral(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnsupportedLiteral(literal) => {
                write!(f, "Unsupported literal type: {}", literal)
            }
        }
    }
}

impl std::error::Error for EvalError {}

pub struct Context {
    // Future: variable bindings, scope chain, etc.
}

impl Context {
    pub fn new() -> Self {
        Self {}
    }
}

pub fn eval_program(
    program: &Program,
    ctx: &mut Context,
    features: &FeatureRegistry,
) -> Result<Option<Value>, EvalError> {
    let mut last_value = None;

    for stmt in &program.body {
        if let Some(value) = eval_statement(stmt, ctx, features)? {
            last_value = Some(value);
        }
    }

    Ok(last_value)
}

fn eval_statement(
    stmt: &Statement,
    ctx: &mut Context,
    features: &FeatureRegistry,
) -> Result<Option<Value>, EvalError> {
    if let Some(value) = features.eval_statement(stmt, ctx)? {
        return Ok(Some(value));
    }

    match stmt {
        Statement::Expression(expr) => Ok(Some(eval_expression(expr, ctx, features)?)),
    }
}

fn eval_expression(
    expr: &Expression,
    ctx: &mut Context,
    features: &FeatureRegistry,
) -> Result<Value, EvalError> {
    match expr {
        Expression::Literal(literal) => eval_literal(literal, ctx, features),
    }
}

fn eval_literal(
    literal: &Literal,
    ctx: &mut Context,
    features: &FeatureRegistry,
) -> Result<Value, EvalError> {
    if let Some(value) = features.eval_literal(literal, ctx)? {
        return Ok(value);
    }

    // No feature could evaluate this literal
    Err(EvalError::UnsupportedLiteral(format!("{:?}", literal)))
}
