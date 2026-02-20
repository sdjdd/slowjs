use crate::ast::{Expression, Literal, Program, Statement, Value};

#[derive(Debug)]
pub enum EvalError {}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EvalError")
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

pub fn eval_program(program: &Program, ctx: &mut Context) -> Result<Option<Value>, EvalError> {
    let mut last_value = None;

    for stmt in &program.body {
        if let Some(value) = eval_statement(stmt, ctx)? {
            last_value = Some(value);
        }
    }

    Ok(last_value)
}

fn eval_statement(stmt: &Statement, ctx: &mut Context) -> Result<Option<Value>, EvalError> {
    match stmt {
        Statement::Expression(expr) => Ok(Some(eval_expression(expr, ctx)?)),
    }
}

fn eval_expression(expr: &Expression, ctx: &mut Context) -> Result<Value, EvalError> {
    match expr {
        Expression::Literal(literal) => eval_literal(literal, ctx),
    }
}

fn eval_literal(literal: &Literal, _ctx: &mut Context) -> Result<Value, EvalError> {
    match literal {
        Literal::Null => Ok(Value::Null),
        Literal::Boolean(b) => Ok(Value::Boolean(*b)),
        Literal::String(s) => Ok(Value::String(s.clone())),
    }
}
