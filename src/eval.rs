use crate::ast::{BinaryOperator, Expression, Literal, Program, Statement};

pub enum Value {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Boolean(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Number(n) => {
                if n.is_infinite() {
                    return if n.is_sign_positive() {
                        write!(f, "Infinity")
                    } else {
                        write!(f, "-Infinity")
                    };
                }
                if n.is_nan() {
                    return write!(f, "NaN");
                }
                if n.fract() == 0.0 {
                    write!(f, "{:.0}", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::String(s) => write!(f, "'{s}'"),
        }
    }
}

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
        Statement::ExpressionStatement { expression } => {
            Ok(Some(eval_expression(&expression, ctx)?))
        }
        Statement::BlockStatement { body } => eval_block(body, ctx),
        Statement::EmptyStatement => Ok(None),
    }
}

fn eval_block(statements: &[Statement], ctx: &mut Context) -> Result<Option<Value>, EvalError> {
    let mut last_value = None;
    for stmt in statements {
        if let Some(value) = eval_statement(stmt, ctx)? {
            last_value = Some(value);
        }
    }
    Ok(last_value)
}

fn eval_expression(expr: &Expression, ctx: &mut Context) -> Result<Value, EvalError> {
    match expr {
        Expression::Literal(literal) => eval_literal(literal, ctx),
        Expression::BinaryExpression {
            operator,
            left,
            right,
        } => eval_binary(left, operator, right, ctx),
    }
}

fn eval_binary(
    left: &Expression,
    op: &BinaryOperator,
    right: &Expression,
    ctx: &mut Context,
) -> Result<Value, EvalError> {
    let left_val = eval_expression(left, ctx)?;
    let right_val = eval_expression(right, ctx)?;

    match (left_val, right_val) {
        (Value::Number(l), Value::Number(r)) => match op {
            BinaryOperator::Add => Ok(Value::Number(l + r)),
            BinaryOperator::Subtract => Ok(Value::Number(l - r)),
            BinaryOperator::Multiply => Ok(Value::Number(l * r)),
            BinaryOperator::Divide => Ok(Value::Number(l / r)),
        },
        (Value::String(l), Value::String(r)) => match op {
            BinaryOperator::Add => Ok(Value::String(l + &r)),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn eval_literal(literal: &Literal, _ctx: &mut Context) -> Result<Value, EvalError> {
    match literal {
        Literal::Null => Ok(Value::Null),
        Literal::Boolean(b) => Ok(Value::Boolean(*b)),
        Literal::Number(n) => Ok(Value::Number(*n)),
        Literal::String(s) => Ok(Value::String(s.clone())),
    }
}
