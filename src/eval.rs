use crate::ast::{
    BinaryExpression, BinaryOperator, BlockStatement, Expression, ExpressionStatement, Identifier,
    Literal, Program, Statement,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Null,
    Undefined,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Undefined => write!(f, "undefined"),
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
pub enum EvalError {
    ReferenceError(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::ReferenceError(name) => write!(f, "ReferenceError: {name} is not defined"),
        }
    }
}

impl std::error::Error for EvalError {}

pub struct Environment {
    bindings: HashMap<String, Value>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            outer: None,
        }
    }

    pub fn extend(&mut self, outer: Rc<RefCell<Environment>>) {
        self.outer = Some(outer);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.bindings
            .get(name)
            .cloned()
            .or_else(|| self.outer.as_ref()?.borrow().get(name))
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }
}

pub struct Context {
    global_env: Rc<RefCell<Environment>>,
}

impl Context {
    pub fn new() -> Self {
        let global_env = Rc::new(RefCell::new(Environment::new()));
        // Register global `undefined`
        global_env
            .borrow_mut()
            .set("undefined".to_string(), Value::Undefined);
        Self { global_env }
    }

    pub fn global_env(&self) -> Rc<RefCell<Environment>> {
        Rc::clone(&self.global_env)
    }
}

pub fn eval_program(program: &Program, ctx: &mut Context) -> Result<Value, EvalError> {
    let env = ctx.global_env();
    let mut last_value = Value::Undefined;

    for stmt in &program.body {
        if let Some(value) = eval_statement(stmt, &env)? {
            last_value = value;
        }
    }

    Ok(last_value)
}

fn eval_statement(
    stmt: &Statement,
    env: &Rc<RefCell<Environment>>,
) -> Result<Option<Value>, EvalError> {
    match stmt {
        Statement::ExpressionStatement(ExpressionStatement { expression }) => {
            Ok(Some(eval_expression(&expression, env)?))
        }
        Statement::BlockStatement(BlockStatement { body }) => eval_block(body, env),
        Statement::EmptyStatement => Ok(None),
    }
}

fn eval_block(
    statements: &[Statement],
    env: &Rc<RefCell<Environment>>,
) -> Result<Option<Value>, EvalError> {
    let mut last_value = None;
    for stmt in statements {
        if let Some(value) = eval_statement(stmt, env)? {
            last_value = Some(value);
        }
    }
    Ok(last_value)
}

fn eval_expression(expr: &Expression, env: &Rc<RefCell<Environment>>) -> Result<Value, EvalError> {
    match expr {
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Identifier(Identifier { name }) => eval_identifier(name, env),
        Expression::BinaryExpression(BinaryExpression {
            operator,
            left,
            right,
        }) => eval_binary(left, operator, right, env),
    }
}

fn eval_identifier(name: &str, env: &Rc<RefCell<Environment>>) -> Result<Value, EvalError> {
    env.borrow()
        .get(name)
        .ok_or_else(|| EvalError::ReferenceError(name.to_string()))
}

fn eval_binary(
    left: &Expression,
    op: &BinaryOperator,
    right: &Expression,
    env: &Rc<RefCell<Environment>>,
) -> Result<Value, EvalError> {
    let left_val = eval_expression(left, env)?;
    let right_val = eval_expression(right, env)?;

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

fn eval_literal(literal: &Literal) -> Result<Value, EvalError> {
    match literal {
        Literal::Null => Ok(Value::Null),
        Literal::Boolean(b) => Ok(Value::Boolean(*b)),
        Literal::Number(n) => Ok(Value::Number(*n)),
        Literal::String(s) => Ok(Value::String(s.clone())),
    }
}
