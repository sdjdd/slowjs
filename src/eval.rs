use crate::ast::{
    BinaryExpression, BinaryOperator, BlockStatement, Declaration, Expression, ExpressionStatement,
    Identifier, Literal, ObjectExpression, Program, PropertyKey, PropertyKind, Statement,
    VariableDeclaration,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

pub trait Eval {
    fn eval(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, EvalError>;
}

#[derive(Clone)]
pub enum Value {
    Null,
    Undefined,
    Boolean(bool),
    Number(f64),
    String(String),
    Object(HashMap<String, Value>),
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
            Value::Object(obj) => {
                write!(f, "{{ ")?;
                for (i, (key, value)) in obj.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, " }}")
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("ReferenceError: {0} is not defined")]
    ReferenceError(String),
}

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
        last_value = eval_statement(stmt, &env)?;
    }

    Ok(last_value)
}

fn eval_statement(stmt: &Statement, env: &Rc<RefCell<Environment>>) -> Result<Value, EvalError> {
    match stmt {
        Statement::ExpressionStatement(ExpressionStatement { expression }) => {
            eval_expression(&expression, env)
        }
        Statement::BlockStatement(BlockStatement { body }) => eval_block(body, env),
        Statement::EmptyStatement => Ok(Value::Undefined),
        Statement::Declaration(Declaration::VariableDeclaration(decl)) => {
            eval_variable_declaration(decl, env)
        }
    }
}

fn eval_block(
    statements: &[Statement],
    env: &Rc<RefCell<Environment>>,
) -> Result<Value, EvalError> {
    let mut last_value = Value::Undefined;
    for stmt in statements {
        last_value = eval_statement(stmt, env)?;
    }
    Ok(last_value)
}

fn eval_variable_declaration(
    decl: &VariableDeclaration,
    env: &Rc<RefCell<Environment>>,
) -> Result<Value, EvalError> {
    for declarator in &decl.declarations {
        let value = match &declarator.init {
            Some(init) => eval_expression(init, env)?,
            None => Value::Undefined,
        };
        env.borrow_mut().set(declarator.id.name.clone(), value);
    }
    Ok(Value::Undefined)
}

fn eval_expression(expr: &Expression, env: &Rc<RefCell<Environment>>) -> Result<Value, EvalError> {
    match expr {
        Expression::Literal(literal) => literal.eval(env),
        Expression::Identifier(Identifier { name }) => eval_identifier(name, env),
        Expression::BinaryExpression(BinaryExpression {
            operator,
            left,
            right,
        }) => eval_binary(left, operator, right, env),
        Expression::ObjectExpression(obj) => eval_object_expression(obj, env),
    }
}

fn eval_identifier(name: &str, env: &Rc<RefCell<Environment>>) -> Result<Value, EvalError> {
    env.borrow()
        .get(name)
        .ok_or_else(|| EvalError::ReferenceError(name.to_string()))
}

fn eval_object_expression(
    obj: &ObjectExpression,
    env: &Rc<RefCell<Environment>>,
) -> Result<Value, EvalError> {
    let mut props = HashMap::new();

    for property in &obj.properties {
        // ignore getter/setter until functions are implemented
        if property.kind != PropertyKind::Init {
            continue;
        }

        let key = match &property.key {
            PropertyKey::Identifier(ident) => ident.name.clone(),
            PropertyKey::Literal(literal) => match literal {
                Literal::String(s) => s.clone(),
                Literal::Number(n) => n.to_string(),
                _ => continue,
            },
        };

        let value = eval_expression(&property.value, env)?;
        props.insert(key, value);
    }

    Ok(Value::Object(props))
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

impl Eval for Literal {
    fn eval(&self, _env: &Rc<RefCell<Environment>>) -> Result<Value, EvalError> {
        match self {
            Literal::Null => Ok(Value::Null),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::String(s) => Ok(Value::String(s.clone())),
        }
    }
}
