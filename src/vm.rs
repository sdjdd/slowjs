use std::collections::HashMap;
use std::collections::hash_map::Entry;

use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum JsValue {
    Null,
    Undefined,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl std::fmt::Display for JsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsValue::Null => write!(f, "null"),
            JsValue::Undefined => write!(f, "undefined"),
            JsValue::Boolean(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            JsValue::Number(n) => {
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
            JsValue::String(s) => write!(f, "'{s}'"),
        }
    }
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("{0} is not defined")]
    ReferenceError(String),
    #[error("Identifier '{0}' has already been declared")]
    SyntaxError(String),
}

#[derive(Debug, Clone)]
pub enum OpCode {
    PushNull,
    PushUndefined,
    PushTrue,
    PushFalse,
    PushConst(usize),

    Add,
    Sub,

    Halt,

    SlowPushValue(JsValue),
    SlowDeclareVar,
    SlowLoadVar,
    SlowPushVar,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum VmConstant {
    String(String),
}

struct Environment {
    bindings: HashMap<String, JsValue>,
}

impl Environment {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
}

pub struct Vm {
    pub(crate) stack: Vec<JsValue>,
    pub(crate) constants: Vec<VmConstant>,
    env: Environment,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            constants: Vec::new(),
            env: Environment::new(),
        }
    }

    pub fn set_constants(&mut self, constants: Vec<VmConstant>) {
        self.constants = constants;
    }

    pub fn clear_stack(&mut self) {
        self.stack.clear();
    }

    pub fn run(&mut self, bytecode: &[OpCode]) -> Result<(), RuntimeError> {
        for op in bytecode {
            match op {
                OpCode::PushNull => {
                    self.stack.push(JsValue::Null);
                }
                OpCode::PushUndefined => {
                    self.stack.push(JsValue::Undefined);
                }
                OpCode::PushTrue => {
                    self.stack.push(JsValue::Boolean(true));
                }
                OpCode::PushFalse => {
                    self.stack.push(JsValue::Boolean(false));
                }
                OpCode::PushConst(idx) => match &self.constants[*idx] {
                    VmConstant::String(s) => {
                        self.stack.push(JsValue::String(s.clone()));
                    }
                },
                OpCode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => {
                            self.stack.push(JsValue::Number(a + b));
                        }
                        (JsValue::String(a), JsValue::String(b)) => {
                            self.stack.push(JsValue::String(a + &b));
                        }
                        _ => unimplemented!(),
                    }
                }
                OpCode::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => {
                            self.stack.push(JsValue::Number(a - b));
                        }
                        _ => unimplemented!(),
                    }
                }
                OpCode::Halt => {
                    break;
                }

                OpCode::SlowPushValue(val) => self.stack.push(val.clone()),
                OpCode::SlowDeclareVar => {
                    let name = self.stack.pop().unwrap();
                    match name {
                        JsValue::String(name) => {
                            self.env.bindings.insert(name.clone(), JsValue::Undefined)
                        }
                        _ => panic!("Invalid variable name"),
                    };
                }
                OpCode::SlowPushVar => {
                    let name = self.stack.pop().unwrap();
                    match name {
                        JsValue::String(name) => self
                            .env
                            .bindings
                            .get(&name)
                            .map(|v| self.stack.push(v.clone()))
                            .ok_or_else(|| RuntimeError::ReferenceError(name))
                            .map(|_| ())?,
                        _ => panic!("Invalid variable name"),
                    }
                }
                OpCode::SlowLoadVar => {
                    let name = self.stack.pop().unwrap();
                    match name {
                        JsValue::String(name) => match self.env.bindings.entry(name.clone()) {
                            Entry::Occupied(mut entry) => {
                                entry.insert(self.stack.pop().unwrap());
                            }
                            Entry::Vacant(_) => return Err(RuntimeError::SyntaxError(name)),
                        },
                        _ => panic!("Invalid variable name"),
                    }
                }
            }
        }

        Ok(())
    }

    pub fn value(&mut self) -> Option<&JsValue> {
        self.stack.last()
    }
}
