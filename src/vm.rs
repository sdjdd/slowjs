use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use thiserror::Error;

#[derive(Debug, Clone)]
pub enum JsValue {
    Null,
    Undefined,
    Boolean(bool),
    Number(f64),
    String(String),
    Function(Rc<JsFunction>),
}

#[derive(Debug, Clone)]
pub struct JsFunction {
    pub name: String,
    pub params: Vec<String>,
    pub bytecode: Vec<OpCode>,
}

pub struct CallFrame {
    pub bytecode: Vec<OpCode>,
    pub ip: usize,
    pub env: Rc<RefCell<Environment>>,
}

impl CallFrame {
    pub fn new(bytecode: Vec<OpCode>, env: Rc<RefCell<Environment>>) -> Self {
        Self {
            bytecode,
            ip: 0,
            env,
        }
    }
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
            JsValue::Function(func) => write!(f, "[Function: {}]", func.name),
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

    Add,
    Sub,

    Halt,

    SlowPushValue(JsValue),
    SlowDeclareVar,
    SlowGetVar,
    SlowSetVar,

    /// Call a function with n arguments
    Call(usize),
    Return,
}

#[derive(Debug, Clone)]
pub struct Environment {
    bindings: HashMap<String, JsValue>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<JsValue> {
        self.bindings
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().get(name)))
    }

    pub fn set(&mut self, name: String, value: JsValue) {
        self.bindings.insert(name, value);
    }
}

pub struct Vm {
    pub(crate) stack: Vec<JsValue>,
    frames: Vec<CallFrame>,
}

impl Vm {
    pub fn new() -> Self {
        let global_env = Rc::new(RefCell::new(Environment::new()));
        Self {
            stack: Vec::new(),
            frames: vec![CallFrame::new(Vec::new(), global_env)],
        }
    }

    fn current_env(&self) -> Rc<RefCell<Environment>> {
        self.frames.last().unwrap().env.clone()
    }

    pub fn run(&mut self, bytecode: &[OpCode]) -> Result<(), RuntimeError> {
        let global_env = self.frames[0].env.clone();
        self.frames = vec![CallFrame::new(bytecode.to_vec(), global_env)];

        self.stack.clear();

        while !self.frames.is_empty() {
            let frame = self.frames.last().unwrap();
            if frame.ip >= frame.bytecode.len() {
                self.frames.pop();
                continue;
            }

            let op = frame.bytecode[frame.ip].clone();
            self.frames.last_mut().unwrap().ip += 1;

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
                            self.current_env()
                                .borrow_mut()
                                .set(name.clone(), JsValue::Undefined);
                        }
                        _ => panic!("Invalid variable name"),
                    };
                }
                OpCode::SlowGetVar => {
                    let name = self.stack.pop().unwrap();
                    match name {
                        JsValue::String(name) => {
                            let result = self.current_env().borrow().get(&name);
                            self.stack.push(
                                result.ok_or_else(|| RuntimeError::ReferenceError(name.clone()))?,
                            );
                        }
                        _ => panic!("Invalid variable name"),
                    }
                }
                OpCode::SlowSetVar => {
                    let name = self.stack.pop().unwrap();
                    let value = self.stack.pop().unwrap();
                    match name {
                        JsValue::String(name) => {
                            self.current_env().borrow_mut().set(name, value);
                        }
                        _ => panic!("Invalid variable name"),
                    }
                }

                // Stack: [func, arg1, arg2, ...]
                OpCode::Call(arg_count) => {
                    let mut args = Vec::new();
                    for _ in 0..arg_count {
                        args.push(self.stack.pop().unwrap());
                    }
                    args.reverse();

                    let func_val = self.stack.pop().unwrap();

                    match func_val {
                        JsValue::Function(func) => {
                            let func_env =
                                Rc::new(RefCell::new(Environment::with_parent(self.current_env())));

                            {
                                let mut env_mut = func_env.borrow_mut();
                                for (i, param) in func.params.iter().enumerate() {
                                    let arg = args.get(i).cloned().unwrap_or(JsValue::Undefined);
                                    env_mut.set(param.clone(), arg);
                                }
                            }

                            self.frames
                                .push(CallFrame::new(func.bytecode.clone(), func_env));
                        }
                        _ => panic!("not a function"),
                    }
                }

                OpCode::Return => {
                    self.frames.pop();
                }
            }
        }

        Ok(())
    }

    pub fn value(&mut self) -> Option<&JsValue> {
        self.stack.last()
    }
}
