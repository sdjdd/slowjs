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

pub type ConstantPool = Vec<JsValue>;

#[derive(Debug, Clone)]
pub struct CodeBlock {
    pub code: Vec<OpCode>,
    pub constants: ConstantPool,
}

#[derive(Debug, Clone)]
pub struct JsFunction {
    pub name: String,
    pub arity: usize,
    pub code_block: CodeBlock,
}

pub struct CallFrame {
    pub code_block: CodeBlock,
    pub ip: usize,
    pub base: usize,
}

impl CallFrame {
    pub fn new(bytecode: Vec<OpCode>, constants: ConstantPool, base: usize) -> Self {
        Self {
            ip: 0,
            code_block: CodeBlock {
                code: bytecode,
                constants,
            },
            base,
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

    PushConstant(usize),
    GetLocal(usize),
    SetLocal(usize),
    GetGlobal(usize),
    SetGlobal(usize),

    /// Call a function with n arguments
    Call(usize),
    Return,
}

pub struct Vm {
    stack: Vec<JsValue>,
    frames: Vec<CallFrame>,
    globals: Vec<JsValue>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: Vec::new(),
        }
    }

    pub fn run_script(
        &mut self,
        bytecode: &[OpCode],
        constants: &ConstantPool,
    ) -> Result<(), RuntimeError> {
        self.stack.clear();

        if self.frames.is_empty() {
            self.frames
                .push(CallFrame::new(bytecode.to_vec(), constants.to_vec(), 0));
        } else {
            let frame = self.frames.first_mut().unwrap();
            frame.code_block.code = bytecode.to_vec();
            frame.code_block.constants = constants.to_vec();
        }

        self.frames[0].ip = 0;

        self.run_loop()
    }

    fn run_loop(&mut self) -> Result<(), RuntimeError> {
        while !self.frames.is_empty() {
            let frame = self.frames.last_mut().unwrap();

            if frame.ip >= frame.code_block.code.len() {
                self.stack.truncate(frame.base);
                self.frames.pop();
                self.stack.push(JsValue::Undefined);
                continue;
            }

            let op = &frame.code_block.code[frame.ip];
            frame.ip += 1;

            println!("{:?}", op);

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
                OpCode::PushConstant(index) => {
                    self.stack.push(frame.code_block.constants[*index].clone());
                }
                OpCode::GetLocal(index) => {
                    let value = self.stack[frame.base + 1 + index].clone();
                    self.stack.push(value);
                }
                OpCode::SetLocal(index) => {
                    let value = self.stack.pop().unwrap();
                    let index = frame.base + 1 + index;
                    if index >= self.stack.len() {
                        self.stack.resize(index + 1, JsValue::Undefined);
                    }
                    self.stack[index] = value;
                }
                OpCode::GetGlobal(index) => {
                    let value = self.globals[*index].clone();
                    self.stack.push(value);
                }
                OpCode::SetGlobal(index) => {
                    let value = self.stack.pop().unwrap();
                    if *index >= self.globals.len() {
                        self.globals.resize(*index + 1, JsValue::Undefined);
                    }
                    self.globals[*index] = value;
                }
                OpCode::Call(arg_count) => {
                    // Stack: [func, arg1, arg2, ...] <- top
                    let base = self.stack.len() - 1 - *arg_count;
                    let func_val = self.stack[base].clone();

                    match func_val {
                        JsValue::Function(func) => {
                            for _ in 0..func.arity - *arg_count {
                                self.stack.push(JsValue::Undefined);
                            }

                            self.frames.push(CallFrame {
                                code_block: func.code_block.clone(),
                                ip: 0,
                                base,
                            });
                        }
                        _ => panic!("not a function"),
                    }
                }
                OpCode::Return => {
                    let value = self.stack.pop().unwrap();
                    self.stack.truncate(frame.base);
                    self.frames.pop();
                    self.stack.push(value);
                }
            }
        }

        Ok(())
    }

    pub fn pop(&mut self) -> Option<JsValue> {
        self.stack.pop()
    }
}
