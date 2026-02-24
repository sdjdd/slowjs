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
pub struct JsFunction {
    pub name: String,
    pub params: Vec<String>,
    pub bytecode: Vec<OpCode>,
    pub constants: ConstantPool,
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub depth: usize,
    pub slot: usize,
}

pub struct CallFrame {
    pub bytecode: Vec<OpCode>,
    pub ip: usize,
    pub slots: Vec<JsValue>,
    pub constants: ConstantPool,
}

impl CallFrame {
    pub fn new(bytecode: Vec<OpCode>, constants: ConstantPool, slot_count: usize) -> Self {
        Self {
            bytecode,
            ip: 0,
            slots: vec![JsValue::Undefined; slot_count],
            constants,
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
    DeclareLocal(usize),
    GetLocal(usize),
    SetLocal(usize),

    /// Call a function with n arguments
    Call(usize),
    Return,
}

pub struct Vm {
    stack: Vec<JsValue>,
    frames: Vec<CallFrame>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub fn run_script(
        &mut self,
        bytecode: &[OpCode],
        constants: &ConstantPool,
    ) -> Result<(), RuntimeError> {
        if self.frames.is_empty() {
            self.frames
                .push(CallFrame::new(bytecode.to_vec(), constants.to_vec(), 0));
        } else {
            let frame = self.frames.first_mut().unwrap();
            frame.bytecode = bytecode.to_vec();
            frame.constants = constants.to_vec();
        }

        self.frames[0].ip = 0;
        self.stack.clear();

        self.run_loop()
    }

    fn run_loop(&mut self) -> Result<(), RuntimeError> {
        while !self.frames.is_empty() {
            let (op, ip) = {
                let frame = self.frames.last().unwrap();
                if frame.ip >= frame.bytecode.len() {
                    self.frames.pop();
                    continue;
                }

                let op = frame.bytecode[frame.ip].clone();
                (op, frame.ip)
            };

            self.frames.last_mut().unwrap().ip = ip + 1;

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
                    let frame = self.frames.last().unwrap();
                    self.stack.push(frame.constants[index].clone());
                }

                OpCode::DeclareLocal(count) => {
                    let frame = self.frames.last_mut().unwrap();
                    frame.slots.resize(count, JsValue::Undefined);
                }
                OpCode::GetLocal(slot) => {
                    let frame = self.frames.last().unwrap();
                    self.stack.push(frame.slots[slot].clone());
                }
                OpCode::SetLocal(slot) => {
                    let value = self.stack.pop().unwrap();
                    let frame = self.frames.last_mut().unwrap();
                    frame.slots.resize(slot + 1, JsValue::Undefined);
                    frame.slots[slot] = value;
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
                            let mut frame_slots = vec![JsValue::Undefined; func.params.len()];
                            for (i, arg) in args.into_iter().enumerate() {
                                frame_slots[i] = arg;
                            }

                            self.frames.push(CallFrame {
                                bytecode: func.bytecode.clone(),
                                ip: 0,
                                slots: frame_slots,
                                constants: func.constants.clone(),
                            });
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
