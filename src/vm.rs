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
    Object(Rc<JsObject>),
}

#[derive(Debug, Clone)]
pub struct JsObject {
    pub properties: HashMap<String, JsValue>,
}

impl JsObject {
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
        }
    }
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
            JsValue::Object(obj) => {
                let props: Vec<String> = obj
                    .properties
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "{{{}}}", props.join(", "))
            }
        }
    }
}

impl JsValue {
    pub fn to_bool(&self) -> bool {
        match self {
            JsValue::Null => false,
            JsValue::Undefined => false,
            JsValue::Boolean(b) => *b,
            JsValue::Number(n) => *n != 0.0,
            JsValue::String(s) => !s.is_empty(),
            JsValue::Function(_) => true,
            JsValue::Object(_) => true,
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
    Mul,
    Div,

    // Comparison
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    Halt,

    PushConstant(usize),
    GetLocal(usize),
    SetLocal(usize),
    GetGlobal(usize),
    SetGlobal(usize),

    /// Call a function with n arguments
    Call(usize),
    Return,

    /// Jump to instruction at index
    Jump(usize),
    /// Jump if top of stack is falsy
    JumpIfFalse(usize),

    NewObject,
    /// Stack: [object, key, value]
    SetProperty(usize),
    /// Stack: [object, key]
    GetProperty(usize),
}

pub struct Vm {
    stack: Vec<JsValue>,
    frames: Vec<CallFrame>,
    globals: Vec<JsValue>,
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::new(),
            frames: Vec::new(),
            globals: Vec::new(),
        };

        // Initialize builtin functions
        vm.globals.push(JsValue::Function(Rc::new(JsFunction {
            name: "print".to_string(),
            arity: 0, // Variable arity handled in Call
            code_block: CodeBlock {
                code: vec![],
                constants: vec![],
            },
        })));

        vm
    }

    pub fn run_script(
        &mut self,
        bytecode: &[OpCode],
        constants: &ConstantPool,
    ) -> Result<(), RuntimeError> {
        self.stack.clear();
        self.frames.clear();

        self.frames
            .push(CallFrame::new(bytecode.to_vec(), constants.to_vec(), 0));

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
                OpCode::Mul => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => {
                            self.stack.push(JsValue::Number(a * b));
                        }
                        _ => unimplemented!(),
                    }
                }
                OpCode::Div => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => {
                            self.stack.push(JsValue::Number(a / b));
                        }
                        _ => unimplemented!(),
                    }
                }
                OpCode::Eq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a == b,
                        (JsValue::String(a), JsValue::String(b)) => a == b,
                        (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
                        (JsValue::Null, JsValue::Null) => true,
                        (JsValue::Undefined, JsValue::Undefined) => true,
                        _ => false,
                    };
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::NotEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a != b,
                        (JsValue::String(a), JsValue::String(b)) => a != b,
                        (JsValue::Boolean(a), JsValue::Boolean(b)) => a != b,
                        (JsValue::Null, JsValue::Null) => false,
                        (JsValue::Undefined, JsValue::Undefined) => false,
                        _ => true,
                    };
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::Less => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a < b,
                        (JsValue::String(a), JsValue::String(b)) => a < b,
                        _ => false,
                    };
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::LessEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a <= b,
                        (JsValue::String(a), JsValue::String(b)) => a <= b,
                        _ => false,
                    };
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::Greater => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a > b,
                        (JsValue::String(a), JsValue::String(b)) => a > b,
                        _ => false,
                    };
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::GreaterEq => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a >= b,
                        (JsValue::String(a), JsValue::String(b)) => a >= b,
                        _ => false,
                    };
                    self.stack.push(JsValue::Boolean(result));
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

                    // Check for builtin functions
                    if let JsValue::Function(func) = &func_val {
                        if func.name == "print" {
                            // Builtin print function - join args with space
                            for i in 0..*arg_count {
                                let arg = &self.stack[base + 1 + i];
                                if i > 0 {
                                    print!(" ");
                                }
                                // Print strings without quotes
                                match arg {
                                    JsValue::String(s) => print!("{}", s),
                                    _ => print!("{}", arg),
                                }
                            }
                            println!();
                            self.stack.truncate(base);
                            self.stack.push(JsValue::Undefined);
                            continue;
                        }
                    }

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
                OpCode::Jump(addr) => {
                    frame.ip = *addr;
                }
                OpCode::JumpIfFalse(addr) => {
                    let value = self.stack.pop().unwrap();
                    if !value.to_bool() {
                        frame.ip = *addr;
                    }
                }
                OpCode::NewObject => {
                    let obj = JsObject::new();
                    self.stack.push(JsValue::Object(Rc::new(obj)));
                }
                OpCode::SetProperty(key_index) => {
                    let value = self.stack.pop().unwrap();
                    let key = &frame.code_block.constants[*key_index];
                    let obj_val = self.stack.pop().unwrap();

                    if let JsValue::Object(obj) = obj_val {
                        let mut props = (*obj).clone();
                        let key_str = match key {
                            JsValue::String(s) => s.clone(),
                            JsValue::Number(n) => n.to_string(),
                            _ => key.to_string(),
                        };
                        props.properties.insert(key_str, value);
                        self.stack.push(JsValue::Object(Rc::new(props)));
                    } else {
                        panic!("not an object");
                    }
                }
                OpCode::GetProperty(key_index) => {
                    let key = &frame.code_block.constants[*key_index];
                    let obj_val = self.stack.pop().unwrap();

                    if let JsValue::Object(obj) = obj_val {
                        let key_str = match key {
                            JsValue::String(s) => s.clone(),
                            JsValue::Number(n) => n.to_string(),
                            _ => key.to_string(),
                        };
                        let value = obj
                            .properties
                            .get(&key_str)
                            .cloned()
                            .unwrap_or(JsValue::Undefined);
                        self.stack.push(value);
                    } else {
                        panic!("not an object");
                    }
                }
            }
        }

        Ok(())
    }

    pub fn pop(&mut self) -> Option<JsValue> {
        self.stack.pop()
    }
}
