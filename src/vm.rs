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

#[derive(Debug, Clone)]
pub enum OpCode {
    PushConst(usize),
    Add,
    Sub,

    Halt,
}

pub struct Vm {
    pub(crate) stack: Vec<JsValue>,
    pub(crate) constants: Vec<JsValue>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn set_constants(&mut self, constants: Vec<JsValue>) {
        self.constants = constants;
    }

    pub fn clear_stack(&mut self) {
        self.stack.clear();
    }

    pub fn run(&mut self, bytecode: &[OpCode]) {
        for op in bytecode {
            match op {
                OpCode::PushConst(idx) => {
                    self.stack.push(self.constants[*idx].clone());
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
            }
        }
    }

    pub fn value(&mut self) -> Option<&JsValue> {
        self.stack.last()
    }
}
