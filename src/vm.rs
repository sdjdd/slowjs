use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use thiserror::Error;

use crate::js_std;

#[derive(Clone)]
pub enum JsValue {
    Null,
    Undefined,
    Boolean(bool),
    Number(f64),
    String(String),
    Object(Gc<JsObject>),
    Function(Gc<JsFunction>),
}

impl Debug for JsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ctx = FormatContext::new();
        // Pre-scan to count all references
        ctx.count_references(self, 0);
        self.fmt_inner(f, &mut ctx)
    }
}

pub type Gc<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone)]
pub struct JsObject {
    pub prototype: Option<Gc<JsObject>>,
    pub properties: HashMap<String, JsValue>,
}

impl JsObject {
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
            prototype: None,
        }
    }

    pub fn set(&mut self, k: String, v: JsValue) {
        self.properties.insert(k, v);
    }

    pub fn get(&self, k: &str) -> JsValue {
        self.properties
            .get(k)
            .cloned()
            .unwrap_or(JsValue::Undefined)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Number(u64),
    String(String),
    Function(FunctionTemplate),
}

impl From<Constant> for JsValue {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Number(n) => JsValue::Number(f64::from_bits(n)),
            Constant::String(s) => JsValue::String(s),
            Constant::Function(f) => JsValue::Function(Rc::new(RefCell::new(JsFunction::new(
                f.name,
                f.arity,
                FunctionBody::Code(f.code_block),
            )))),
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", f64::from_bits(*n)),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Function(func) => write!(f, "[Function: {}]", func.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionTemplate {
    pub name: String,
    pub arity: usize,
    pub code_block: Rc<CodeBlock>,
}

pub type ConstantPool = Vec<Constant>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CodeBlock {
    pub code: Vec<OpCode>,
    pub constants: ConstantPool,
}

#[derive(Clone)]
pub enum FunctionBody {
    Code(Rc<CodeBlock>),
    Native(fn(&[JsValue]) -> Option<JsValue>),
}

#[derive(Clone)]
pub struct JsFunction {
    pub prototype: Gc<JsObject>,
    pub name: String,
    pub arity: usize,
    pub body: FunctionBody,
}

impl JsFunction {
    pub fn new(name: String, arity: usize, body: FunctionBody) -> Self {
        Self {
            prototype: Rc::new(RefCell::new(JsObject::new())),
            name,
            arity,
            body,
        }
    }
}

/// Context for formatting JsValue with circular reference tracking
struct FormatContext {
    obj_depths: HashMap<usize, usize>,
    ref_ids: HashMap<usize, usize>,
    next_ref_id: usize,
}

impl Default for FormatContext {
    fn default() -> Self {
        Self {
            obj_depths: HashMap::new(),
            ref_ids: HashMap::new(),
            next_ref_id: 1,
        }
    }
}

impl FormatContext {
    fn new() -> Self {
        Self::default()
    }

    fn count_references(&mut self, value: &JsValue, depth: usize) {
        if let JsValue::Object(obj) = value {
            let ptr = Rc::as_ptr(obj) as usize;
            let obj_depth = self.obj_depths.entry(ptr).or_insert(depth);
            if depth > *obj_depth {
                self.ref_ids.entry(ptr).or_insert_with(|| {
                    let id = self.next_ref_id;
                    self.next_ref_id += 1;
                    id
                });
                return;
            }
            let obj = &*obj.borrow();
            for v in obj.properties.values() {
                self.count_references(v, depth + 1);
            }
        }
    }
}

pub struct CallFrame {
    pub base: usize,
    pub ip: usize,
    pub code: Rc<CodeBlock>,
}

impl CallFrame {
    pub fn new(code: Rc<CodeBlock>) -> Self {
        Self {
            base: 0,
            ip: 0,
            code,
        }
    }
}

impl std::fmt::Display for JsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ctx = FormatContext::new();
        // Pre-scan to count all references
        ctx.count_references(self, 0);
        self.fmt_inner(f, &mut ctx)
    }
}

impl JsValue {
    fn fmt_inner(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ctx: &mut FormatContext,
    ) -> std::fmt::Result {
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
            JsValue::String(s) => write!(f, "{s}"),
            JsValue::Object(obj) => {
                let ptr = Rc::as_ptr(obj) as usize;
                let obj = &*obj.borrow();
                if let Some(id) = ctx.ref_ids.get(&ptr) {
                    write!(f, "<ref *{}> ", id)?;
                }
                write!(f, "{{ ")?;
                let mut first = true;
                for (k, v) in &obj.properties {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}: ", k)?;
                    match v {
                        JsValue::Object(o) => {
                            let ptr = Rc::as_ptr(o) as usize;
                            if let Some(id) = ctx.ref_ids.get(&ptr) {
                                write!(f, "[Circular *{}]", id)?;
                            } else {
                                v.fmt_inner(f, ctx)?;
                            }
                        }
                        _ => v.fmt_inner(f, ctx)?,
                    };
                }
                write!(f, " }}")
            }
            JsValue::Function(func) => write!(f, "[Function: {}]", func.borrow().name),
        }
    }
}

impl JsValue {
    pub fn to_bool(&self) -> bool {
        match self {
            JsValue::Null | JsValue::Undefined => false,
            JsValue::Boolean(b) => *b,
            JsValue::Number(n) => *n != 0.0,
            JsValue::String(s) => !s.is_empty(),
            JsValue::Object(_) | JsValue::Function(_) => true,
        }
    }
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("{0} is not defined")]
    ReferenceError(String),
    #[error("Identifier '{0}' has already been declared")]
    SyntaxError(String),
    #[error("Type error: {0}")]
    TypeError(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    /// Set object property by string constant id, keep value on stack.
    ///
    /// Stack: `[object, value]` -> `[object]`
    InitProperty(usize),

    /// Set object property by string constant id, keep value on stack.
    ///
    /// Stack: `[object, value]` -> `[value]`
    SetProperty(usize),

    /// Get object property by string constant id.
    ///
    /// Stack: `[object]` -> `[value]`
    GetProperty(usize),

    /// Get object element.
    ///
    /// Stack: `[object, key]` -> `[value]`
    GetElement,

    /// Set object element.
    ///
    /// Stack: `[object, key, value]` -> `[value]`
    SetElement,

    NewFunc(usize),
}

// ============ Abstract Operations for Relational Comparison ============

/// ToNumber abstract operation (ECMAScript 7.1.4)
fn to_number(value: &JsValue) -> f64 {
    match value {
        JsValue::Undefined => f64::NAN,
        JsValue::Null => 0.0,
        JsValue::Boolean(b) => {
            if *b {
                1.0
            } else {
                0.0
            }
        }
        JsValue::Number(n) => *n,
        JsValue::String(s) => {
            if s.is_empty() {
                0.0
            } else {
                s.parse::<f64>().unwrap_or(f64::NAN)
            }
        }
        JsValue::Object(_) | JsValue::Function(_) => f64::NAN,
    }
}

/// Relational comparison helper using ToNumber conversion
fn relational_cmp<F>(left: &JsValue, right: &JsValue, cmp: F) -> bool
where
    F: Fn(f64, f64) -> bool,
{
    let left_num = to_number(left);
    let right_num = to_number(right);

    // NaN comparisons always return false
    if left_num.is_nan() || right_num.is_nan() {
        false
    } else {
        cmp(left_num, right_num)
    }
}

/// IsLessThan abstract operation (ECMAScript 7.2.12)
fn is_less_than(left: &JsValue, right: &JsValue) -> bool {
    relational_cmp(left, right, |a, b| a < b)
}

/// LessThanOrEqual operator (<=)
fn less_than_or_equal(left: &JsValue, right: &JsValue) -> bool {
    relational_cmp(left, right, |a, b| a <= b)
}

/// GreaterThan operator (>)
fn is_greater_than(left: &JsValue, right: &JsValue) -> bool {
    relational_cmp(left, right, |a, b| a > b)
}

/// GreaterThanOrEqual operator (>=)
fn is_greater_than_or_equal(left: &JsValue, right: &JsValue) -> bool {
    relational_cmp(left, right, |a, b| a >= b)
}

// ============ Virtual Machine ============

pub struct Vm {
    stack: Vec<JsValue>,
    frames: Vec<CallFrame>,
    global_obj: Gc<JsObject>,
}

impl Vm {
    pub fn new() -> Self {
        let global_obj = Rc::new(RefCell::new(JsObject::new()));

        js_std::new_std().into_iter().for_each(|(name, value)| {
            global_obj.borrow_mut().set(name, value);
        });

        global_obj.borrow_mut().set(
            "globalThis".to_string(),
            JsValue::Object(global_obj.clone()),
        );
        global_obj
            .borrow_mut()
            .set("window".to_string(), JsValue::Object(global_obj.clone()));

        Self {
            stack: Vec::new(),
            frames: Vec::new(),
            global_obj,
        }
    }

    /// Helper to pop two values from stack (right operand first, then left)
    fn pop_binary(&mut self) -> (JsValue, JsValue) {
        let b = self
            .stack
            .pop()
            .expect("stack underflow in binary operation");
        let a = self
            .stack
            .pop()
            .expect("stack underflow in binary operation");
        (a, b)
    }

    /// Handle arithmetic binary operations
    fn binary_arithmetic(&mut self, op: OpCode, op_name: &'static str) -> Result<(), RuntimeError> {
        let (a, b) = self.pop_binary();
        let result = match (op, &a, &b) {
            (OpCode::Add, JsValue::String(a), JsValue::String(b)) => {
                JsValue::String(format!("{}{}", a, b))
            }
            (OpCode::Add, JsValue::Number(a), JsValue::Number(b)) => JsValue::Number(*a + *b),
            (OpCode::Sub, JsValue::Number(a), JsValue::Number(b)) => JsValue::Number(*a - *b),
            (OpCode::Mul, JsValue::Number(a), JsValue::Number(b)) => JsValue::Number(*a * *b),
            (OpCode::Div, JsValue::Number(a), JsValue::Number(b)) => JsValue::Number(*a / *b),
            _ => {
                return Err(RuntimeError::TypeError(format!(
                    "invalid operands for {}",
                    op_name
                )));
            }
        };
        self.stack.push(result);
        Ok(())
    }

    /// Handle comparison binary operations
    fn binary_comparison<F>(&mut self, _op_name: &'static str, cmp: F) -> Result<(), RuntimeError>
    where
        F: Fn(&JsValue, &JsValue) -> bool,
    {
        let (a, b) = self.pop_binary();
        let result = cmp(&a, &b);
        self.stack.push(JsValue::Boolean(result));
        Ok(())
    }

    pub fn run_script(
        &mut self,
        bytecode: &[OpCode],
        constants: &ConstantPool,
    ) -> Result<(), RuntimeError> {
        self.stack.clear();
        self.frames.clear();

        self.frames.push(CallFrame::new(Rc::new(CodeBlock {
            code: bytecode.to_vec(),
            constants: constants.to_vec(),
        })));

        self.run_loop()
    }

    fn run_loop(&mut self) -> Result<(), RuntimeError> {
        while let Some(frame) = self.frames.last_mut() {
            if frame.ip >= frame.code.code.len() {
                self.stack.truncate(frame.base);
                self.stack.push(JsValue::Undefined);
                self.frames.pop();
                continue;
            }

            let op = &frame.code.code[frame.ip];
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
                    self.binary_arithmetic(OpCode::Add, "+")?;
                }
                OpCode::Sub => {
                    self.binary_arithmetic(OpCode::Sub, "-")?;
                }
                OpCode::Mul => {
                    self.binary_arithmetic(OpCode::Mul, "*")?;
                }
                OpCode::Div => {
                    self.binary_arithmetic(OpCode::Div, "/")?;
                }
                OpCode::Eq => {
                    self.binary_comparison("==", |a, b| match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a == b,
                        (JsValue::String(a), JsValue::String(b)) => a == b,
                        (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
                        (JsValue::Null, JsValue::Null) => true,
                        (JsValue::Undefined, JsValue::Undefined) => true,
                        _ => false,
                    })?;
                }
                OpCode::NotEq => {
                    self.binary_comparison("!=", |a, b| match (a, b) {
                        (JsValue::Number(a), JsValue::Number(b)) => a != b,
                        (JsValue::String(a), JsValue::String(b)) => a != b,
                        (JsValue::Boolean(a), JsValue::Boolean(b)) => a != b,
                        (JsValue::Null, JsValue::Null) => false,
                        (JsValue::Undefined, JsValue::Undefined) => false,
                        _ => true,
                    })?;
                }
                OpCode::Less => {
                    let (left, right) = self.pop_binary();
                    let result = is_less_than(&left, &right);
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::LessEq => {
                    let (left, right) = self.pop_binary();
                    let result = less_than_or_equal(&left, &right);
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::Greater => {
                    let (left, right) = self.pop_binary();
                    let result = is_greater_than(&left, &right);
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::GreaterEq => {
                    let (left, right) = self.pop_binary();
                    let result = is_greater_than_or_equal(&left, &right);
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::Halt => {
                    break;
                }
                OpCode::PushConstant(index) => {
                    let constant = &frame.code.constants[*index];
                    self.stack.push(constant.clone().into());
                }
                OpCode::GetLocal(index) => {
                    let value = self.stack[frame.base + 1 + index].clone();
                    self.stack.push(value);
                }
                OpCode::SetLocal(index) => {
                    let value = self.stack.last().unwrap().clone();
                    let index = frame.base + 1 + index;
                    if index >= self.stack.len() {
                        self.stack.resize(index + 1, JsValue::Undefined);
                    }
                    self.stack[index] = value;
                }
                OpCode::GetGlobal(name_index) => {
                    let name = &frame.code.constants[*name_index];
                    let name_str = match name {
                        Constant::String(s) => s.clone(),
                        _ => unreachable!(),
                    };
                    let value = self.global_obj.borrow().get(&name_str);
                    self.stack.push(value);
                }
                OpCode::SetGlobal(name_index) => {
                    let value = self.stack.last().unwrap();
                    let name = &frame.code.constants[*name_index];
                    let name_str = match name {
                        Constant::String(s) => s.clone(),
                        _ => unreachable!(),
                    };
                    self.global_obj.borrow_mut().set(name_str, value.clone());
                }
                OpCode::Call(arg_count) => {
                    // Stack: [func, arg1, arg2, ...] <- top
                    let base = self.stack.len() - 1 - *arg_count;
                    let func_val = self.stack[base].clone();

                    // Check if it's a function object
                    if let JsValue::Function(func) = &func_val {
                        let func = &*func.borrow();
                        match &func.body {
                            FunctionBody::Code(code) => {
                                for _ in 1..func.arity.saturating_sub(*arg_count) {
                                    self.stack.push(JsValue::Undefined);
                                }
                                self.frames.push(CallFrame {
                                    base,
                                    ip: 0,
                                    code: code.clone(),
                                });
                            }
                            FunctionBody::Native(native_func) => {
                                let mut args =
                                    Vec::from(&self.stack[base + 1..base + 1 + *arg_count]);
                                for _ in 1..func.arity.saturating_sub(*arg_count) {
                                    args.push(JsValue::Undefined);
                                }
                                self.stack.truncate(base);
                                let return_value = native_func(&args).unwrap_or(JsValue::Undefined);
                                self.stack.push(return_value);
                            }
                        }

                        continue;
                    }

                    return Err(RuntimeError::TypeError("not a function".to_string()));
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
                    let gc_obj = Rc::new(RefCell::new(obj));
                    self.stack.push(JsValue::Object(gc_obj));
                }
                OpCode::InitProperty(const_id) => {
                    let value = self.stack.pop().unwrap();
                    let obj = self.stack.pop().unwrap();
                    let obj = to_object(obj);
                    let key = &frame.code.constants[*const_id];
                    obj.borrow_mut().set(key.to_string(), value);
                    self.stack.push(JsValue::Object(obj));
                }
                OpCode::SetProperty(key_index) => {
                    let value = self.stack.pop().unwrap();
                    let obj = self.stack.pop().unwrap();
                    let key = &frame.code.constants[*key_index];

                    let obj = match obj {
                        JsValue::Object(obj) => obj,
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    let key = match key {
                        Constant::String(s) => s.clone(),
                        _ => unreachable!(),
                    };

                    obj.borrow_mut().set(key, value);
                }
                OpCode::GetProperty(key_index) => {
                    let obj = match self.stack.pop().unwrap() {
                        JsValue::Object(obj) => obj,
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    let key = match &frame.code.constants[*key_index] {
                        Constant::String(s) => s,
                        _ => unreachable!(),
                    };

                    self.stack.push(obj.borrow().get(key));
                }
                OpCode::GetElement => {
                    let key_val = self.stack.pop().unwrap();
                    let obj = match self.stack.pop().unwrap() {
                        JsValue::Object(obj) => obj,
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    let key = match &key_val {
                        JsValue::String(s) => s.clone(),
                        JsValue::Number(n) => n.to_string(),
                        _ => unimplemented!(),
                    };

                    self.stack.push(obj.borrow().get(&key));
                }
                OpCode::SetElement => {
                    let value = self.stack.pop().unwrap();
                    let key_val = self.stack.pop().unwrap();
                    let obj = match self.stack.pop().unwrap() {
                        JsValue::Object(obj) => obj,
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    let key = match &key_val {
                        JsValue::String(s) => s.clone(),
                        JsValue::Number(n) => n.to_string(),
                        _ => unimplemented!(),
                    };

                    obj.borrow_mut().set(key, value);
                }
                OpCode::NewFunc(index) => {
                    let tmpl = match &frame.code.constants[*index] {
                        Constant::Function(func) => func,
                        _ => unreachable!(),
                    };
                    let func = JsFunction::new(
                        tmpl.name.clone(),
                        tmpl.arity,
                        FunctionBody::Code(tmpl.code_block.clone()),
                    );
                    let value = JsValue::Function(Rc::new(RefCell::new(func.clone())));
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

fn to_object(value: JsValue) -> Gc<JsObject> {
    match value {
        JsValue::Object(obj) => obj,
        JsValue::Function(func) => func.borrow().prototype.clone(),
        _ => Rc::new(RefCell::new(JsObject::new())),
    }
}
