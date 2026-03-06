use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use thiserror::Error;

use crate::js_std;
use crate::runtime::{
    self, CodeBlock, ConstantTable, FunctionBody, Gc, JsFunction, JsObject, JsValue, NativeFnCtx,
    PropertyDescriptor,
};

#[derive(Clone, PartialEq, Eq)]
pub enum Constant {
    Number(u64),
    String(String),
    Function(FunctionTemplate),
}

impl Debug for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", f64::from_bits(*n)),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Function(func) => write!(f, "[Function: {}]", func.name),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct FunctionTemplate {
    pub name: String,
    pub arity: usize,
    pub params: Vec<String>,
    pub code_block: Rc<CodeBlock>,
}

pub type ConstantPool = Vec<Constant>;

struct CallFrame {
    base: usize,
    ip: usize,
    code: Rc<CodeBlock>,
    env: Rc<RefCell<Env>>,
    this_value: JsValue,
    is_constructor: bool,
}

impl CallFrame {
    pub fn new(code: Rc<CodeBlock>, env: Rc<RefCell<Env>>) -> Self {
        Self {
            base: 0,
            ip: 0,
            code,
            env,
            this_value: JsValue::Undefined,
            is_constructor: false,
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
    /// Push this binding onto stack
    PushThis,

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

    // Logical
    LogicalNot,

    Dup,
    Pop,

    Halt,

    PushConstant(usize),
    GetLocal(usize),
    SetLocal(usize),
    GetGlobal(usize),
    SetGlobal(usize),

    DeclareVar(usize),
    SetVar(usize),
    GetVar(usize),

    /// Call a function with n arguments
    Call(usize),
    /// Construct an object using new. Stack: [constructor, prototype, args...]
    Construct(usize),
    Return,

    /// Jump to instruction at index
    Jump(usize),
    /// Jump if top of stack is truthy
    JumpIfTrue(usize),
    /// Jump if top of stack is falsy
    JumpIfFalse(usize),

    NewObject,

    /// Set object property by string constant id, keep object on stack.
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

    /// Stack: `[object, constructor]` -> `[boolean]`
    InstanceOf,
}

// ============ Abstract Operations for Relational Comparison ============

/// Relational comparison helper using ToNumber conversion
fn relational_cmp<F>(left: &JsValue, right: &JsValue, cmp: F) -> bool
where
    F: Fn(f64, f64) -> bool,
{
    let left_num = runtime::to_number(left);
    let right_num = runtime::to_number(right);

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

enum HeapObject {
    Object(JsObject),
    Function(JsFunction),
}

pub struct Heap {
    objects: Vec<Option<HeapObject>>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    fn alloc(&mut self, obj: HeapObject) -> usize {
        let index = self.objects.len();
        self.objects.push(Some(obj));
        index
    }

    pub fn alloc_object(&mut self, obj: JsObject) -> Gc<JsObject> {
        Gc::new(self.alloc(HeapObject::Object(obj)))
    }

    pub fn alloc_func(&mut self, func: JsFunction) -> Gc<JsFunction> {
        Gc::new(self.alloc(HeapObject::Function(func)))
    }

    pub fn get_object(&self, gc_ref: &Gc<JsObject>) -> &JsObject {
        match self.objects[gc_ref.index].as_ref().unwrap() {
            HeapObject::Object(obj) => obj,
            _ => panic!("Heap value mismatch"),
        }
    }

    pub fn get_func(&self, gc_ref: &Gc<JsFunction>) -> &JsFunction {
        match self.objects[gc_ref.index].as_ref().unwrap() {
            HeapObject::Function(func) => func,
            _ => panic!("Heap value mismatch"),
        }
    }

    pub fn get_object_mut(&mut self, gc_ref: &Gc<JsObject>) -> &mut JsObject {
        match self.objects[gc_ref.index].as_mut().unwrap() {
            HeapObject::Object(obj) => obj,
            _ => panic!("Heap value mismatch"),
        }
    }

    pub fn get_func_mut(&mut self, gc_ref: &Gc<JsFunction>) -> &mut JsFunction {
        match self.objects[gc_ref.index].as_mut().unwrap() {
            HeapObject::Function(func) => func,
            _ => panic!("Heap value mismatch"),
        }
    }
}

#[derive(Clone)]
pub struct Env {
    bindings: HashMap<String, JsValue>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(outer: Option<Rc<RefCell<Env>>>) -> Self {
        Self {
            bindings: HashMap::new(),
            outer,
        }
    }

    fn is_root(&self) -> bool {
        self.outer.is_none()
    }

    fn declare_var(&mut self, name: String, value: JsValue) {
        self.bindings.entry(name).or_insert(value);
    }

    fn set_var(&mut self, name: String, value: JsValue) -> bool {
        if self.bindings.contains_key(&name) {
            self.bindings.insert(name, value);
            return true;
        }
        if let Some(outer) = &self.outer {
            return outer.borrow_mut().set_var(name, value);
        }
        return false;
    }

    fn get_var(&self, name: &str) -> Option<JsValue> {
        if self.bindings.contains_key(name) {
            return self.bindings.get(name).cloned();
        }
        if let Some(outer) = &self.outer {
            return outer.borrow().get_var(name);
        }
        None
    }
}

pub struct Vm {
    pub heap: Heap,

    stack: Vec<JsValue>,
    frames: Vec<CallFrame>,
    global_obj: Gc<JsObject>,
}

impl Vm {
    pub fn new() -> Self {
        let mut heap = Heap::new();
        let mut global_obj = JsObject::new();

        js_std::new_std(&mut heap)
            .into_iter()
            .for_each(|(name, value)| {
                global_obj.properties.insert(
                    name,
                    PropertyDescriptor {
                        value,
                        writable: true,
                        enumerable: false,
                        configurable: true,
                    },
                );
            });

        let global_obj_ref = heap.alloc_object(global_obj);
        let global_obj = heap.get_object_mut(&global_obj_ref);

        global_obj.set(
            "globalThis".to_string(),
            JsValue::Object(global_obj_ref.clone()),
        );
        global_obj.set(
            "window".to_string(),
            JsValue::Object(global_obj_ref.clone()),
        );
        global_obj.properties.insert(
            "undefined".to_string(),
            PropertyDescriptor {
                value: JsValue::Undefined,
                enumerable: false,
                writable: false,
                configurable: false,
            },
        );

        Self {
            stack: Vec::new(),
            frames: Vec::new(),
            heap,
            global_obj: global_obj_ref,
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
        let env = match self.frames.last() {
            Some(frame) => frame.env.clone(),
            None => Rc::new(RefCell::new(Env::new(None))),
        };

        self.stack.clear();
        self.frames.clear();

        let mut frame = CallFrame::new(
            Rc::new(CodeBlock {
                code: bytecode.to_vec(),
                constants: ConstantTable::new(constants.clone()),
            }),
            env,
        );
        frame.this_value = JsValue::Object(self.global_obj.clone());
        self.frames.push(frame);

        self.run_loop()
    }

    fn run_loop(&mut self) -> Result<(), RuntimeError> {
        while let Some(frame) = self.frames.last_mut() {
            if frame.ip >= frame.code.code.len() {
                // No explicit return
                self.stack.push(JsValue::Undefined);
                self.end_current_frame();
                continue;
            }

            let op = frame.code.code[frame.ip].clone();
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
                OpCode::PushThis => {
                    let this_value = frame.this_value.clone();
                    self.stack.push(this_value);
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
                OpCode::LogicalNot => {
                    let value = self.stack.pop().expect("stack underflow in LogicalNot");
                    let result = !value.to_bool();
                    self.stack.push(JsValue::Boolean(result));
                }
                OpCode::Dup => {
                    let value = self.stack.last().expect("stack underflow in Dup");
                    self.stack.push(value.clone());
                }
                OpCode::Pop => {
                    self.stack.pop().expect("stack underflow in Pop");
                }
                OpCode::Halt => {
                    break;
                }
                OpCode::PushConstant(index) => {
                    let value = frame.code.constants.get(index);
                    self.stack.push(value);
                }
                OpCode::DeclareVar(const_idx) => {
                    let name = frame.code.constants.get_string(const_idx);
                    if frame.env.borrow().is_root() {
                        self.heap
                            .get_object_mut(&self.global_obj)
                            .properties
                            .entry(name.clone())
                            .or_insert(PropertyDescriptor {
                                value: JsValue::Undefined,
                                configurable: true,
                                writable: true,
                                enumerable: true,
                            });
                    } else {
                        frame
                            .env
                            .borrow_mut()
                            .declare_var(name.clone(), JsValue::Undefined);
                    }
                }
                OpCode::SetVar(const_idx) => {
                    let name = frame.code.constants.get_string(const_idx);
                    let value = self.stack.last().unwrap();
                    if frame.env.borrow().is_root() {
                        self.heap
                            .get_object_mut(&self.global_obj)
                            .set(name.clone(), value.clone());
                    } else {
                        frame.env.borrow_mut().set_var(name.clone(), value.clone());
                    }
                }
                OpCode::GetVar(const_idx) => {
                    let name = frame.code.constants.get_string(const_idx);
                    let mut value = frame.env.borrow().get_var(name);
                    if value.is_none() {
                        value = self.heap.get_object(&self.global_obj).get(&self.heap, name);
                    }
                    let value = value.ok_or(RuntimeError::ReferenceError(name.clone()))?;
                    self.stack.push(value);
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
                    let name = frame.code.constants.get_string(name_index);
                    let global_obj = self.heap.get_object(&self.global_obj);
                    let value = global_obj
                        .get(&self.heap, name)
                        .ok_or(RuntimeError::ReferenceError(name.clone()))?;
                    self.stack.push(value);
                }
                OpCode::SetGlobal(name_index) => {
                    let value = self.stack.last().unwrap();
                    let name = frame.code.constants.get_string(name_index);
                    let global_obj = self.heap.get_object_mut(&self.global_obj);
                    global_obj.set(name.clone(), value.clone());
                }
                OpCode::Call(argc) => self.handle_op_call(argc)?,
                OpCode::Construct(argc) => self.handle_op_construct(argc)?,

                OpCode::Return => {
                    self.end_current_frame();
                }
                OpCode::Jump(addr) => {
                    frame.ip = addr;
                }
                OpCode::JumpIfTrue(addr) => {
                    let value = self.stack.pop().unwrap();
                    if value.to_bool() {
                        frame.ip = addr;
                    }
                }
                OpCode::JumpIfFalse(addr) => {
                    let value = self.stack.pop().unwrap();
                    if !value.to_bool() {
                        frame.ip = addr;
                    }
                }
                OpCode::NewObject => {
                    let obj = JsObject::new();
                    let obj = self.heap.alloc_object(obj);
                    self.stack.push(JsValue::Object(obj));
                }
                OpCode::InitProperty(const_id) => {
                    let value = self.stack.pop().unwrap();
                    let obj = self.stack.pop().unwrap();
                    let key = frame.code.constants.get_string(const_id);
                    let obj_ref = match obj {
                        JsValue::Object(obj) => obj,
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };
                    let obj = self.heap.get_object_mut(&obj_ref);
                    obj.set(key.to_string(), value);
                    self.stack.push(JsValue::Object(obj_ref));
                }
                OpCode::SetProperty(key_index) => {
                    let value = self.stack.pop().unwrap();
                    let obj = self.stack.pop().unwrap();
                    let key = frame.code.constants.get_string(key_index);

                    let obj = match obj {
                        JsValue::Object(obj) => self.heap.get_object_mut(&obj),
                        JsValue::Function(func) => {
                            let func = self.heap.get_func_mut(&func);
                            &mut func.object
                        }
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    obj.set(key.clone(), value);
                }
                OpCode::GetProperty(key_index) => {
                    let obj = match self.stack.pop().unwrap() {
                        JsValue::Object(obj) => self.heap.get_object(&obj),
                        JsValue::Function(func) => {
                            let func = self.heap.get_func(&func);
                            &func.object
                        }
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    let key = frame.code.constants.get_string(key_index);

                    self.stack
                        .push(obj.get(&self.heap, key).unwrap_or(JsValue::Undefined));
                }
                OpCode::GetElement => {
                    let key_val = self.stack.pop().unwrap();
                    let obj = match self.stack.pop().unwrap() {
                        JsValue::Object(obj) => self.heap.get_object(&obj),
                        JsValue::Function(func) => {
                            let func = self.heap.get_func(&func);
                            &func.object
                        }
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    let key = match &key_val {
                        JsValue::String(s) => s.clone(),
                        JsValue::Number(n) => n.to_string(),
                        _ => unimplemented!(),
                    };

                    self.stack
                        .push(obj.get(&self.heap, &key).unwrap_or(JsValue::Undefined));
                }
                OpCode::SetElement => {
                    let value = self.stack.pop().unwrap();
                    let key_val = self.stack.pop().unwrap();
                    let obj = match self.stack.pop().unwrap() {
                        JsValue::Object(obj) => self.heap.get_object_mut(&obj),
                        JsValue::Function(func) => {
                            let func = self.heap.get_func_mut(&func);
                            &mut func.object
                        }
                        _ => return Err(RuntimeError::TypeError("not an object".to_string())),
                    };

                    let key = match &key_val {
                        JsValue::String(s) => s.clone(),
                        JsValue::Number(n) => n.to_string(),
                        _ => unimplemented!(),
                    };

                    obj.set(key, value);
                }
                OpCode::NewFunc(index) => {
                    let tmpl = frame.code.constants.get_func_tmpl(index);
                    let mut func = JsFunction::new(
                        tmpl.name.clone(),
                        tmpl.params.clone(),
                        FunctionBody::Script(tmpl.code_block.clone()),
                    );
                    func.object.set(
                        "prototype".to_string(),
                        JsValue::Object(self.heap.alloc_object(JsObject::new())),
                    );
                    func.env = Some(frame.env.clone());
                    let func = self.heap.alloc_func(func);
                    let value = JsValue::Function(func);
                    self.stack.push(value);
                }
                OpCode::InstanceOf => {
                    let constructor = self.stack.pop().unwrap();
                    let obj = self.stack.pop().unwrap();

                    let result = match (obj, constructor) {
                        (JsValue::Object(obj), JsValue::Function(constructor)) => {
                            runtime::is_instance_of(&self.heap, &obj, &constructor)
                        }
                        _ => false,
                    };
                    self.stack.push(JsValue::Boolean(result));
                }
            }
        }

        Ok(())
    }

    fn handle_op_call(&mut self, argc: usize) -> Result<(), RuntimeError> {
        // Stack: [func, this, args...]
        let base = self.stack.len() - argc - 2;
        let args = self.stack.split_off(base + 2);
        let this = self.stack.pop().unwrap();
        let func = match self.stack.pop().unwrap() {
            JsValue::Function(func) => func,
            _ => return Err(RuntimeError::TypeError("not a function".to_string())),
        };
        self.handle_function_call(base, this, func, args)?;
        Ok(())
    }

    fn handle_op_construct(&mut self, argc: usize) -> Result<(), RuntimeError> {
        // Stack: [constructor, args...]
        let base = self.stack.len() - argc - 1;
        let args = self.stack.split_off(base + 1);
        let constructor = match self.stack.pop().unwrap() {
            JsValue::Function(func) => func,
            _ => return Err(RuntimeError::TypeError("not a function".to_string())),
        };
        let this = {
            let mut this = JsObject::new();
            let constructor = self.heap.get_func(&constructor);
            let constructor_proto = constructor.object.get(&self.heap, "prototype");
            if let Some(JsValue::Object(proto)) = constructor_proto {
                this.prototype = Some(proto.clone());
            }
            JsValue::Object(self.heap.alloc_object(this))
        };
        let frame_len = self.frames.len();
        self.handle_function_call(base, this, constructor, args)?;
        if frame_len != self.frames.len() {
            // New frame pushed
            self.frames.last_mut().unwrap().is_constructor = true;
        }
        Ok(())
    }

    fn handle_function_call(
        &mut self,
        base: usize,
        this: JsValue,
        func: Gc<JsFunction>,
        args: Vec<JsValue>,
    ) -> Result<(), RuntimeError> {
        let func = self.heap.get_func(&func);
        match func.body.clone() {
            FunctionBody::Script(code) => {
                let env = Rc::new(RefCell::new(Env::new(func.env.as_ref().cloned())));

                for (i, param) in func.params.iter().enumerate() {
                    if i < args.len() {
                        env.borrow_mut().declare_var(param.clone(), args[i].clone());
                    } else {
                        env.borrow_mut()
                            .declare_var(param.clone(), JsValue::Undefined);
                    }
                }

                self.frames.push(CallFrame {
                    base,
                    ip: 0,
                    code: code.clone(),
                    env,
                    this_value: this,
                    is_constructor: false,
                })
            }
            FunctionBody::Native(native_fn) => {
                let mut args = Vec::from(args);
                if args.len() < func.params.len() {
                    args.resize(func.params.len(), JsValue::Undefined);
                }

                let mut ctx = NativeFnCtx {
                    heap: &mut self.heap,
                    args: &args,
                    this_value: &this,
                    return_value: None,
                };

                native_fn(&mut ctx);

                self.stack
                    .push(ctx.return_value.unwrap_or(JsValue::Undefined));
            }
        }

        Ok(())
    }

    fn end_current_frame(&mut self) {
        let frame = self.frames.pop().unwrap();
        let return_value = self.stack.pop().unwrap();
        self.stack.truncate(frame.base);

        if frame.is_constructor && !runtime::is_object(&return_value) {
            self.stack.push(frame.this_value);
            return;
        }

        self.stack.push(return_value);
    }

    pub fn pop(&mut self) -> Option<JsValue> {
        self.stack.pop()
    }
}
