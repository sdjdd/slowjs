use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::vm::{Constant, Env, FunctionTemplate, Heap, OpCode};

#[derive(Clone, Copy)]
pub struct Gc<T> {
    pub index: usize,
    _marker: std::marker::PhantomData<T>,
}

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

impl std::fmt::Debug for JsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsValue::Null => write!(f, "null"),
            JsValue::Undefined => write!(f, "undefined"),
            JsValue::Boolean(b) => write!(f, "{}", b),
            JsValue::Number(n) => write!(f, "{}", n),
            JsValue::String(s) => write!(f, "'{}'", s),
            JsValue::Object(_) => write!(f, "object"),
            JsValue::Function(_) => write!(f, "function"),
        }
    }
}

#[derive(Clone)]
pub struct JsObject {
    pub prototype: Option<Gc<JsObject>>,
    pub properties: HashMap<String, PropertyDescriptor>,
}

#[derive(Clone)]
pub struct PropertyDescriptor {
    pub value: JsValue,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

#[derive(Clone)]
pub struct JsFunction {
    pub prototype: Gc<JsObject>,
    pub name: String,
    pub params: Vec<String>,
    pub body: FunctionBody,
    pub env: Option<Rc<RefCell<Env>>>,
}

#[derive(Clone)]
pub enum FunctionBody {
    Script(Rc<CodeBlock>),
    Native(fn(&NativeFnCtx)),
}

#[derive(Clone, PartialEq, Eq)]
pub struct ConstantTable {
    consts: Vec<Constant>,
}

impl ConstantTable {
    pub fn new(consts: Vec<Constant>) -> Self {
        Self { consts }
    }

    pub fn get(&self, index: usize) -> JsValue {
        match self.consts[index] {
            Constant::Number(n) => JsValue::Number(f64::from_bits(n)),
            Constant::String(ref s) => JsValue::String(s.clone()),
            Constant::Function(_) => panic!("constant type mismatch"),
        }
    }

    pub fn get_string(&self, index: usize) -> &String {
        match self.consts[index] {
            Constant::String(ref s) => s,
            _ => panic!("constant type mismatch"),
        }
    }

    pub fn get_func_tmpl(&self, index: usize) -> &FunctionTemplate {
        match self.consts[index] {
            Constant::Function(ref tmpl) => tmpl,
            _ => panic!("constant type mismatch"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct CodeBlock {
    pub code: Vec<OpCode>,
    pub constants: ConstantTable,
}

pub struct NativeFnCtx<'a> {
    pub heap: &'a Heap,
    pub args: &'a [JsValue],
    pub return_value: Option<JsValue>,
}

impl<T> Gc<T> {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            _marker: std::marker::PhantomData,
        }
    }
}

impl Default for JsValue {
    fn default() -> Self {
        Self::Undefined
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

impl Default for PropertyDescriptor {
    fn default() -> Self {
        Self {
            value: JsValue::Undefined,
            writable: false,
            enumerable: false,
            configurable: false,
        }
    }
}

impl JsObject {
    pub fn new() -> Self {
        Self {
            properties: HashMap::new(),
            prototype: None,
        }
    }

    pub fn set(&mut self, k: String, value: JsValue) {
        self.properties.insert(
            k,
            PropertyDescriptor {
                value,
                writable: true,
                enumerable: true,
                configurable: true,
            },
        );
    }

    pub fn get(&self, k: &str) -> JsValue {
        self.properties
            .get(k)
            .cloned()
            .map(|attrs| attrs.value)
            .unwrap_or(JsValue::Undefined)
    }
}
