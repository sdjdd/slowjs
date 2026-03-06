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
    pub object: JsObject,

    pub name: String,
    pub params: Vec<String>,
    pub body: FunctionBody,

    /// The environment in which the function was defined
    pub env: Option<Rc<RefCell<Env>>>,

    pub prototype: Option<Gc<JsObject>>,
}

impl JsFunction {
    pub fn new(name: String, params: Vec<String>, body: FunctionBody) -> Self {
        let mut object = JsObject::new();
        object.set("name".to_string(), JsValue::String(name.clone()));
        object.set("length".to_string(), JsValue::Number(params.len() as f64));
        Self {
            object,
            name,
            params,
            body,
            env: None,
            prototype: None,
        }
    }
}

pub type NativeFn = fn(&mut NativeFnCtx);

#[derive(Clone)]
pub enum FunctionBody {
    Script(Rc<CodeBlock>),
    Native(NativeFn),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub heap: &'a mut Heap,
    pub args: &'a [JsValue],
    pub this_value: &'a JsValue,
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

    pub fn with_prototype(prototype: Option<Gc<JsObject>>) -> Self {
        let mut obj = Self::new();
        obj.prototype = prototype;
        obj
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

    pub fn get(&self, heap: &Heap, k: &str) -> Option<JsValue> {
        if let Some(desc) = self.properties.get(k) {
            return Some(desc.value.clone());
        }
        if let Some(proto) = &self.prototype {
            return heap.get_object(proto).get(heap, k);
        }
        None
    }
}

pub fn string_to_number(s: &str) -> f64 {
    let s = s.trim();

    if s.is_empty() {
        return 0.0;
    }

    s.parse::<f64>().unwrap_or(f64::NAN)
}

pub fn to_number(value: &JsValue) -> f64 {
    match value {
        JsValue::Number(n) => *n,
        JsValue::Undefined => f64::NAN,
        JsValue::Null => 0.0,
        JsValue::Boolean(b) => {
            if *b {
                1.0
            } else {
                0.0
            }
        }
        JsValue::String(s) => {
            if s.is_empty() {
                0.0
            } else {
                s.parse::<f64>().unwrap_or(f64::NAN)
            }
        }
        _ => unimplemented!("to_number"),
    }
}

pub fn is_object(value: &JsValue) -> bool {
    match value {
        JsValue::Null
        | JsValue::Undefined
        | JsValue::Boolean(_)
        | JsValue::Number(_)
        | JsValue::String(_) => false,
        _ => true,
    }
}

pub fn to_object<'a>(heap: &'a Heap, value: &JsValue) -> &'a JsObject {
    match value {
        JsValue::Object(o) => heap.get_object(o),
        JsValue::Function(f) => &heap.get_func(f).object,
        _ => unimplemented!(),
    }
}

pub fn is_instance_of(heap: &Heap, obj: &Gc<JsObject>, constructor: &Gc<JsFunction>) -> bool {
    let constructor = heap.get_func(constructor);
    let prototype = match constructor.object.get(heap, "prototype") {
        Some(JsValue::Object(prototype)) => prototype,
        _ => return false,
    };
    let mut current = obj.clone();
    loop {
        let obj = heap.get_object(&current);
        match &obj.prototype {
            Some(proto) => {
                if proto.index == prototype.index {
                    return true;
                }
                current = proto.clone();
            }
            None => return false,
        }
    }
}
