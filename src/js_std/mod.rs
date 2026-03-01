use std::{cell::RefCell, rc::Rc};

use crate::vm::{FunctionBody, JsFunction, JsObject, JsValue};

fn print(args: &[JsValue]) -> Option<JsValue> {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{}", arg);
    }
    println!();
    None
}

fn new_console_object() -> JsValue {
    let mut console = JsObject::new();
    console.set(
        "log".to_string(),
        JsValue::Function(Rc::new(RefCell::new(JsFunction::new(
            "log".to_string(),
            0,
            FunctionBody::Native(print),
        )))),
    );
    JsValue::Object(Rc::new(RefCell::new(console)))
}

pub fn new_std() -> Vec<(String, JsValue)> {
    vec![("console".to_string(), new_console_object())]
}
