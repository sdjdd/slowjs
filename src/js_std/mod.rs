pub mod console;
pub mod object;

use crate::{runtime::JsValue, vm::Heap};

pub fn new_std(heap: &mut Heap) -> Vec<(String, JsValue)> {
    vec![("console".to_string(), console::new_console_object(heap))]
}
