use crate::{
    runtime::{FunctionBody, JsFunction, JsObject, JsValue, NativeFn, PropertyDescriptor},
    vm::Heap,
};

pub fn add_member_func(heap: &mut Heap, target: &mut JsObject, name: &str, func: NativeFn) {
    target.properties.insert(
        name.to_string(),
        PropertyDescriptor {
            value: JsValue::Function(heap.alloc_func(JsFunction::new(
                name.to_string(),
                vec![],
                FunctionBody::Native(func),
            ))),
            writable: true,
            enumerable: false,
            configurable: true,
        },
    );
}
