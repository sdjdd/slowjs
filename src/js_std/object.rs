use std::collections::HashMap;

use crate::{
    js_std::util::add_member_func,
    runtime::{self, FunctionBody, JsFunction, JsObject, JsValue, NativeFnCtx},
    vm::Heap,
};

pub fn new_object_constructor(heap: &mut Heap) -> JsValue {
    let mut object_constructor = JsFunction::new(
        "Object".to_string(),
        vec![],
        FunctionBody::Native(object_constructor),
    );

    add_member_func(
        heap,
        &mut object_constructor.object,
        "assign",
        object_assign,
    );

    JsValue::Function(heap.alloc_func(object_constructor))
}

fn object_constructor(ctx: &mut NativeFnCtx) {
    let value = ctx.args.get(0).unwrap_or(&JsValue::Undefined);
    let return_value = match value {
        JsValue::Undefined | JsValue::Null => {
            JsValue::Object(ctx.heap.alloc_object(JsObject::new()))
        }
        JsValue::Object(o) => JsValue::Object(o.clone()),
        JsValue::Function(f) => JsValue::Function(f.clone()),
        _ => unimplemented!(),
    };
    ctx.return_value = Some(return_value);
}

fn object_assign(ctx: &mut NativeFnCtx) {
    let mut temp = HashMap::new();
    for source in ctx.args.iter().skip(1) {
        if runtime::is_object(source) {
            let source = runtime::to_object(ctx.heap, source);
            for (k, desc) in source.properties.iter() {
                if desc.enumerable {
                    temp.insert(k.clone(), desc.clone());
                }
            }
        }
    }

    let target: &mut JsObject = match ctx.args.get(0) {
        Some(v) => match v {
            JsValue::Object(o) => ctx.heap.get_object_mut(o),
            JsValue::Function(f) => &mut ctx.heap.get_func_mut(f).object,
            _ => {
                let gc = ctx.heap.alloc_object(JsObject::new());
                ctx.heap.get_object_mut(&gc)
            }
        },
        None => {
            // TODO: throw TypeError
            unimplemented!()
        }
    };

    target.properties.extend(temp);
}
