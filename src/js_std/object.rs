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
    let value = ctx.args.first().unwrap_or(&JsValue::Undefined);
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

    match ctx.args.first() {
        Some(v) => match v {
            JsValue::Object(o) => {
                ctx.heap.get_object_mut(o).properties.extend(temp);
                ctx.return_value = Some(JsValue::Object(o.clone()));
            }
            JsValue::Function(f) => {
                ctx.heap.get_func_mut(f).object.properties.extend(temp);
                ctx.return_value = Some(JsValue::Function(f.clone()));
            }
            _ => {
                let mut obj = JsObject::new();
                obj.properties.extend(temp);
                ctx.return_value = Some(JsValue::Object(ctx.heap.alloc_object(obj)));
            }
        },
        None => {
            // TODO: throw TypeError
            unimplemented!()
        }
    };
}
