use std::collections::{HashMap, HashSet};

use colored::Colorize;

use crate::{
    runtime::{FunctionBody, Gc, JsFunction, JsObject, JsValue, NativeFnCtx},
    vm::Heap,
};

struct ObjectRefCounter<'a> {
    visited: HashSet<usize>,
    ref_ids: HashMap<usize, usize>,
    next_ref_id: usize,
    heap: &'a Heap,
}

impl<'a> ObjectRefCounter<'a> {
    fn new(heap: &'a Heap) -> Self {
        Self {
            visited: HashSet::new(),
            ref_ids: HashMap::new(),
            next_ref_id: 1,
            heap,
        }
    }

    fn count(&mut self, obj: &Gc<JsObject>) {
        let idx = obj.index;
        if self.visited.contains(&idx) {
            self.ref_ids.entry(idx).or_insert_with(|| {
                let id = self.next_ref_id;
                self.next_ref_id += 1;
                id
            });
            return;
        }
        self.visited.insert(idx);
        let obj = self.heap.get_object(obj);
        for v in obj.properties.values() {
            if let JsValue::Object(o) = &v.value {
                self.count(o);
            }
        }
        self.visited.remove(&idx);
    }
}

fn print_function(f: &JsFunction) {
    print!("{}", format!("[Function: {}]", f.name).cyan())
}

fn print_object(heap: &Heap, obj: &Gc<JsObject>, depth: usize, counter: &mut ObjectRefCounter) {
    let ptr = obj.index;
    let obj = heap.get_object(obj);

    if obj.properties.is_empty() {
        print!("{{}}");
        return;
    }

    if let Some(id) = counter.ref_ids.get(&ptr) {
        print!("{}", format!("<ref *{}> ", id).cyan());
    }

    counter.visited.insert(ptr);

    println!("{{");
    for (k, v) in &obj.properties {
        if !v.enumerable {
            continue;
        }
        for _ in 0..depth {
            print!("  ");
        }
        print!("  {}: ", k);
        match &v.value {
            JsValue::Object(o) => {
                let ptr = o.index;
                if counter.visited.contains(&ptr) {
                    print!(
                        "{}",
                        format!("[Circular *{}]", counter.ref_ids.get(&ptr).unwrap()).cyan()
                    );
                } else {
                    print_object(heap, o, depth + 1, counter);
                }
            }
            JsValue::String(s) => print!("{}", format!("'{}'", s).green()),
            _ => print_with_depth(heap, &v.value, depth + 1),
        };
        println!(",");
    }
    for _ in 0..depth {
        print!("  ");
    }
    print!("}}");

    counter.visited.remove(&ptr);
}

fn print_with_depth(heap: &Heap, value: &JsValue, depth: usize) {
    match value {
        JsValue::Null => print!("{}", "null".white()),
        JsValue::Undefined => print!("{}", "undefined".black()),
        JsValue::Boolean(b) => print!("{}", b.to_string().yellow()),
        JsValue::Number(n) => {
            if n.is_infinite() {
                return if n.is_sign_positive() {
                    print!("{}", "Infinity".yellow());
                } else {
                    print!("{}", "-Infinity".yellow());
                };
            }
            if n.is_nan() {
                return print!("{}", "NaN".yellow());
            }
            if n.fract() == 0.0 {
                print!("{}", format!("{:.0}", n).yellow());
            } else {
                print!("{}", format!("{}", n).yellow());
            }
        }
        JsValue::String(s) => print!("{s}"),
        JsValue::Object(o) => {
            let mut counter = ObjectRefCounter::new(heap);
            counter.count(o);
            counter.visited.clear();
            print_object(heap, o, depth, &mut counter);
        }
        JsValue::Function(f) => {
            let func = heap.get_func(f);
            print_function(func);
        }
    }
}

fn console_log(ctx: &mut NativeFnCtx) {
    for (i, arg) in ctx.args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print(ctx.heap, arg);
    }
    println!();
}

pub fn print(heap: &Heap, value: &JsValue) {
    print_with_depth(heap, value, 0);
}

pub fn new_console_object(heap: &mut Heap) -> JsValue {
    let mut console = JsObject::new();
    let log = JsFunction::new("log".to_string(), vec![], FunctionBody::Native(console_log));
    console.set("log".to_string(), JsValue::Function(heap.alloc_func(log)));
    JsValue::Object(heap.alloc_object(console))
}
