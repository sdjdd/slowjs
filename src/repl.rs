use rustyline::{DefaultEditor, error::ReadlineError};
use slowjs::compiler::Compiler;
use slowjs::lexer::{TokenKind, tokenize};
use slowjs::parser::{ParseError, parse};
use slowjs::vm::{JsValue, Vm};

enum ReplError {
    ImcompleteInput,
    Other(String),
}

pub fn run() {
    println!("Welcome to SlowJS.");
    println!("Press Ctrl-D to exit.");

    let mut rl = DefaultEditor::new().expect("Failed to create editor");
    let mut vm = Vm::new();
    let mut compiler = Compiler::new();
    let mut input_buffer = String::new();

    loop {
        let prompt = if input_buffer.is_empty() {
            "> "
        } else {
            "... "
        };

        match rl.readline(prompt) {
            Ok(input) => {
                let _ = rl.add_history_entry(input.as_str());

                let input = input.trim();

                if input.is_empty() && input_buffer.is_empty() {
                    continue;
                }

                input_buffer.push_str(input);

                match process_input(&input_buffer, &mut compiler, &mut vm) {
                    Ok(value) => println!("{value}"),
                    Err(ReplError::ImcompleteInput) => {
                        continue;
                    }
                    Err(ReplError::Other(e)) => {
                        println!("Error: {e}");
                    }
                }

                input_buffer.clear();
            }
            Err(ReadlineError::Interrupted) => {
                input_buffer.clear();
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn process_input(input: &str, compiler: &mut Compiler, vm: &mut Vm) -> Result<JsValue, ReplError> {
    let tokens = tokenize(input).map_err(|e| ReplError::Other(e.to_string()))?;

    let program = parse(tokens).map_err(|e| match e {
        ParseError::UnexpectedToken {
            found: TokenKind::Eof,
            ..
        } => ReplError::ImcompleteInput,
        e => ReplError::Other(e.to_string()),
    })?;

    let result = compiler.compile(&program).unwrap();

    vm.run(&result.bytecode, &result.constants)
        .map_err(|e| ReplError::Other(e.to_string()))?;

    Ok(vm.value().map(|v| v.clone()).unwrap_or(JsValue::Undefined))
}
