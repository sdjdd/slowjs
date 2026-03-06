use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use rustyline::{
    Cmd, ConditionalEventHandler, DefaultEditor, Event, EventHandler, KeyCode, KeyEvent, Modifiers,
    error::ReadlineError,
};

use slowjs::compiler::Compiler;
use slowjs::js_std::console;
use slowjs::lexer::{Lexer, TokenKind};
use slowjs::parser::{ParseError, Parser};
use slowjs::runtime::JsValue;
use slowjs::vm::Vm;

enum ReplError {
    ImcompleteInput,
    Other(String),
}

struct InterruptHandler(Arc<AtomicBool>);

impl ConditionalEventHandler for InterruptHandler {
    fn handle(
        &self,
        _evt: &Event,
        _n: rustyline::RepeatCount,
        _positive: bool,
        ctx: &rustyline::EventContext,
    ) -> Option<Cmd> {
        self.0.store(ctx.line().is_empty(), Ordering::Relaxed);
        None
    }
}

pub fn run() {
    println!("Welcome to SlowJS.");

    let mut rl = DefaultEditor::new().expect("Failed to create REPL");
    let mut vm = Vm::new();
    let mut lexer = Lexer::new();
    let mut compiler = Compiler::new();
    let mut input_buffer = String::new();

    let mut interrupt_count = 0;
    let empty_interrupt = Arc::new(AtomicBool::new(false));
    let handle_interrupt = InterruptHandler(empty_interrupt.clone());

    rl.bind_sequence(
        Event::KeySeq(vec![KeyEvent(KeyCode::Char('c'), Modifiers::CTRL)]),
        EventHandler::Conditional(Box::new(handle_interrupt)),
    );

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

                if input == ".exit" {
                    break;
                }
                if input.is_empty() && input_buffer.is_empty() {
                    continue;
                }

                input_buffer.push_str(input);

                match process_input(&input_buffer, &mut lexer, &mut compiler, &mut vm) {
                    Ok(value) => match value {
                        JsValue::String(s) => {
                            println!("'{s}'");
                        }
                        _ => {
                            console::print(&vm.heap, &value);
                            println!();
                        }
                    },
                    Err(ReplError::ImcompleteInput) => {
                        input_buffer.push('\n');
                        continue;
                    }
                    Err(ReplError::Other(e)) => {
                        println!("Error: {e}");
                    }
                }

                input_buffer.clear();
                interrupt_count = 0;
            }
            Err(ReadlineError::Interrupted) => {
                input_buffer.clear();
                if empty_interrupt.load(Ordering::Relaxed) {
                    interrupt_count += 1;
                    if interrupt_count > 1 {
                        break;
                    }
                    println!("(To exit, press Ctrl+C again or Ctrl+D or type .exit)");
                } else {
                    interrupt_count = 0;
                }
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

fn map_parse_error(e: ParseError) -> ReplError {
    match e {
        ParseError::UnexpectedToken {
            found: TokenKind::Eof,
            ..
        } => ReplError::ImcompleteInput,
        e => ReplError::Other(e.to_string()),
    }
}

fn process_input(
    input: &str,
    lexer: &mut Lexer,
    compiler: &mut Compiler,
    vm: &mut Vm,
) -> Result<JsValue, ReplError> {
    let tokens = lexer
        .tokenize(input)
        .map_err(|e| ReplError::Other(e.to_string()))?;

    if tokens.is_empty() {
        return Ok(JsValue::Undefined);
    }

    let first_token = tokens[0].kind.clone();

    let mut parser = Parser::new(tokens);

    compiler.reset();

    let result = {
        if matches!(first_token, TokenKind::Var | TokenKind::Function) {
            let program = parser.parse_program().map_err(map_parse_error)?;
            compiler.compile(&program).unwrap()
        } else {
            // Try to parse expression
            if let Ok(expr) = parser.parse_expression()
                && parser.is_complete()
            {
                compiler.compile_expression(&expr).unwrap();
                compiler.get_result()
            } else {
                // Fallback to statements
                parser.reset();
                let program = parser.parse_program().map_err(map_parse_error)?;
                compiler.compile(&program).unwrap()
            }
        }
    };

    vm.run_script(&result.bytecode, &result.constants)
        .map_err(|e| ReplError::Other(e.to_string()))?;

    Ok(vm.pop().unwrap_or(JsValue::Undefined))
}
