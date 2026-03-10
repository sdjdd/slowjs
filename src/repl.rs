use colored::Colorize;

use slowjs::compiler::Compiler;
use slowjs::js_std::console;
use slowjs::lexer::{Lexer, TokenKind};
use slowjs::parser::{ParseError, Parser};
use slowjs::runtime::JsValue;
use slowjs::vm::Vm;

mod editor;

use crate::repl::editor::{Editor, EditorContext, EditorError, EditorEventHandler};

enum ReplError {
    ImcompleteInput,
    Other(String),
}

struct Repl {
    compiler: Compiler,
    vm: Vm,
    input_buffer: String,
    ctrl_c_count: usize,
}

impl Repl {
    fn new() -> Self {
        Self {
            compiler: Compiler::new(),
            vm: Vm::new(),
            input_buffer: String::new(),
            ctrl_c_count: 0,
        }
    }

    fn process_input(&mut self) -> Result<JsValue, ReplError> {
        let tokens = Lexer::new()
            .tokenize(&self.input_buffer)
            .map_err(|e| ReplError::Other(e.to_string()))?;

        if tokens.is_empty() {
            return Ok(JsValue::Undefined);
        }

        let first_token = tokens[0].kind.clone();

        let mut parser = Parser::new(tokens);

        self.compiler.reset();

        let result = {
            if matches!(first_token, TokenKind::Var | TokenKind::Function) {
                let program = parser.parse_program().map_err(map_parse_error)?;
                self.compiler.compile(&program).unwrap()
            } else {
                // Try to parse expression
                if let Ok(expr) = parser.parse_expression()
                    && parser.is_complete()
                {
                    self.compiler.compile_expression(&expr).unwrap();
                    self.compiler.get_result()
                } else {
                    // Fallback to statements
                    parser.reset();
                    let program = parser.parse_program().map_err(map_parse_error)?;
                    self.compiler.compile(&program).unwrap()
                }
            }
        };

        self.vm
            .run_script(&result.bytecode, &result.constants, &result.exception_table)
            .map_err(|e| ReplError::Other(e.to_string()))?;

        Ok(self.vm.pop().unwrap_or(JsValue::Undefined))
    }

    fn get_hint_candidates(&self, input: &str) -> Vec<String> {
        let search = input.trim_start();
        self.vm
            .get_global_idents()
            .iter()
            .filter(|id| id.starts_with(search))
            .map(|id| id[search.len()..].to_string())
            .collect()
    }

    fn get_completion_candidates(&self, input: &str) -> Vec<String> {
        let search = input.trim_start();
        self.vm
            .get_global_enumerable_idents()
            .iter()
            .filter(|id| id.starts_with(search))
            .map(|id| id[search.len()..].to_string())
            .collect()
    }
}

impl EditorEventHandler for Repl {
    fn get_prompt(&self) -> &str {
        if self.input_buffer.is_empty() {
            "> "
        } else {
            "... "
        }
    }

    fn handle_input(&mut self, line: String, ctx: &mut EditorContext) {
        self.ctrl_c_count = 0;

        let input = line.trim();

        if input == ".exit" {
            ctx.exit();
            return;
        }
        if input.is_empty() && self.input_buffer.is_empty() {
            return;
        }

        self.input_buffer.push_str(input);

        match self.process_input() {
            Ok(value) => {
                match value {
                    JsValue::String(s) => {
                        println!("{}", format!("'{s}'").green());
                    }
                    _ => {
                        console::print(&self.vm.heap, &value);
                        println!();
                    }
                };

                self.input_buffer.clear();
            }
            Err(ReplError::ImcompleteInput) => {
                self.input_buffer.push('\n');
            }
            Err(ReplError::Other(e)) => {
                println!("Error: {e}");
                self.input_buffer.clear();
            }
        }
    }

    fn handle_error(&mut self, err: editor::EditorError, ctx: &mut EditorContext) {
        match err {
            EditorError::Interrupt(line) => {
                self.input_buffer.clear();
                if line.is_empty() {
                    self.ctrl_c_count += 1;
                    if self.ctrl_c_count > 1 {
                        ctx.exit();
                    } else {
                        println!("(To exit, press Ctrl+C again or Ctrl+D or type .exit)");
                    }
                } else {
                    self.ctrl_c_count = 0;
                }
            }
            EditorError::Eof => {
                ctx.exit();
            }
            EditorError::Io(e) => {
                panic!("{}", e);
            }
        }
    }

    fn handle_hint(&mut self, input: String) -> Option<Vec<String>> {
        Some(self.get_hint_candidates(&input))
    }

    fn handle_completion(&mut self, input: String) -> Option<Vec<String>> {
        Some(self.get_completion_candidates(&input))
    }
}

pub fn run() {
    println!("Welcome to SlowJS.");

    let repl = Repl::new();
    let mut editor = Editor::new(Box::new(repl));

    editor.read_lines();
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
