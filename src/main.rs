mod repl;

use std::env;
use std::fs;
use std::io::{self, IsTerminal, Read};

use slowjs::compiler::Compiler;
use slowjs::lexer::Lexer;
use slowjs::parser::Parser;
use slowjs::vm::Vm;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        let script_path = &args[1];
        let input = fs::read_to_string(script_path)
            .expect(&format!("Failed to read file: {}", script_path));

        run_script(&input);
        return;
    }

    if io::stdin().is_terminal() {
        repl::run();
    } else {
        let mut input = String::new();
        io::stdin()
            .read_to_string(&mut input)
            .expect("Failed to read stdin");

        run_script(&input);
    }
}

fn run_script(input: &str) {
    let mut lexer = Lexer::new();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new();

    let tokens = match lexer.tokenize(input) {
        Ok(tokens) => tokens,
        Err(err) => {
            panic!("{}", err);
        }
    };

    let program = Parser::new(tokens)
        .parse_program()
        .expect("Failed to parse input");

    let result = compiler
        .compile(&program)
        .expect("Failed to compile program");

    vm.run_script(&result.bytecode, &result.constants, &result.exception_table)
        .expect("Failed to run script");
}
