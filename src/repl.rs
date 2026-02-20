use crate::eval::{Context, eval_program};
use crate::lexer::tokenize;
use crate::parser::parse;
use rustyline::{DefaultEditor, error::ReadlineError};

pub fn run() {
    println!("Welcome to SlowJS");

    let mut context = Context::new();

    let mut rl = DefaultEditor::new().expect("Failed to create editor");

    loop {
        match rl.readline("> ") {
            Ok(input) => {
                let _ = rl.add_history_entry(input.as_str());

                let input = input.trim();

                if input.is_empty() {
                    continue;
                }

                match process_input(input, &mut context) {
                    Ok(Some(value)) => println!("{value}"),
                    Ok(None) => {}
                    Err(e) => println!("Error: {}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
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

fn process_input(
    input: &str,
    context: &mut Context,
) -> Result<Option<crate::ast::Value>, Box<dyn std::error::Error>> {
    let tokens = tokenize(input)?;

    let program = parse(tokens)?;

    let result = eval_program(&program, context)?;

    Ok(result)
}
