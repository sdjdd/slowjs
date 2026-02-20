mod ast;
mod eval;
mod features;
mod lexer;
mod parser;
mod repl;

fn main() {
    repl::run();
}
