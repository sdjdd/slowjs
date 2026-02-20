mod ast;
mod eval;
mod lexer;
mod parser;
mod repl;

fn main() {
    repl::run();
}
