use ast::{Expression, Identifier, Program, Statement};
use lexer::Lexer;
use repl::Repl;
use token::{Token, TokenType};

mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn _main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    Repl::start();
}
fn main() {
    _main()
}
