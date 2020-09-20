use ast::{BlockStatement, Expression, Identifier, Program, Statement};
use errors::Errors;
use evaluator::Eval;
use lexer::Lexer;
use object::{Environment, HashPair, Object};
use parser::Parser;
use repl::Repl;
use token::{Token, TokenType};

mod ast;
mod builtins;
mod errors;
mod evaluator;
mod lexer;
mod object;
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
