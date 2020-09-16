use ast::{BlockStatement, Expression, Identifier, Program, Statement};
use evaluator::Eval;
use lexer::Lexer;
use object::{Environment, Object};
use parser::{ParseError, Parser};
use repl::Repl;
use token::{Token, TokenType};

mod ast;
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
