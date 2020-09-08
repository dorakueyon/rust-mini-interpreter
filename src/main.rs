use lexer::Lexer;
use repl::Repl;
use token::{Token, TokenType};

mod lexer;
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
