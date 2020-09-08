use lexer::Lexer;
use std::io;
use token::{Token, TokenType};

mod lexer;
mod token;

fn _main() {
    println!("starting monkey rust!!");
    loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");
        let mut lexer = Lexer::new(input);
        loop {
            let token = lexer.next_token();
            dbg!(&token);
            if token.token_type == TokenType::Illigal || token.token_type == TokenType::Eof {
                break;
            }
        }
    }
}
fn main() {
    _main()
}
