use super::{Lexer, TokenType};
use std::io;
use std::io::Write;

pub struct Repl;

const PROMPT: &str = ">>";

impl Repl {
  pub fn start() {
    loop {
      print!("{} ", PROMPT);
      io::stdout().flush().unwrap();
      let mut input = String::new();
      match io::stdin().read_line(&mut input) {
        Ok(_) => {
          let mut l = Lexer::new(input);
          loop {
            let tok = l.next_token();
            println!("{:?}", &tok);
            if tok.token_type == TokenType::Eof {
              break;
            }
          }
        }
        Err(_) => break,
      }
    }
  }
}
