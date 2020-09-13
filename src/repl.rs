use super::{Eval, Lexer, ParseError, Parser, TokenType};
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
          let mut p = Parser::new(l);
          let program = p.parse_program();

          if p.errors.len() != 0 {
            Repl::print_parse_errors(p.errors);
            continue;
          }
          if let Some(evaluated) = program.eval() {
            println!("{}", evaluated.inspect());
            io::stdout().flush().unwrap();
          }
          //println!("{}", program);
          //io::stdout().flush().unwrap();
        }
        Err(_) => break,
      }
    }
  }
  fn print_parse_errors(errors: Vec<ParseError>) {
    for pe in errors.iter() {
      println!("{}", pe)
    }
  }
}
