use super::{Environment, Errors, Eval, Lexer, Parser};
use std::io;
use std::io::Write;

pub struct Repl;

const PROMPT: &str = ">>";

impl Repl {
  pub fn start() {
    let mut env = Environment::new();
    loop {
      print!("{} ", PROMPT);
      io::stdout().flush().unwrap();
      let mut input = String::new();
      match io::stdin().read_line(&mut input) {
        Ok(_) => {
          let l = Lexer::new(input);
          let mut p = Parser::new(l);
          match p.parse_program() {
            Ok(p) => {
              if let Some(evaluated) = p.eval(&mut env) {
                println!("{}", evaluated.inspect());
                io::stdout().flush().unwrap();
              }
            }
            Err(e) => {
              Repl::print_parse_errors(e);
              continue;
            }
          }
        }
        Err(_) => break,
      }
    }
  }
  fn print_parse_errors(errors: Errors) {
    println!("{}", errors)
  }
}
