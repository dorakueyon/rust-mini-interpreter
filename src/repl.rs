use super::{Environment, Eval, Lexer, Parser};
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
          let program = p.parse_program();

          if p.errors.len() != 0 {
            Repl::print_parse_errors(p.errors);
            continue;
          }
          if let Some(evaluated) = program.eval(&mut env) {
            println!("{}", evaluated.inspect());
            io::stdout().flush().unwrap();
          }
        }
        Err(_) => break,
      }
    }
  }
  fn print_parse_errors(errors: Vec<String>) {
    for pe in errors.iter() {
      println!("{}", pe)
    }
  }
}
