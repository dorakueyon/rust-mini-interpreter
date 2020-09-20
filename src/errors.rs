use std::fmt::{Debug, Display, Formatter, Result as FormatResult};

#[derive(Debug)]
pub enum Errors {
  ParseError,
}

impl Display for Errors {
  fn fmt(&self, f: &mut Formatter<'_>) -> FormatResult {
    match self {
      Errors::ParseError => write!(f, "parse error"),
    }
  }
}
