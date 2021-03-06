use std::fmt::Debug;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Token {
  pub token_type: TokenType,
  pub literal: String,
}

impl Token {
  pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
      "fn" => TokenType::Function,
      "let" => TokenType::Let,
      "true" => TokenType::True,
      "false" => TokenType::False,
      "if" => TokenType::If,
      "else" => TokenType::Else,
      "return" => TokenType::Return,
      _ => TokenType::Ident,
    }
  }
}

#[derive(Debug, PartialEq, Copy, Clone, Ord, PartialOrd, Eq)]
pub enum TokenType {
  Illigal,
  Eof,

  Ident,
  Int,
  String,

  Assign,
  Plus,
  Minus,
  Bang, // !
  Asterisk,
  Slash,

  Lt,
  Gt,

  Eq,
  NotEq,

  Comma,
  Semicolon,
  Colon,

  Lparen, // (
  Rparen,
  Lbrace,
  Rbrace,
  Lbracket, // [
  Rbracket, // ]

  Function,
  Let,
  True,
  False,
  If,
  Else,
  Return,
}
