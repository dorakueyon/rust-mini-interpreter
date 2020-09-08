use super::{Token, TokenType};
use std::fmt::Debug;

#[derive(Debug)]
pub struct Lexer {
  input: String,
  position: usize,
  read_position: usize,
  //ch: Option<char>,
  ch: u8,
}

impl Lexer {
  pub fn new(input: String) -> Self {
    let mut lexer = Lexer {
      input,
      position: 0,
      read_position: 0,
      ch: 0,
    };
    lexer.read_char();
    lexer
  }

  pub fn read_char(&mut self) {
    if self.read_position >= self.input.chars().count() {
      self.ch = 0
    } else {
      self.ch = self.input.as_bytes()[self.read_position] // u8なので
    }
    self.position = self.read_position;
    self.read_position = self.read_position + 1;
  }

  fn skip_whitespace(&mut self) {
    while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
      self.read_char()
    }
  }

  pub fn next_token(&mut self) -> Token {
    self.skip_whitespace();

    let tok = match self.ch {
      b'=' => {
        if self.peek_char() == b'=' {
          let ch = self.ch;
          self.read_char();
          let literal = String::from_utf8(vec![ch, self.ch]).unwrap();
          Token {
            token_type: TokenType::Eq,
            literal,
          }
        } else {
          new_token(TokenType::Assign, self.ch)
        }
      }
      b';' => new_token(TokenType::Semicolon, self.ch),
      b'(' => new_token(TokenType::Lparen, self.ch),
      b')' => new_token(TokenType::Rparen, self.ch),
      b',' => new_token(TokenType::Comma, self.ch),
      b'+' => new_token(TokenType::Plus, self.ch),
      b'-' => new_token(TokenType::Minus, self.ch),
      b'!' => {
        if self.peek_char() == b'=' {
          let ch = self.ch;
          self.read_char();
          let literal = String::from_utf8(vec![ch, self.ch]).unwrap();
          Token {
            token_type: TokenType::NotEq,
            literal,
          }
        } else {
          new_token(TokenType::Bang, self.ch)
        }
      }
      b'/' => new_token(TokenType::Slash, self.ch),
      b'*' => new_token(TokenType::Asterisk, self.ch),
      b'<' => new_token(TokenType::Lt, self.ch),
      b'>' => new_token(TokenType::Gt, self.ch),
      b'{' => new_token(TokenType::Lbrace, self.ch),
      b'}' => new_token(TokenType::Rbrace, self.ch),
      0 => Token {
        token_type: TokenType::Eof,
        literal: "".to_string(),
      },
      _ => {
        if Lexer::is_letter(self.ch) {
          let literal = self.read_identifier();
          let token_type = Token::lookup_ident(&literal);
          return Token {
            token_type,
            literal,
          };
        } else if Lexer::is_digit(self.ch) {
          let literal = self.read_number();
          let token_type = TokenType::Int;
          return Token {
            token_type,
            literal,
          };
        } else {
          new_token(TokenType::Illigal, self.ch)
        }
      }
    };
    self.read_char();
    tok
  }

  fn is_letter(ch: u8) -> bool {
    return b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z';
  }

  fn is_digit(ch: u8) -> bool {
    return b'0' <= ch && ch <= b'9';
  }

  fn read_number(&mut self) -> String {
    let position = self.position;
    loop {
      if !Lexer::is_digit(self.ch) {
        break;
      }
      self.read_char()
    }
    let mut str = "".to_string();
    for (i, s) in self.input.chars().enumerate() {
      if position <= i && i < self.position {
        str.push(s)
      }
    }
    str
  }

  fn read_identifier(&mut self) -> String {
    let position = self.position;
    loop {
      if !Lexer::is_letter(self.ch) {
        break;
      }
      self.read_char()
    }
    let mut str = "".to_string();
    for (i, s) in self.input.chars().enumerate() {
      if position <= i && i < self.position {
        str.push(s)
      }
    }
    str
  }

  fn peek_char(&self) -> u8 {
    if self.read_position >= self.input.chars().count() {
      0
    } else {
      self.input.as_bytes()[self.read_position]
    }
  }
}

fn new_token(token_type: TokenType, ch: u8) -> Token {
  let str = String::from_utf8(vec![ch]).unwrap();
  Token {
    token_type,
    literal: str,
  }
}

#[cfg(test)]
mod test {

  use super::*;
  #[test]
  fn test_next_token() {
    let input = "
      let five = 5;
      let ten = 10;
      let add = fn(x, y) {
         x + y;
       };

       let result = add(five, ten);
       !-/*5;
       5 < 10 > 5;

       if (5 < 10) {
         return true;
       } else {
         return false;
       }

       10 == 10;
       10 != 9;
       "
    .to_string();

    let tests = vec![
      (TokenType::Let, "let".to_string()),
      (TokenType::Ident, "five".to_string()),
      (TokenType::Assign, "=".to_string()),
      (TokenType::Int, "5".to_string()),
      (TokenType::Semicolon, ";".to_string()),
      (TokenType::Let, "let".to_string()),
      (TokenType::Ident, "ten".to_string()),
      (TokenType::Assign, "=".to_string()),
      (TokenType::Int, "10".to_string()),
      (TokenType::Semicolon, ";".to_string()),
      (TokenType::Let, "let".to_string()),
      (TokenType::Ident, "add".to_string()),
      (TokenType::Assign, "=".to_string()),
      (TokenType::Function, "fn".to_string()),
      (TokenType::Lparen, "(".to_string()),
      (TokenType::Ident, "x".to_string()),
      (TokenType::Comma, ",".to_string()),
      (TokenType::Ident, "y".to_string()),
      (TokenType::Rparen, ")".to_string()),
      (TokenType::Lbrace, "{".to_string()),
      (TokenType::Ident, "x".to_string()),
      (TokenType::Plus, "+".to_string()),
      (TokenType::Ident, "y".to_string()),
      (TokenType::Semicolon, ";".to_string()),
      (TokenType::Rbrace, "}".to_string()),
      (TokenType::Semicolon, ";".to_string()),
      (TokenType::Let, "let".to_string()),
      (TokenType::Ident, "result".to_string()),
      (TokenType::Assign, "=".to_string()),
      (TokenType::Ident, "add".to_string()),
      (TokenType::Lparen, "(".to_string()),
      (TokenType::Ident, "five".to_string()),
      (TokenType::Comma, ",".to_string()),
      (TokenType::Ident, "ten".to_string()),
      (TokenType::Rparen, ")".to_string()),
      (TokenType::Semicolon, ";".to_string()),
      //(TokenType::EOF, "".to_string()),
    ];
    let mut l = Lexer::new(input);
    let mut succeed = true;
    for (i, tt) in tests.iter().enumerate() {
      let tok = l.next_token();
      if tok.token_type != tt.0 {
        println!(
          "{}: Type match failed. expected: {:?}, got: {:?}",
          i + 1,
          &tt.0,
          &tok.token_type
        );
        succeed = false;
      }
      if tok.literal != tt.1 {
        println!(
          "{}: Literal failed.  expected: {:?}, got: {:?}",
          i + 1,
          &tt.1,
          &tok.literal
        );
        succeed = false;
      }
      //assert_eq!(tok.Type, tt.0);
      //assert_eq!(tok.Literal, tt.1);
    }
    assert!(succeed);
  }
}