use super::{Expression, Lexer, Object, Parser, Program, Statement, TokenType};

pub trait Eval {
  fn eval(&self) -> Option<Object>;
}

impl Eval for Program {
  fn eval(&self) -> Option<Object> {
    self.statements.eval()
  }
}

impl Eval for Vec<Statement> {
  fn eval(&self) -> Option<Object> {
    //for stmt in self {
    //  //      return stmt.eval();
    //  return None;

    self[0].eval()
  }
}

impl Eval for Statement {
  fn eval(&self) -> Option<Object> {
    match self {
      Statement::ExpressionStatement { expression } => return expression.eval(),
      _ => None,
    }
  }
}

impl Eval for Expression {
  fn eval(&self) -> Option<Object> {
    match self {
      Expression::IntegerExpr(i) => {
        return Some(Object::IntegerObj(i.clone()));
      }
      Expression::BooleanExp(t) => match t {
        TokenType::True => Some(Object::BooleanObj(true)),
        TokenType::False => Some(Object::BooleanObj(false)),
        _ => None,
      },
      _ => None,
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn test_eval_integer_expression() {
    let tests = vec![("5", 5), ("10", 10)];

    for tt in tests {
      let evaluated = test_eval(tt.0.to_string()).unwrap();
      assert!(test_integer_object(&evaluated, tt.1));
    }
  }

  #[test]
  fn test_eval_boolean_expression() {
    let tests = vec![("true", true), ("false", false)];
    for tt in tests {
      let evaluated = test_eval(tt.0.to_string()).unwrap();
      assert!(test_boolean_object(&evaluated, tt.1))
    }
  }

  fn test_eval(input: String) -> Option<Object> {
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let program = p.parse_program();

    match program.eval() {
      Some(s) => Some(s),
      _ => None,
    }
  }

  fn test_integer_object(obj: &Object, expected: i64) -> bool {
    match obj {
      Object::IntegerObj(value) => {
        assert_eq!(value, &expected);
        return true;
      }
      _ => panic!(),
    }
    false
  }

  fn test_boolean_object(obj: &Object, expected: bool) -> bool {
    match obj {
      Object::BooleanObj(b) => {
        assert_eq!(b, &expected);
        return true;
      }
      _ => panic!(),
    }
    false
  }
}
