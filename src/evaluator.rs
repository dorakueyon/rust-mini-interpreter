use super::{Expression, Lexer, Object, Parser, Program, Statement};

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
        return Some(Object::Integer(i.clone()));
      }
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
      Object::Integer(value) => {
        assert_eq!(value, &expected);
        return true;
      }
      _ => panic!(),
    }
    false
  }
}
