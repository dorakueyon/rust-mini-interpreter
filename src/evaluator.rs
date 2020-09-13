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

impl Eval for &Expression {
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
      Expression::PrefixExp {
        token,
        operator,
        right,
      } => {
        let exp = right.as_ref();
        let exp = exp.eval().unwrap();
        eval_prefix_expression(operator, exp)
      }
      Expression::InfixExp {
        token,
        left,
        operator,
        right,
      } => {
        let lt = left.as_ref().eval().unwrap();
        let rt = right.as_ref().eval().unwrap();
        eval_infix_expression(operator, lt, rt)
      }
      _ => None,
    }
  }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Option<Object> {
  match operator {
    "!" => Some(eval_bang_operator_expression(right)),
    "-" => Some(eval_minus_prefix_operator_expression(right)),
    _ => None,
  }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Option<Object> {
  if operator == "==" {
    return Some(Object::BooleanObj(left == right));
  }
  if operator == "!=" {
    return Some(Object::BooleanObj(left != right));
  }
  match left {
    Object::IntegerObj(l) => match right {
      Object::IntegerObj(r) => eval_integer_infix_expression(operator, l, r),
      _ => Some(Object::Null),
    },
    _ => Some(Object::Null),
  }
}

fn eval_bang_operator_expression(right: Object) -> Object {
  match right {
    Object::BooleanObj(b) => {
      if b {
        Object::BooleanObj(false)
      } else {
        Object::BooleanObj(true)
      }
    }
    _ => Object::BooleanObj(false),
  }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Option<Object> {
  match operator {
    "+" => Some(Object::IntegerObj(left + right)),
    "-" => Some(Object::IntegerObj(left - right)),
    "*" => Some(Object::IntegerObj(left * right)),
    "/" => Some(Object::IntegerObj(left / right)),
    ">" => Some(Object::BooleanObj(left > right)),
    "<" => Some(Object::BooleanObj(left < right)),
    "==" => Some(Object::BooleanObj(left == right)),
    "!=" => Some(Object::BooleanObj(left != right)),
    _ => Some(Object::Null),
  }
}

fn eval_minus_prefix_operator_expression(obj: Object) -> Object {
  match obj {
    Object::IntegerObj(i) => Object::IntegerObj(-i),
    _ => Object::Null,
  }
}

#[cfg(test)]
mod test {
  use super::*;
  #[test]
  fn test_eval_integer_expression() {
    let tests = vec![
      ("5", 5),
      ("10", 10),
      ("-5", -5),
      ("-10", -10),
      ("5 + 5 + 5 +5 -10", 10),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-50 + 100 + -50", 0),
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

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

  #[test]
  fn test_bang_operator() {
    let tests = vec![
      ("!true", false),
      ("!false", true),
      ("!5", false),
      ("!!true", true),
      ("!!false", false),
      ("!!5", true),
      ("1 < 2", true),
      ("1 > 2", false),
      ("1 < 1", false),
      ("1 > 1", false),
      ("1 == 1", true),
      ("1 != 1", false),
      ("1 == 2", false),
      ("1 != 2", true),
      ("true == true", true),
      ("false == false", true),
      ("true == false", false),
      ("true != false", true),
      ("false != true", true),
      ("(1 < 2) == true", true),
      ("(1 < 2) == false", false),
      ("(1 > 2) == true", false),
      ("(1 > 2) == false", true),
    ];

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
      Some(s) => {
        dbg!(&s);
        Some(s)
      }
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
