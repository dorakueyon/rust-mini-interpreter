use super::{BlockStatement, Expression, Lexer, Object, Parser, Program, Statement, TokenType};

pub trait Eval {
  fn eval(&self) -> Option<Object>;
}

impl Eval for Program {
  fn eval(&self) -> Option<Object> {
    let mut result = None;

    for stmt in self.statements.iter() {
      result = stmt.eval();

      match stmt.eval() {
        Some(obj) => match obj {
          Object::ReturnObj(bo) => {
            return Some(*bo);
          }
          _ => {}
        },
        None => {}
      }
    }
    return result;
  }
}

impl Eval for &BlockStatement {
  fn eval(&self) -> Option<Object> {
    let mut result = None;

    for stmt in self.statements.iter() {
      result = stmt.eval();

      match stmt.eval() {
        Some(obj) => match obj {
          Object::ReturnObj(bo) => return result,
          _ => {}
        },
        None => {}
      }
    }
    return result;
  }
}

impl Eval for Statement {
  fn eval(&self) -> Option<Object> {
    match self {
      Statement::ExpressionStatement { expression } => return expression.eval(),
      Statement::ReturnStatement { return_value } => {
        let exp = return_value.eval().unwrap();
        return Some(Object::ReturnObj(Box::new(exp)));
      }
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
      Expression::IfExp {
        condition,
        consequesnce,
        alternative,
      } => eval_if_expression(condition.as_ref(), consequesnce, alternative),
      _ => None,
    }
  }
}

fn eval_if_expression(
  condition: &Expression,
  consequence: &BlockStatement,
  alternative: &Option<BlockStatement>,
) -> Option<Object> {
  let condition = condition.eval();
  if is_truthy(condition.unwrap()) {
    return consequence.eval();
  } else {
    match alternative {
      Some(s) => s.eval(),
      _ => Some(Object::Null),
    }
  }
}

fn is_truthy(obj: Object) -> bool {
  match obj {
    Object::Null => false,
    Object::BooleanObj(b) => b,
    _ => true,
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

  #[test]
  fn test_if_else_expressions() {
    let tests = vec![
      ("if (true) {10} ", 10),
      ("if (false) {10} ", 9999), //null
      ("if (1) {10}", 10),
      ("if (1 < 2) {10}", 10),
      ("if (1 > 2) {10}", 9999), //null
      ("if (1 > 2) {10} else {20}", 20),
      ("if (1 < 2) {10} else {20}", 10),
    ];

    for tt in tests {
      let evaluated = test_eval(tt.0.to_string());
      match evaluated {
        Some(e) => match e {
          Object::IntegerObj(i) => assert!(test_integer_object(&e, tt.1)),
          _ => assert!(test_null_object(e)),
        },
        None => panic!(),
      }
    }
  }

  #[test]
  fn test_return_statements() {
    let tests = vec![
      ("return 10;", 10),
      ("return 10; 9;", 10),
      ("return 2 * 5; 9;", 10),
      ("9; return 2 * 5; 9;", 10),
      (
        "if (10 > 1) {
        if (10 > 1){
          return 10;
        }
        return 1;
      }",
        10,
      ),
    ];
    for tt in tests {
      let evaluated = test_eval(tt.0.to_string()).unwrap();
      assert!(test_integer_object(&evaluated, tt.1))
    }
  }

  fn test_null_object(obj: Object) -> bool {
    match obj {
      Object::Null => true,
      _ => false,
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
    dbg!(obj);
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
