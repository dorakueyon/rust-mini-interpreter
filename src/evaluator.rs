use super::{
    BlockStatement, Environment, Expression, Identifier, Lexer, Object, Parser, Program, Statement,
    TokenType,
};

pub trait Eval {
    fn eval(&self, env: &mut Environment) -> Option<Object>;
}

impl Eval for Program {
    fn eval(&self, env: &mut Environment) -> Option<Object> {
        let mut result = None;

        for stmt in self.statements.iter() {
            result = stmt.eval(env);

            match stmt.eval(env) {
                Some(obj) => match obj {
                    Object::ReturnObj(bo) => {
                        return Some(*bo);
                    }
                    Object::ErrorObj(_) => return result,
                    _ => {}
                },
                None => {}
            }
        }
        return result;
    }
}

impl Eval for &BlockStatement {
    fn eval(&self, env: &mut Environment) -> Option<Object> {
        let mut result = None;

        for stmt in self.statements.iter() {
            result = stmt.eval(env);

            match stmt.eval(env) {
                Some(obj) => match obj {
                    Object::ReturnObj(bo) => return result,
                    Object::ErrorObj(_) => return result,
                    _ => {}
                },
                None => {}
            }
        }
        return result;
    }
}

impl Eval for Statement {
    fn eval(&self, env: &mut Environment) -> Option<Object> {
        match self {
            Statement::ExpressionStatement { expression } => return expression.eval(env),
            Statement::ReturnStatement { return_value } => {
                let exp = return_value.eval(env).unwrap();
                return Some(Object::ReturnObj(Box::new(exp)));
            }
            Statement::LetStatement { identifier, value } => {
                let val = value.eval(env);
                if is_error(value.eval(env)) {
                    return val;
                }
                env.set(identifier.value.clone(), val.unwrap());
                None
            }
            _ => None,
        }
    }
}

impl Eval for &Expression {
    fn eval(&self, env: &mut Environment) -> Option<Object> {
        match self {
            Expression::IntegerExpr(i) => {
                return Some(Object::IntegerObj(i.clone()));
            }
            Expression::BooleanExp(t) => match t {
                TokenType::True => Some(Object::BooleanObj(true)),
                TokenType::False => Some(Object::BooleanObj(false)),
                _ => None,
            },
            Expression::StrExpr(s) => Some(Object::StringObj(s.clone())),
            Expression::PrefixExp {
                token,
                operator,
                right,
            } => {
                let exp = right.as_ref().eval(env);
                if is_error(right.as_ref().eval(env)) {
                    return exp;
                }
                eval_prefix_expression(operator, exp.unwrap())
            }
            Expression::InfixExp {
                token,
                left,
                operator,
                right,
            } => {
                let lt = left.as_ref().eval(env).unwrap();
                if is_error(left.as_ref().eval(env)) {
                    return Some(lt);
                }
                let rt = right.as_ref().eval(env).unwrap();
                if is_error(right.as_ref().eval(env)) {
                    return Some(rt);
                }
                eval_infix_expression(operator, lt, rt)
            }
            Expression::IfExp {
                condition,
                consequesnce,
                alternative,
            } => {
                let cd = condition.as_ref().eval(env);
                if is_error(condition.as_ref().eval(env)) {
                    return cd;
                }
                return eval_if_expression(condition.as_ref(), consequesnce, alternative, env);
            }
            Expression::IdentExpr(i) => return eval_identifier(i, env),
            Expression::FnExp { parameters, body } => {
                return Some(Object::FunctionObj {
                    parameters: parameters.clone(),
                    body: body.clone(),
                    env: env.clone(),
                });
            }
            Expression::CallExp {
                function,
                arguments,
            } => {
                let function = function.as_ref();
                let func = function.eval(env);
                if is_error(function.eval(env)) {
                    return func;
                }
                let args = eval_expressions(arguments, env);
                if args.len() == 1 && is_error(Some(args[0].clone())) {
                    return Some(args[0].clone());
                }
                return apply_function(func.unwrap(), args);
            }
            _ => None,
        }
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> Option<Object> {
    match func {
        Object::FunctionObj {
            parameters,
            body,
            env,
        } => {
            let mut extended_env = extend_function_env(&env, &parameters, &args);
            let b = &body;
            let evaluated = b.eval(&mut extended_env);
            return unwrap_return_value(evaluated.unwrap());
        }
        Object::BuilinObj { func } => return Some(func(args)),
        _ => Some(Object::ErrorObj(format!(
            "not a function: {}",
            func.type_name()
        ))),
    };
    None
}

fn unwrap_return_value(obj: Object) -> Option<Object> {
    match obj {
        Object::ReturnObj(bo) => {
            return Some(*bo);
        }
        _ => Some(obj),
    }
}

fn extend_function_env(
    env: &Environment,
    params: &Vec<Identifier>,
    args: &Vec<Object>,
) -> Environment {
    let mut env = Environment::new_enclosed_environment(env);
    for (i, param) in params.iter().enumerate() {
        env.set(param.value.clone(), args[i].clone());
    }

    env
}

fn eval_expressions(exps: &Vec<Box<Expression>>, env: &mut Environment) -> Vec<Object> {
    let mut result: Vec<Object> = Vec::new();

    for e in exps {
        let evaluated = e.as_ref().eval(env);
        if is_error(evaluated.clone()) {
            if let Some(ob) = evaluated {
                return vec![ob];
            }
        }
        match evaluated {
            Some(o) => result.push(o),
            None => (),
        }
    }

    result
}

fn eval_identifier(i: &String, env: &mut Environment) -> Option<Object> {
    match env.get(i) {
        Some(o) => {
            return Some(o);
        }
        None => match env.get_builins(i) {
            Some(o) => Some(o),
            None => Some(Object::ErrorObj(format!("identifier not found: {}", i))),
        },
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: &mut Environment,
) -> Option<Object> {
    let condition = condition.eval(env);
    if is_truthy(condition.unwrap()) {
        return consequence.eval(env);
    } else {
        match alternative {
            Some(s) => s.eval(env),
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
        _ => Some(Object::ErrorObj(format!(
            "unknown operator: {}{}",
            operator,
            right.type_name()
        ))),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Option<Object> {
    if operator == "==" {
        return Some(Object::BooleanObj(left == right));
    }
    if operator == "!=" {
        return Some(Object::BooleanObj(left != right));
    }
    match &left {
        Object::IntegerObj(l) => match right {
            Object::IntegerObj(r) => eval_integer_infix_expression(operator, l, &r),
            _ => Some(Object::ErrorObj(format!(
                "type mismatch: {} {} {}",
                &left.type_name(),
                &operator,
                &right.type_name()
            ))),
        },
        Object::StringObj(l) => match right {
            Object::StringObj(r) => eval_string_infix_expression(operator, l, &r),
            _ => Some(Object::ErrorObj(format!(
                "type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ))),
        },
        _ => Some(Object::ErrorObj(format!(
            "unknown operator: {} {} {}",
            left.type_name(),
            operator,
            right.type_name()
        ))),
    }
    //Some(Object::ErrorObj(format!("unknown operator:")))
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

fn eval_string_infix_expression(operator: &str, left: &String, right: &String) -> Option<Object> {
    match operator {
        "+" => Some(Object::StringObj(format!("{}{}", left, right))),
        _ => Some(Object::ErrorObj(format!(
            "unknown operator: STRING {} STRING",
            operator
        ))),
    }
}

fn eval_integer_infix_expression(operator: &str, left: &i64, right: &i64) -> Option<Object> {
    match operator {
        "+" => Some(Object::IntegerObj(left + right)),
        "-" => Some(Object::IntegerObj(left - right)),
        "*" => Some(Object::IntegerObj(left * right)),
        "/" => Some(Object::IntegerObj(left / right)),
        ">" => Some(Object::BooleanObj(left > right)),
        "<" => Some(Object::BooleanObj(left < right)),
        "==" => Some(Object::BooleanObj(left == right)),
        "!=" => Some(Object::BooleanObj(left != right)),
        _ => Some(Object::ErrorObj(format!(
            "unknown operator: INTEGER {} INTEGER",
            operator
        ))),
    }
}

fn is_error(obj: Option<Object>) -> bool {
    if let Some(o) = obj {
        match o {
            Object::ErrorObj(_) => return true,
            _ => return false,
        }
    }

    false
}

fn eval_minus_prefix_operator_expression(obj: Object) -> Object {
    match obj {
        Object::IntegerObj(i) => Object::IntegerObj(-i),
        _ => Object::ErrorObj(format!("unknown operator: -{}", obj.type_name())),
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

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
            (r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
            ("len(1)", "argument to 'len' not supported, got INTEGER"),
            (r#"len("1", "2")"#, "wrong number of arguments. got=2, want=1"),
        ];
        for tt in tests {
            let evaluated = test_eval(tt.0.to_string());
            match evaluated.unwrap() {
                Object::ErrorObj(message) => assert_eq!(message, tt.1),
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for tt in tests {
            let evaluated = test_eval(tt.0.to_string());
            assert!(test_integer_object(&evaluated.unwrap(), tt.1));
        }
    }

    fn test_null_object(obj: Object) -> bool {
        match obj {
            Object::Null => true,
            _ => false,
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";
        let evaluated = test_eval(input.to_string());
        match evaluated {
            Some(o) => match o {
                Object::FunctionObj {
                    parameters,
                    body,
                    env,
                } => {
                    assert_eq!(parameters.len(), 1);
                    assert_eq!(parameters[0].value, "x");
                    assert_eq!(format!("{}", body), "(x + 2)");
                }
                _ => panic!(),
            },
            None => panic!(),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, 10);", 20),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(6,7));", 23),
            ("fn(x) {x;} (5)", 5),
        ];

        for tt in tests {
            let evaluated = test_eval(tt.0.to_string());
            assert!(test_integer_object(&evaluated.unwrap(), tt.1))
        }
    }

    #[test]
    fn test_closures() {
        let input = "
let newAdder = fn(x) {
fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);
        ";
        let evaluated = test_eval(input.to_string());
        assert!(test_integer_object(&evaluated.unwrap(), 4));
    }

    fn test_eval(input: String) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();

        match program.eval(&mut env) {
            Some(s) => Some(s),
            _ => None,
        }
    }

    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!""#;

        let evaluated = test_eval(input.to_string());
        match evaluated {
            Some(o) => match o {
                Object::StringObj(s) => assert_eq!(s, "Hello World!"),
                _ => panic!(),
            },
            None => panic!(),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " +  "World!""#;
        let evaluated = test_eval(input.to_string());
        match evaluated {
            Some(o) => match o {
                Object::StringObj(s) => assert_eq!(s, "Hello World!"),
                _ => panic!(),
            },
            None => panic!(),
        }
    }

    #[test]
    fn test_buildin_functions() {
        let tests = vec![
            (r#"len("")"#, 0),
            (r#"len("four")"#, 4),
            (r#"len("hello world")"#, 11),
        ];
        for tt in tests {
            let evaluated = test_eval(tt.0.to_string());

            dbg!(&evaluated);
            match evaluated {
                Some(o) => match o {
                    Object::IntegerObj(i) => assert!(test_integer_object(&o, tt.1)),
                    _ => panic!(),
                },
                None => panic!(),
            }
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
