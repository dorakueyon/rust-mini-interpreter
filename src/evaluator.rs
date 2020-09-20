use super::{
    BlockStatement, Environment, Errors, Expression, HashPair, Identifier, Lexer, Object, Parser,
    Program, Statement, TokenType,
};

use std::collections::BTreeMap;

pub trait Eval {
    fn eval(&self, env: &mut Environment) -> Result<Object, Errors>;
}

impl Eval for Program {
    fn eval(&self, env: &mut Environment) -> Result<Object, Errors> {
        let mut result = Err(Errors::DefaultError); // TODO

        for stmt in self.statements.iter() {
            let obj = stmt.eval(env)?;
            result = Ok(obj.clone());
            match &obj {
                Object::ReturnObj(bo) => {
                    return Ok(bo.as_ref().clone());
                }
                Object::ErrorObj(_) => return result,
                _ => {}
            }
        }
        return result;
    }
}

impl Eval for &BlockStatement {
    fn eval(&self, env: &mut Environment) -> Result<Object, Errors> {
        let mut result = Err(Errors::DefaultError); // TODO

        for stmt in self.statements.iter() {
            let obj = stmt.eval(env)?;
            result = Ok(obj.clone());
            match &obj {
                Object::ReturnObj(_) => return result,
                Object::ErrorObj(_) => return result,
                _ => {}
            }
        }
        return result;
    }
}

impl Eval for Statement {
    fn eval(&self, env: &mut Environment) -> Result<Object, Errors> {
        match self {
            Statement::ExpressionStatement { expression } => return expression.eval(env),
            Statement::ReturnStatement { return_value } => {
                let exp = return_value.eval(env)?;
                return Ok(Object::ReturnObj(Box::new(exp)));
            }
            Statement::LetStatement { identifier, value } => {
                let val = value.eval(env)?;
                if is_error(&val) {
                    return Ok(val);
                }
                env.set(identifier.value.clone(), val);
                Ok(Object::None)
            }
        }
    }
}

impl Eval for &Expression {
    fn eval(&self, env: &mut Environment) -> Result<Object, Errors> {
        match self {
            Expression::IntegerExpr(i) => {
                return Ok(Object::IntegerObj(i.clone()));
            }
            Expression::BooleanExp(t) => match t {
                TokenType::True => Ok(Object::BooleanObj(true)),
                TokenType::False => Ok(Object::BooleanObj(false)),
                _ => Err(Errors::DefaultError), // TODO
            },
            Expression::StrExpr(s) => Ok(Object::StringObj(s.clone())),
            Expression::PrefixExp {
                token: _,
                operator,
                right,
            } => {
                let rt = right.as_ref().eval(env)?;
                if is_error(&rt) {
                    return Ok(rt);
                }
                eval_prefix_expression(operator, rt)
            }
            Expression::InfixExp {
                token: _,
                left,
                operator,
                right,
            } => {
                let lt = left.as_ref().eval(env)?;
                if is_error(&lt) {
                    return Ok(lt);
                }
                let rt = right.as_ref().eval(env)?;
                if is_error(&rt) {
                    return Ok(rt);
                }
                eval_infix_expression(operator, lt, rt)
            }
            Expression::IfExp {
                condition,
                consequesnce,
                alternative,
            } => {
                let cd = condition.as_ref().eval(env)?;
                if is_error(&cd) {
                    return Ok(cd);
                }
                return eval_if_expression(condition.as_ref(), consequesnce, alternative, env);
            }
            Expression::IdentExpr(i) => return eval_identifier(i, env),
            Expression::FnExp { parameters, body } => {
                return Ok(Object::FunctionObj {
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
                let func = function.eval(env)?;
                if is_error(&func) {
                    return Ok(func);
                }

                let args = eval_expressions(arguments, env)?;
                if args.len() == 1 && is_error(&(args[0].clone())) {
                    return Ok(args[0].clone());
                }
                return apply_function(func, args);
            }
            Expression::ArrayExp(elements) => {
                let elmts = eval_expressions(elements, env)?;
                if elmts.len() == 1 && is_error(&elmts[0].clone()) {
                    return Ok(elmts[0].clone());
                }
                return Ok(Object::ArrayObj(elmts));
            }
            Expression::IndexExp { left, index } => {
                let l = left.as_ref().eval(env)?;
                if is_error(&l) {
                    return Ok(l);
                }
                let ix = index.as_ref().eval(env)?;
                if is_error(&ix) {
                    return Ok(ix);
                }
                return eval_index_expression(l, ix);
            }
            Expression::HashExp(pair) => return eval_hash_expression(pair, env),
        }
    }
}

fn eval_hash_expression(
    node: &BTreeMap<Box<Expression>, Box<Expression>>,
    env: &mut Environment,
) -> Result<Object, Errors> {
    let mut pair: BTreeMap<Object, HashPair> = BTreeMap::new();
    for (k, v) in node {
        let key = k.as_ref().eval(env)?;
        if is_error(&key) {
            return Ok(key.clone());
        }
        let value = v.as_ref().eval(env)?;
        if is_error(&value) {
            return Ok(value);
        }

        let hp = HashPair {
            key: key.clone(),
            value,
        };

        pair.insert(key, hp);
    }
    Ok(Object::HashObj(pair))
}

fn eval_index_expression(left: Object, index: Object) -> Result<Object, Errors> {
    let err_obj = Object::ErrorObj(format!(
        "index operator not supported: {}",
        &left.type_name()
    ));
    match &left {
        Object::ArrayObj(objcs) => match index {
            Object::IntegerObj(i) => {
                return eval_array_index_expression(&objcs, &i);
            }
            _ => Ok(err_obj),
        },
        Object::HashObj(left) => {
            if let Some(pair) = left.get(&index) {
                return Ok(pair.value.clone());
            }
            return Ok(Object::Null);
        }
        _ => Ok(err_obj),
    }
}

fn eval_array_index_expression(array: &Vec<Object>, index: &i64) -> Result<Object, Errors> {
    let max = (array.len() - 1) as i64;

    if *index < 0 || *index > max {
        return Ok(Object::Null);
    }
    Ok(array[*(index) as usize].clone())
}

fn apply_function(func: Object, args: Vec<Object>) -> Result<Object, Errors> {
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
        Object::BuilinObj { func } => return Ok(func(args)),
        _ => Some(Object::ErrorObj(format!(
            "not a function: {}",
            func.type_name()
        ))),
    };
    Err(Errors::DefaultError)
}

fn unwrap_return_value(obj: Object) -> Result<Object, Errors> {
    match obj {
        Object::ReturnObj(bo) => {
            return Ok(*bo);
        }
        _ => Ok(obj),
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

fn eval_expressions(
    exps: &Vec<Box<Expression>>,
    env: &mut Environment,
) -> Result<Vec<Object>, Errors> {
    let mut result: Vec<Object> = Vec::new();

    for e in exps {
        let evaluated = e.as_ref().eval(env)?;
        if is_error(&evaluated) {
            return Ok(vec![evaluated]);
        }
        result.push(evaluated);
    }

    Ok(result)
}

fn eval_identifier(i: &String, env: &mut Environment) -> Result<Object, Errors> {
    match env.get(i) {
        Some(o) => {
            return Ok(o);
        }
        None => match env.get_builins(i) {
            Some(o) => Ok(o),
            None => Ok(Object::ErrorObj(format!("identifier not found: {}", i))),
        },
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: &mut Environment,
) -> Result<Object, Errors> {
    let condition = condition.eval(env)?;
    if is_truthy(condition) {
        return consequence.eval(env);
    } else {
        match alternative {
            Some(s) => s.eval(env),
            _ => Ok(Object::Null),
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

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, Errors> {
    match operator {
        "!" => Ok(eval_bang_operator_expression(right)),
        "-" => Ok(eval_minus_prefix_operator_expression(right)),
        _ => Ok(Object::ErrorObj(format!(
            "unknown operator: {}{}",
            operator,
            right.type_name()
        ))),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object, Errors> {
    if operator == "==" {
        return Ok(Object::BooleanObj(left == right));
    }
    if operator == "!=" {
        return Ok(Object::BooleanObj(left != right));
    }
    match &left {
        Object::IntegerObj(l) => match right {
            Object::IntegerObj(r) => eval_integer_infix_expression(operator, l, &r),
            _ => Ok(Object::ErrorObj(format!(
                "type mismatch: {} {} {}",
                &left.type_name(),
                &operator,
                &right.type_name()
            ))),
        },
        Object::StringObj(l) => match right {
            Object::StringObj(r) => eval_string_infix_expression(operator, l, &r),
            _ => Ok(Object::ErrorObj(format!(
                "type mismatch: {} {} {}",
                left.type_name(),
                operator,
                right.type_name()
            ))),
        },
        _ => Ok(Object::ErrorObj(format!(
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

fn eval_string_infix_expression(
    operator: &str,
    left: &String,
    right: &String,
) -> Result<Object, Errors> {
    match operator {
        "+" => Ok(Object::StringObj(format!("{}{}", left, right))),
        _ => Ok(Object::ErrorObj(format!(
            "unknown operator: STRING {} STRING",
            operator
        ))),
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: &i64,
    right: &i64,
) -> Result<Object, Errors> {
    let obj = match operator {
        "+" => Object::IntegerObj(left + right),
        "-" => Object::IntegerObj(left - right),
        "*" => Object::IntegerObj(left * right),
        "/" => {
            if *right == 0 {
                Object::ErrorObj(String::from("invalid expression: divide by zero"))
            } else {
                Object::IntegerObj(left / right)
            }
        }
        ">" => Object::BooleanObj(left > right),
        "<" => Object::BooleanObj(left < right),
        "==" => Object::BooleanObj(left == right),
        "!=" => Object::BooleanObj(left != right),
        _ => Object::ErrorObj(format!("unknown operator: INTEGER {} INTEGER", operator)),
    };
    Ok(obj)
}

fn is_error(obj: &Object) -> bool {
    match obj {
        Object::ErrorObj(_) => return true,
        _ => return false,
    }
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
            let evaluated = test_eval(tt.0.to_string()).unwrap();
            match evaluated {
                Object::IntegerObj(_) => assert!(test_integer_object(&evaluated, tt.1)),
                _ => assert!(test_null_object(evaluated)),
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
            (
                r#"len("1", "2")"#,
                "wrong number of arguments. got=2, want=1",
            ),
            (
                r#"first("1", "2")"#,
                "wrong number of arguments. got=2, want=1",
            ),
            (
                r#"first(1)"#,
                "argument to 'first' must be ARRAY, got INTEGER",
            ),
            (
                r#"last("1", "2")"#,
                "wrong number of arguments. got=2, want=1",
            ),
            (
                r#"last(1)"#,
                "argument to 'last' must be ARRAY, got INTEGER",
            ),
            (
                r#"push([1, 2, 3], [1, 2], 3)"#,
                "wrong number of arguments. got=3, want=2",
            ),
            (
                r#"push(1, 2)"#,
                "argument to 'push' must be ARRAY, got INTEGER",
            ),
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
            let evaluated = test_eval(tt.0.to_string()).unwrap();
            assert!(test_integer_object(&evaluated, tt.1));
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
        let evaluated = test_eval(input.to_string()).unwrap();
        match evaluated {
            Object::FunctionObj {
                parameters,
                body,
                env: _,
            } => {
                assert_eq!(parameters.len(), 1);
                assert_eq!(parameters[0].value, "x");
                assert_eq!(format!("{}", body), "(x + 2)");
            }
            _ => panic!(),
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

    fn test_eval(input: String) -> Result<Object, Errors> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        let mut env = Environment::new();

        Ok(program.eval(&mut env)?)
    }

    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!""#;

        let evaluated = test_eval(input.to_string()).unwrap();
        match evaluated {
            Object::StringObj(s) => assert_eq!(s, "Hello World!"),
            _ => panic!(),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " +  "World!""#;
        let evaluated = test_eval(input.to_string()).unwrap();
        match evaluated {
            Object::StringObj(s) => assert_eq!(s, "Hello World!"),
            _ => panic!(),
        };
    }

    #[test]
    fn test_buildin_functions() {
        let tests = vec![
            (r#"len("")"#, 0),
            (r#"len("four")"#, 4),
            (r#"len("hello world")"#, 11),
            (r#"len([2, 2, 2])"#, 3),
            (r#"len([])"#, 0),
            (r#"first([1, 2, 3])"#, 1),
            (r#"last([1, 2, 3])"#, 3),
        ];
        for tt in tests {
            let evaluated = test_eval(tt.0.to_string()).unwrap();

            match evaluated {
                Object::IntegerObj(_) => assert!(test_integer_object(&evaluated, tt.1)),
                _ => panic!(),
            }
        }

        let input = String::from("rest([1, 2, 3])");
        let evaluated = test_eval(input).unwrap();

        match evaluated {
            Object::ArrayObj(v) => {
                assert_eq!(v.len(), 2);
                assert!(test_integer_object(&v[0], 2));
                assert!(test_integer_object(&v[1], 3));
            }
            _ => panic!(),
        };

        let input = String::from("rest([])");
        let evaluated = test_eval(input).unwrap();

        match evaluated {
            Object::Null => {}
            _ => panic!(),
        };

        let input = String::from(
            "
        let a = [1, 2, 3];
        push(a, 5);
        ",
        );
        let evaluated = test_eval(input).unwrap();

        match evaluated {
            Object::ArrayObj(v) => {
                assert_eq!(v.len(), 4);
                assert!(test_integer_object(&v[0], 1));
                assert!(test_integer_object(&v[1], 2));
                assert!(test_integer_object(&v[2], 3));
                assert!(test_integer_object(&v[3], 5));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_array_literals() {
        let input = String::from("[1, 2 * 2, 3 + 3]");
        let evaluated = test_eval(input).unwrap();
        match evaluated {
            Object::ArrayObj(elements) => {
                assert_eq!(elements.len(), 3);
                test_integer_object(&elements[0], 1);
                test_integer_object(&elements[1], 4);
                test_integer_object(&elements[2], 6);
            }
            _ => panic!(),
        };
    }

    #[test]
    fn test_array_index_expression() {
        let tests = vec![
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][2]", 3),
            ("let i = 0; [1][i];", 1),
            ("[1, 2, 3][1 + 1];", 3),
            ("let myArray = [1, 2, 3]; myArray[2];", 3),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                6,
            ),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2),
            //            ("[1, 2, 3][3]", nil),
            //            ("[1, 2, 3][-1]", nil),
        ];
        for tt in tests {
            let evaluated = test_eval(tt.0.to_string()).unwrap();
            match evaluated {
                Object::IntegerObj(_) => assert!(test_integer_object(&evaluated, tt.1)),
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_array_invalid_index_expression() {
        let tests = vec![("[1, 2, 3][3]"), ("[1, 2, 3][-1]")];
        for tt in tests {
            let evaluated = test_eval(tt.to_string()).unwrap();
            match evaluated {
                Object::Null => {}
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee" : 6/2,
            4: 4,
            true: 5,
            false: 6
        }
        "#;

        let evaluated = test_eval(input.to_string()).unwrap();
        match &evaluated {
            Object::HashObj(pair) => {
                println!("========================");
                assert_eq!(pair.len(), 6);
            }
            _ => panic!(),
        };
    }

    #[test]
    fn test_hash_index_expression() {
        let tests = vec![
            (r#"{"foo": 5}["foo"]"#, 5),
            (r#"let key = "foo"; {"foo": 5}[key]"#, 5),
            (r#"{5: 5}[5]"#, 5),
            (r#"{true: 5}[true]"#, 5),
            (r#"{false: 5}[false]"#, 5),
        ];

        for tt in tests {
            let evaluated = test_eval(tt.0.to_string()).unwrap();
            match &evaluated {
                Object::IntegerObj(_) => assert!(test_integer_object(&evaluated, tt.1)),
                _ => panic!(),
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
    }

    fn test_boolean_object(obj: &Object, expected: bool) -> bool {
        match obj {
            Object::BooleanObj(b) => {
                assert_eq!(b, &expected);
                return true;
            }
            _ => panic!(),
        }
    }
}
