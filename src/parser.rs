use super::{BlockStatement, Expression, Identifier, Lexer, Program, Statement, Token, TokenType};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FormatterResult};

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

#[derive(PartialOrd, PartialEq, Debug)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, //> || <
    Sum,         //+ || -
    Product,     // * || /
    Prefix,      //-X || !X
    Call,        //myFunction(X)
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer,
            current_token: Token {
                token_type: TokenType::Eof,
                literal: "".to_string(),
            },
            peek_token: Token {
                token_type: TokenType::Eof,
                literal: "".to_string(),
            },
            errors: vec![],
        };
        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        let pt = &self.peek_token;
        self.current_token = Token {
            token_type: pt.token_type.clone(),
            literal: pt.literal.to_string(),
        };

        self.peek_token = self.lexer.next_token()
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = Vec::new();

        loop {
            if self.current_token.token_type == TokenType::Eof {
                break;
            }
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt)
            }
            self.next_token();
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            TokenType::Let => return self.parse_let_statement(),
            TokenType::Return => return self.parse_return_statement(),
            _ => return self.parse_expression_statement(),
        };
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenType::Ident) {
            return None;
        }
        let identifier = Identifier {
            token: self.current_token.token_type,
            value: self.current_token.literal.clone(),
        };
        if !self.expect_peek(TokenType::Assign) {
            return None;
        }
        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token()
        }
        return Some(Statement::LetStatement {
            identifier,
            value: Expression::Default,
        });
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        while !self.current_token_is(TokenType::Semicolon) {
            self.next_token()
        }

        return Some(Statement::ReturnStatement {
            return_value: Expression::Default,
        });
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(exp) => exp,
            None => return None,
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token()
        }

        Some(Statement::ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left = match self.current_token.token_type {
            TokenType::Ident => self.parse_ident_expression(),
            TokenType::Int => self.parse_integer_expression(),
            TokenType::Bang | TokenType::Minus | TokenType::Plus => self.parse_prefix_expression(),
            TokenType::True | TokenType::False => self.parse_boolean_expression(),
            TokenType::Lparen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            _ => {
                println!("no predefined parse expression: {:?}", self.current_token);
                None
            }
        };

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_predecence() {
            match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt => {
                    self.next_token();
                    left = self.parse_infix_expression(left.unwrap());
                }
                _ => return left,
            };
        }

        left
    }

    fn parse_ident_expression(&self) -> Option<Expression> {
        let ident = self.current_token.literal.clone();
        Some(Expression::IdentExpr(ident))
    }

    fn parse_integer_expression(&self) -> Option<Expression> {
        match self.current_token.token_type {
            TokenType::Int => Some(Expression::IntegerExpr(
                self.current_token.literal.parse::<i64>().unwrap(), //unwrapしていいのか問題
            )),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.current_token.token_type.clone();
        let operator = self.current_token.literal.clone();
        self.next_token();
        let right = Box::new(self.parse_expression(Precedence::Prefix).unwrap());

        Some(Expression::PrefixExp {
            token,
            operator,
            right,
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.current_token.token_type.clone();
        let operator = self.current_token.literal.clone();
        let precedence = self.current_precedence();
        self.next_token();
        let left = Box::new(left);
        let right = Box::new(self.parse_expression(precedence).unwrap());

        Some(Expression::InfixExp {
            token,
            left,
            operator,
            right,
        })
    }

    fn parse_boolean_expression(&mut self) -> Option<Expression> {
        Some(Expression::BooleanExp(self.current_token.token_type))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }
        return exp;
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::Lparen) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest).unwrap();

        if !self.expect_peek(TokenType::Rparen) {
            return None;
        }

        if !self.expect_peek(TokenType::Lbrace) {
            return None;
        }

        let consequesnce = self.parse_block_statement();

        let mut alternative: Option<BlockStatement> = None;
        if self.peek_token_is(TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::Lbrace) {
                return None;
            }
            alternative = Some(self.parse_block_statement())
        }

        Some(Expression::IfExp {
            condition: Box::new(condition),
            consequesnce,
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut statements: Vec<Statement> = Vec::new();
        self.next_token();

        while !self.current_token_is(TokenType::Rbrace) && !self.current_token_is(TokenType::Eof) {
            if let Some(s) = self.parse_statement() {
                statements.push(s)
            }
            self.next_token();
        }

        BlockStatement { statements }
    }

    fn token_to_precedence(token: TokenType) -> Precedence {
        match token {
            TokenType::Eq | TokenType::NotEq => Precedence::Equals,
            TokenType::Lt | TokenType::Gt => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Slash | TokenType::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn peek_predecence(&self) -> Precedence {
        Parser::token_to_precedence(self.peek_token.token_type)
    }

    fn current_precedence(&self) -> Precedence {
        Parser::token_to_precedence(self.current_token.token_type)
    }

    fn current_token_is(&self, t: TokenType) -> bool {
        return self.current_token.token_type == t;
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            return true;
        } else {
            println!("expected {:?}. got {:?}", t, self.peek_token.token_type);
            self.errors.push(ParseError::DefaultError);
            return false;
        }
    }
    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let  foobar = 838383;
      "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);
        let tests = vec!["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            let statement = &program.statements[i];
            assert!(test_let_statement(statement, tt));
        }
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        assert_eq!(s.token_literal(), "let");
        match s {
            Statement::LetStatement { identifier, value } => {
                assert_eq!(identifier.value, name);
                return true;
            }
            _ => return false,
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
      "
        .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            assert_eq!(stmt.token_literal(), "return");
            match stmt {
                Statement::ReturnStatement { return_value } => {}
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement { expression } => match expression {
                Expression::BooleanExp(_) => {
                    assert_eq!(format!("{}", expression), "true".to_string())
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".to_string();

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement { expression } => match expression {
                Expression::IdentExpr(s) => {
                    assert_eq!(s, "foobar");
                    assert_eq!(program.statements[0].token_literal(), "foobar");
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_string();
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement { expression } => match expression {
                Expression::IntegerExpr(i) => {
                    assert_eq!(i.clone(), 5);
                    assert_eq!(&program.statements[0].token_literal(), "5");
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for tt in prefix_tests.iter() {
            let l = Lexer::new(String::from(tt.0));
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                Statement::ExpressionStatement { expression } => match expression {
                    Expression::PrefixExp {
                        token,
                        operator,
                        right,
                    } => {
                        assert_eq!(operator, tt.1);
                        assert!(test_integer_literal(Box::new(right.as_ref()), &tt.2));
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            // 0: input, 1: left_value, 2: operator, 3: right_value
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 > 5", 5, ">", 5),
            ("5 < 5", 5, "<", 5),
            ("5 == 5", 5, "==", 5),
            ("5 != 5", 5, "!=", 5),
        ];

        for tt in infix_tests.iter() {
            let l = Lexer::new(tt.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                Statement::ExpressionStatement { expression } => match expression {
                    Expression::InfixExp {
                        token,
                        left,
                        operator,
                        right,
                    } => {
                        assert!(test_integer_literal(Box::new(left.as_ref()), &tt.1));
                        assert_eq!(operator, tt.2);
                        assert!(test_integer_literal(Box::new(right.as_ref()), &tt.3));
                        //assert!(test_infix_expression(expression, left, operator, right));
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }

        let infix_tests = vec![
            // 0: input, 1: left_value, 2: operator, 3: right_value
            ("true == true", true, "==", true),
            ("true != true", true, "!=", true),
            ("false == false", false, "==", false),
        ];

        for tt in infix_tests.iter() {
            let l = Lexer::new(tt.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);
            match &program.statements[0] {
                Statement::ExpressionStatement { expression } => match expression {
                    Expression::InfixExp {
                        token,
                        left,
                        operator,
                        right,
                    } => {
                        assert!(test_boolean_literal(left, tt.1));
                        assert_eq!(operator, &tt.2.to_string());
                        assert!(test_boolean_literal(right, tt.3));
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("1 + (2+ 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / ( 5 + 5)", "(2 / (5 + 5))"),
            ("-( 5 + 5)", "(-(5 + 5))"),
            ("!(true  == true)", "(!(true == true))"),
        ];

        for tt in tests.iter() {
            let l = Lexer::new(tt.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);
            //dbg!(&program);

            assert_eq!(format!("{}", program), tt.1);
        }
    }

    fn test_identifier(exp: &Expression, value: String) -> bool {
        match exp {
            Expression::IdentExpr(s) => {
                assert_eq!(s, &value);
                assert_eq!(format!("{}", &exp), value);
                return true;
            }
            _ => {
                println!("expression not IdentExpr");
                return false;
            }
        }
    }

    #[test]
    fn test_if_ixpression() {
        let input = "if ( x < y) { x }";

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStatement { expression } => match expression {
                Expression::IfExp {
                    condition,
                    consequesnce,
                    alternative,
                } => {
                    //assert!(test_infix_expression(exp, left_arg, operator_arg, right_arg))}
                    assert_eq!(consequesnce.statements.len(), 1);
                    match &consequesnce.statements[0] {
                        Statement::ExpressionStatement { expression } => {
                            assert!(test_identifier(expression, "x".to_string()));
                        }
                        _ => panic!(),
                    }
                    assert!(alternative.is_none());
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    #[test]
    fn test_if_else_ixpression() {
        let input = "if ( x < y) { x } else { y }";

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStatement { expression } => match expression {
                Expression::IfExp {
                    condition,
                    consequesnce,
                    alternative,
                } => {
                    //assert!(test_infix_expression(exp, left_arg, operator_arg, right_arg))}
                    assert_eq!(consequesnce.statements.len(), 1);
                    match &consequesnce.statements[0] {
                        Statement::ExpressionStatement { expression } => {
                            assert!(test_identifier(expression, "x".to_string()));
                        }
                        _ => panic!(),
                    }
                    match alternative {
                        Some(blc) => {
                            assert_eq!(blc.statements.len(), 1);
                            match &blc.statements[0] {
                                Statement::ExpressionStatement { expression } => {
                                    assert!(test_identifier(expression, "y".to_string()));
                                }
                                _ => panic!(),
                            }
                        }
                        None => panic!(),
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    trait TestLiteral<T> {
        fn test_literal(self, exp: &Expression) -> bool;
    }
    impl TestLiteral<Box<i64>> for Box<i64> {
        fn test_literal(self, exp: &Expression) -> bool {
            test_integer_literal(Box::new(&exp), self.as_ref())
        }
    }
    impl TestLiteral<String> for String {
        fn test_literal(self, exp: &Expression) -> bool {
            test_identifier(exp, self)
        }
    }

    fn test_literal_expression<T>(exp: &Expression, expected: T) -> bool
    where
        T: TestLiteral<T>,
    {
        expected.test_literal(exp)
    }

    fn test_boolean_literal(exp: &Expression, value: bool) -> bool {
        match exp {
            Expression::BooleanExp(t) => match t {
                TokenType::True => {
                    assert!(value);
                    assert_eq!(format!("{}", exp), "true");
                }
                TokenType::False => {
                    assert!(!value);
                    assert_eq!(format!("{}", exp), "false");
                }
                _ => return false,
            },
            _ => return false,
        }

        return true;
    }

    fn test_infix_expression<T>(
        exp: &Expression,
        left_arg: T,
        operator_arg: &String,
        right_arg: T,
    ) -> bool
    where
        T: TestLiteral<T>,
    {
        match exp {
            Expression::InfixExp {
                token,
                operator,
                left,
                right,
            } => {
                assert!(test_literal_expression(left.as_ref(), left_arg));
                assert_eq!(operator, operator_arg);
                assert!(test_literal_expression(right.as_ref(), right_arg));
            }
            _ => panic!(),
        }
        true
    }

    fn test_integer_literal(il: Box<&Expression>, value: &i64) -> bool {
        match il.as_ref() {
            Expression::IntegerExpr(i) => {
                assert_eq!(i, value);
                assert_eq!(format!("{}", il), value.to_string())
            }
            _ => panic!(),
        }
        true
    }

    fn check_parser_errors(p: &Parser) {
        let errors = &p.errors;
        if errors.len() == 0 {
            return;
        }

        println!("parser has {} errors", errors.len());
        for error in errors.iter() {
            println!("{}", error);
        }
        panic!();
    }
}

impl Error for ParseError {}

enum ParseError {
    DefaultError,
}

impl ParseError {
    fn message(&self) -> &str {
        match self {
            DefaultError => "Default Error",
            _ => "NotDefined",
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        write!(f, "{}", self.message())
    }
}
impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        write!(f, "{}", self.message())
    }
}
