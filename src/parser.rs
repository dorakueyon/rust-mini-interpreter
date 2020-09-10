use super::{Expression, Identifier, Lexer, Program, Statement, Token, TokenType};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FormatterResult};

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

enum Precedence {
    Lowest,
    Prefix,
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
        dbg!(&identifier);
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
        let left = match self.current_token.token_type {
            TokenType::Ident => self.parse_ident_expression(),
            TokenType::Int => self.parse_integer_expression(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            _ => {
                println!("no predefined parse expression: {:?}", self.current_token);
                None
            }
        };
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

        dbg!(&program);
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
                        assert!(test_integer_literal(right, tt.2));
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }
    }

    fn test_integer_literal(il: &Box<Expression>, value: i64) -> bool {
        match il.as_ref() {
            Expression::IntegerExpr(i) => {
                assert_eq!(i, &value);
                assert_eq!(format!("{}", il.get_value()), value.to_string())
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
