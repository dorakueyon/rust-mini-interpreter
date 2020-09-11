use super::TokenType;
use std::fmt::{Display, Formatter, Result as FormatterResult};

#[derive(Debug)]
pub enum Statement {
    LetStatement {
        identifier: Identifier,
        value: Expression,
    },
    ReturnStatement {
        return_value: Expression,
    },
    ExpressionStatement {
        expression: Expression,
    },
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement { identifier, value } => "let".to_string(),
            Statement::ReturnStatement { return_value } => "return".to_string(),
            Statement::ExpressionStatement { expression } => match expression {
                Expression::IntegerExpr(i) => i.to_string(),
                Expression::IdentExpr(s) => s.clone(),
                _ => "not defined expression".to_string(),
            },
            _ => "not defined".to_string(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        let mut s = String::new();
        for stmt in self.statements.iter() {
            let fmt = format!("{}", stmt);
            s.push_str(&fmt)
        }
        return write!(f, "{}", s);
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        match self {
            Statement::LetStatement { identifier, value } => write!(
                f,
                "{} {} = {};",
                self.token_literal(),
                identifier.value,
                value
            ),
            Statement::ReturnStatement { return_value } => {
                write!(f, "{} {};", self.token_literal(), return_value)
            }
            Statement::ExpressionStatement { expression } => write!(f, "{}", expression),
            _ => write!(f, "hoge"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        let s = match self {
            Expression::IntegerExpr(i) => i.to_string(),
            Expression::IdentExpr(s) => s.clone(),
            Expression::PrefixExp {
                token,
                operator,
                right,
            } => format!("({}{})", operator, &right),
            Expression::InfixExp {
                token,
                right,
                operator,
                left,
            } => format!("({} {} {})", &left, operator, &right),
            _ => "not defined".to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub enum Expression {
    Default,
    IntegerExpr(i64),
    IdentExpr(String),
    PrefixExp {
        token: TokenType,
        operator: String,
        right: Box<Expression>,
    },
    InfixExp {
        token: TokenType,
        right: Box<Expression>,
        operator: String,
        left: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Identifier {
    pub token: TokenType,
    pub value: String,
}

impl Identifier {}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_string() {
        let stmts = vec![Statement::LetStatement {
            identifier: Identifier {
                token: TokenType::Ident,
                value: "myVar".to_string(),
            },
            value: Expression::IdentExpr("anotherVar".to_string()),
        }];
        let program = Program { statements: stmts };
        assert_eq!(format!("{}", program), "let myVar = anotherVar;");
    }
}
