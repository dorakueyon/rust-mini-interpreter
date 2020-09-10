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
                value.get_value()
            ),
            Statement::ReturnStatement { return_value } => {
                write!(f, "{} {};", self.token_literal(), return_value.get_value())
            }
            Statement::ExpressionStatement { expression } => {
                write!(f, "{}", expression.get_value())
            }
            _ => write!(f, "hoge"),
        }
    }
}

impl Expression {
    pub fn get_value(&self) -> String {
        match self {
            Expression::IntegerExpr(i) => i.to_string(),
            Expression::IdentExpr(s) => s.clone(),
            Expression::PrefixExp {
                token,
                operator,
                right,
            } => {
                let mut s = String::new();
                s.push_str("(");
                s.push_str(operator);
                s.push_str(&right.get_value());
                s.push_str(")");
                s
            }
            _ => "not defined".to_string(),
        }
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
