use super::TokenType;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter, Result as FormatterResult};

#[derive(Debug, PartialEq, Clone, Ord, PartialOrd, Eq)]
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

#[derive(Debug, PartialEq, Clone, Ord, PartialOrd, Eq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement {
                identifier: _,
                value: _,
            } => "let".to_string(),
            Statement::ReturnStatement { return_value: _ } => "return".to_string(),
            Statement::ExpressionStatement { expression } => match expression {
                Expression::IntegerExpr(i) => i.to_string(),
                Expression::IdentExpr(s) => s.clone(),
                _ => "not defined expression".to_string(),
            },
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

impl Display for BlockStatement {
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
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        let s = match self {
            Expression::IntegerExpr(i) => i.to_string(),
            Expression::IdentExpr(s) => s.clone(),
            Expression::StrExpr(s) => s.clone(),
            Expression::PrefixExp {
                token: _,
                operator,
                right,
            } => format!("({}{})", operator, &right),
            Expression::InfixExp {
                token: _,
                right,
                operator,
                left,
            } => format!("({} {} {})", &left, operator, &right),
            Expression::BooleanExp(token) => match token {
                TokenType::True => "true".to_string(),
                TokenType::False => "false".to_string(),
                _ => "not defined".to_string(),
            },
            Expression::IfExp {
                condition,
                consequesnce,
                alternative,
            } => {
                let mut s = String::new();
                s.push_str("if");
                s.push_str(&format!("{}", condition.as_ref()));
                s.push_str(" ");
                s.push_str(&format!("{}", consequesnce));
                if let Some(alt) = alternative {
                    s.push_str("else");
                    s.push_str(&format!("{}", alt));
                }
                s
            }
            Expression::FnExp { parameters, body } => {
                let mut s = String::new();
                s.push_str("fn(");
                for p in parameters {
                    s.push_str(&p.value);
                }
                s.push_str(")");
                s.push_str(&format!("{}", body));

                s
            }
            Expression::CallExp {
                function,
                arguments,
            } => {
                let mut v: Vec<String> = Vec::new();
                for a in arguments {
                    v.push(format!("{}", a));
                }
                let mut s = String::new();
                s.push_str(&format!("{}", function));
                s.push_str("(");
                s.push_str(&v.join(", "));
                //for a in arguments {
                //    s.push_str(&format!("{}", a));
                //}
                s.push_str(")");
                s
            }
            Expression::ArrayExp(elements) => {
                let mut s = String::new();
                s.push('[');
                let mut els = Vec::new();
                for el in elements {
                    els.push(format!("{}", el))
                }
                s.push_str(&els.join(", "));
                s.push(']');
                s
            }
            Expression::IndexExp { left, index } => {
                let mut s = String::new();
                s.push('(');
                s.push_str(&format!("{}", left));
                s.push('[');
                s.push_str(&format!("{}", index));
                s.push_str("])");
                s
            }
            _ => "not defined".to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Clone, Ord, PartialOrd, Eq)]
pub enum Expression {
    IntegerExpr(i64),
    IdentExpr(String),
    StrExpr(String),
    PrefixExp {
        token: TokenType,
        operator: String,
        right: Box<Expression>,
    },
    InfixExp {
        token: TokenType,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    BooleanExp(TokenType),
    IfExp {
        condition: Box<Expression>,
        consequesnce: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FnExp {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    CallExp {
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
    ArrayExp(Vec<Box<Expression>>),
    IndexExp {
        left: Box<Expression>,
        index: Box<Expression>,
    },
    HashExp(BTreeMap<Box<Expression>, Box<Expression>>),
}

#[derive(Debug, PartialEq, Clone, Ord, PartialOrd, Eq)]
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
