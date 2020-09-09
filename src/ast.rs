use super::TokenType;

#[derive(Debug)]
pub enum Statement {
    LetStatement {
        identifier: Identifier,
        value: Expression,
    },
    ReturnStatement {
        return_value: Expression,
    },
}

#[derive(Debug)]
pub enum Expression {
    Default,
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

impl Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            let mut statement = &self.statements[0];
            //statement.token_literal()
            "".to_string()
        } else {
            "".to_string()
        }
    }
}
