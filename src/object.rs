use super::{BlockStatement, Identifier};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: HashMap::new(),
        }
    }
    pub fn get(&self, name: &String) -> Option<Object> {
        dbg!(&self.outer);
        match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => match self.outer.get(name) {
                Some(outer_value) => Some(outer_value.clone()),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }

    pub fn new_enclosed_environment(outer_env: &Environment) -> Environment {
        let mut env = Environment::new();
        env.outer = outer_env.store.clone();
        env
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    IntegerObj(i64),
    BooleanObj(bool),
    ReturnObj(Box<Object>),
    FunctionObj {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        env: Environment,
    },
    ErrorObj(String),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::IntegerObj(i) => format!("{}", i),
            Object::BooleanObj(b) => format!("{}", b),
            Object::ReturnObj(b) => format! {"{}", b.as_ref().inspect()},
            Object::Null => "null".to_string(),
            _ => "".to_string(),
        }
    }
    pub fn type_name(&self) -> &str {
        match self {
            Object::IntegerObj(_) => "INTEGER",
            Object::BooleanObj(_) => "BOOLEAN",
            _ => "NOT_DEFINED",
        }
    }
}
