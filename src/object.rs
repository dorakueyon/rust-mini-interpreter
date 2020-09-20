use super::{builtins, BlockStatement, Identifier};
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Environment {
    store: BTreeMap<String, Object>,
    outer: BTreeMap<String, Object>,
    builtins: BTreeMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: BTreeMap::new(),
            outer: BTreeMap::new(),
            builtins: builtins::new(),
        }
    }
    pub fn get(&self, name: &String) -> Option<Object> {
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

    pub fn get_builins(&self, name: &String) -> Option<Object> {
        match self.builtins.get(name) {
            Some(value) => Some(value.to_owned()),
            None => None,
        }
    }

    pub fn new_enclosed_environment(outer_env: &Environment) -> Environment {
        let mut env = Environment::new();
        env.outer = outer_env.store.clone();
        env
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Ord, PartialOrd)]
pub enum Object {
    Null,
    IntegerObj(i64),
    BooleanObj(bool),
    StringObj(String),
    ReturnObj(Box<Object>),
    FunctionObj {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        env: Environment,
    },
    BuilinObj {
        func: fn(Vec<Object>) -> Object,
    },
    ArrayObj(Vec<Object>),
    HashObj(BTreeMap<Object, HashPair>),
    ErrorObj(String),
}

#[derive(Debug, PartialEq, Clone, Eq, Ord, PartialOrd)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::IntegerObj(i) => format!("{}", i),
            Object::BooleanObj(b) => format!("{}", b),
            Object::ReturnObj(b) => b.as_ref().inspect(),
            Object::StringObj(s) => s.clone(),
            Object::ArrayObj(v) => {
                let mut elms = Vec::new();

                for elm in v {
                    elms.push(elm.inspect());
                }

                let mut s = String::new();
                s.push('[');
                s.push_str(&elms.join(", "));
                s.push(']');
                s
            }
            Object::HashObj(pair) => {
                let mut pairs = Vec::new();
                for (key, hp) in pair {
                    let mut t = String::new();
                    t.push_str(&key.inspect());
                    t.push_str(": ");
                    t.push_str(&hp.value.inspect());
                    pairs.push(t);
                }
                let mut s = String::new();
                s.push('{');
                s.push_str(&pairs.join(", "));
                s.push('}');
                s
            }
            Object::ErrorObj(s) => s.clone(),
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
