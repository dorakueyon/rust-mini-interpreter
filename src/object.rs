#[derive(Debug, PartialEq)]
pub enum Object {
  Null,
  IntegerObj(i64),
  BooleanObj(bool),
}

impl Object {
  pub fn inspect(&self) -> String {
    match self {
      Object::IntegerObj(i) => format!("{}", i),
      Object::BooleanObj(b) => format!("{}", b),
      Object::Null => "null".to_string(),
      _ => "".to_string(),
    }
  }
}
