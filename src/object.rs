#[derive(Debug, PartialEq)]
pub enum Object {
  Null,
  IntegerObj(i64),
  BooleanObj(bool),
  ReturnObj(Box<Object>),
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
}
