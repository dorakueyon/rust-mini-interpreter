#[derive(Debug)]
pub enum Object {
  IntegerObj(i64),
  BooleanObj(bool),
}

impl Object {
  pub fn inspect(&self) -> String {
    match self {
      Object::IntegerObj(i) => format!("{}", i),
      Object::BooleanObj(b) => format!("{}", b),
      _ => "".to_string(),
    }
  }
}
