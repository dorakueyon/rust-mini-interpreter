#[derive(Debug)]
pub enum Object {
  Integer(i64),
}

impl Object {
  pub fn inspect(&self) -> String {
    match self {
      Object::Integer(i) => format!("{}", i),
    }
  }
}
