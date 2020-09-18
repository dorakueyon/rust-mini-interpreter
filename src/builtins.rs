use super::Object;
use std::collections::HashMap;

pub fn new() -> HashMap<String, Object> {
  let mut builtin_functions = HashMap::new();

  builtin_functions.insert(String::from("len"), Object::BuilinObj { func: len });
  builtin_functions
}

fn len(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return Object::ErrorObj(format!(
      "wrong number of arguments. got={}, want=1",
      args.len()
    ));
  }
  match &args[0] {
    Object::StringObj(s) => Object::IntegerObj(s.chars().count() as i64),
    _ => {
      return Object::ErrorObj(format!(
        "argument to 'len' not supported, got {}",
        args[0].type_name()
      ))
    }
  }
}
