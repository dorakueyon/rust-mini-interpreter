use super::Object;
use std::collections::BTreeMap;

pub fn new() -> BTreeMap<String, Object> {
  let mut builtin_functions = BTreeMap::new();

  builtin_functions.insert(String::from("len"), Object::BuilinObj { func: len });
  builtin_functions.insert(String::from("first"), Object::BuilinObj { func: first });
  builtin_functions.insert(String::from("last"), Object::BuilinObj { func: last });
  builtin_functions.insert(String::from("rest"), Object::BuilinObj { func: rest });
  builtin_functions.insert(String::from("push"), Object::BuilinObj { func: push });

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
    Object::ArrayObj(v) => Object::IntegerObj(v.len() as i64),
    _ => {
      return Object::ErrorObj(format!(
        "argument to 'len' not supported, got {}",
        args[0].type_name()
      ))
    }
  }
}

fn first(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return Object::ErrorObj(format!(
      "wrong number of arguments. got={}, want=1",
      args.len()
    ));
  }
  match &args[0] {
    Object::ArrayObj(v) => return v[0].clone(),
    _ => Object::ErrorObj(format!(
      "argument to 'first' must be ARRAY, got {}",
      args[0].type_name()
    )),
  }
}

fn last(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return Object::ErrorObj(format!(
      "wrong number of arguments. got={}, want=1",
      args.len()
    ));
  }
  match &args[0] {
    Object::ArrayObj(v) => return v[v.len() - 1].clone(),
    _ => Object::ErrorObj(format!(
      "argument to 'last' must be ARRAY, got {}",
      args[0].type_name()
    )),
  }
}

fn rest(args: Vec<Object>) -> Object {
  if args.len() != 1 {
    return Object::ErrorObj(format!(
      "wrong number of arguments. got={}, want=1",
      args.len()
    ));
  }
  match &args[0] {
    Object::ArrayObj(v) => {
      if v.len() == 0 {
        return Object::Null;
      }

      let mut new_v = Vec::new();
      for i in 1..v.len() {
        new_v.push(v[i].clone())
      }
      return Object::ArrayObj(new_v);
    }
    _ => Object::ErrorObj(format!(
      "argument to 'last' must be ARRAY, got {}",
      args[0].type_name()
    )),
  }
}

fn push(args: Vec<Object>) -> Object {
  if args.len() != 2 {
    return Object::ErrorObj(format!(
      "wrong number of arguments. got={}, want=2",
      args.len()
    ));
  }
  match &args[0] {
    Object::ArrayObj(v) => {
      let mut new_v = v.clone();
      new_v.push(args[1].clone());
      return Object::ArrayObj(new_v);
    }
    _ => Object::ErrorObj(format!(
      "argument to 'push' must be ARRAY, got {}",
      args[0].type_name()
    )),
  }
}
