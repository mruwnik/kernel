use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct Symbol(pub String);

#[derive(Debug, PartialEq)]
pub struct Env {
    bindings: HashMap<Symbol, Value>,
    parents: Vec<Rc<RefCell<Env>>>,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Symbol(Symbol),
    Env(Env),
}


impl Env {
    pub fn new(parents: Vec<Rc<RefCell<Env>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Env { bindings: HashMap::new(), parents }))
    }
    pub fn bind(&mut self, symbol: Symbol, value: Value) {
        self.bindings.insert(symbol, value);
    }
    pub fn get(&self, symbol: Symbol) -> Option<&Value> {
        self.bindings.get(&symbol)
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;
    use crate::values::{Value, Env, Symbol};

    #[test]
    fn test_make_env_is_empty() {
        let env = Env::new(vec![]);
        assert_eq!(env.borrow().bindings, HashMap::new());
        assert_eq!(env.borrow().parents, vec![]);
    }

    #[test]
    fn test_make_env_bind() {
        let env = Env::new(vec![]);
        let key1 = Symbol("key 1".to_string());
        let key2 = Symbol("key 2".to_string());
        let key3 = Symbol("key 3".to_string());
        env.borrow_mut().bind(key1.clone(), Value::Symbol(Symbol("value 1".to_string())));
        env.borrow_mut().bind(key2.clone(), Value::Symbol(Symbol("value 2".to_string())));
        env.borrow_mut().bind(key3.clone(), Value::Symbol(Symbol("value 3".to_string())));

        let mut map = HashMap::new();
        map.insert(key1, Value::Symbol(Symbol("value 1".to_string())));
        map.insert(key2, Value::Symbol(Symbol("value 2".to_string())));
        map.insert(key3, Value::Symbol(Symbol("value 3".to_string())));

        assert_eq!(env.borrow().bindings, map);
    }

    #[test]
    fn test_make_env_bind_replace() {
        let env = Env::new(vec![]);
        let key = Symbol("key".to_string());
        env.borrow_mut().bind(key.clone(), Value::Symbol(Symbol("value 1".to_string())));
        env.borrow_mut().bind(key.clone(), Value::Symbol(Symbol("value 2".to_string())));
        env.borrow_mut().bind(key.clone(), Value::Symbol(Symbol("value 3".to_string())));

        let mut map = HashMap::new();
        map.insert(key, Value::Symbol(Symbol("value 3".to_string())));

        assert_eq!(env.borrow().bindings, map);
    }

    #[test]
    fn test_make_env_get() {
        let env = Env::new(vec![]);
        let key1 = Symbol("key 1".to_string());
        let key2 = Symbol("key 2".to_string());
        let key3 = Symbol("key 3".to_string());
        env.borrow_mut().bind(key1.clone(), Value::Symbol(Symbol("value 1".to_string())));
        env.borrow_mut().bind(key2.clone(), Value::Symbol(Symbol("value 2".to_string())));
        env.borrow_mut().bind(key3.clone(), Value::Symbol(Symbol("value 3".to_string())));

        assert_eq!(
            *env.borrow().get(key2.clone()).ok_or("asd").unwrap(),
            Value::Symbol(Symbol("value 2".to_string()))
        );
    }
}
