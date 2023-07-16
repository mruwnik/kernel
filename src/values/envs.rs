use std::fmt;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::values::{Symbol, Value, gen_sym};

#[derive(Debug, PartialEq)]
pub struct Env {
    bindings: HashMap<Symbol, Rc<Value>>,
    parents: Vec<EnvRef>,
    id: usize,
}
pub type EnvRef = Rc<RefCell<Env>>;


impl Env {
    pub fn new(parents: Vec<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Env {
            bindings: HashMap::new(),
            id: gen_sym(),
            parents
        }))
    }
    pub fn bind(&mut self, symbol: Symbol, value: Rc<Value>) {
        self.bindings.insert(symbol, value);
    }
    pub fn get(&self, symbol: Symbol) -> Option<Rc<Value>> {
        if let Some(val) = self.bindings.get(&symbol) {
            Some(val.clone())
        } else {
            for parent in self.parents.iter() {
                if let Some(v) = parent.borrow().get(symbol.clone()) {
                    return Some(v.clone());
                };
            }
            None
        }
    }

    pub fn is_eq(&self, other: EnvRef) -> bool {
        *self == *other.borrow()
    }
}

impl Value {
    pub fn is_env(&self) -> Rc<Self> {
        Value::boolean(matches!(self, Value::Env(_)))
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#env")
    }
}


#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use std::collections::HashMap;
    use crate::values::{Value, Symbol, Str, Bool};
    use crate::values::envs::Env;
    use yare::parameterized;

    use super::EnvRef;

    fn make_value(string: &str) -> Rc<Value> {
        Rc::new(Value::Symbol(Symbol(string.to_string())))
    }
    fn add(env: &EnvRef, key: &str, val: &str) {
        env.borrow_mut().bind(Symbol(key.to_string()), make_value(val));
    }

    // Make a vector of sample values, one of each kind
    fn sample_values() -> Vec<Value> {
        vec![
            Value::Symbol(Symbol("bla".to_string())),
            Value::Env(Env::new(vec![])),
            Value::Bool(Bool::True),
            Value::String(Str::new("bla")),
        ]
    }

    #[test]
    fn test_make_env_is_empty() {
        let env = Env::new(vec![]);
        assert_eq!(env.borrow().bindings, HashMap::new());
        assert_eq!(env.borrow().parents, vec![]);
    }

    #[test]
    fn test_make_env_bind() {
        let env = Env::new(vec![]);
        add(&env, "key 1", "value 1");
        add(&env, "key 2", "value 2");
        add(&env, "key 3", "value 3");

        let mut map = HashMap::new();
        map.insert(Symbol("key 1".to_string()), make_value("value 1"));
        map.insert(Symbol("key 2".to_string()), make_value("value 2"));
        map.insert(Symbol("key 3".to_string()), make_value("value 3"));

        assert_eq!(env.borrow().bindings, map);
    }

    #[test]
    fn test_make_env_bind_replace() {
        let env = Env::new(vec![]);
        let key = Symbol("key".to_string());
        add(&env, "key", "value 1");
        add(&env, "key", "value 2");
        add(&env, "key", "value 3");

        let mut map = HashMap::new();
        map.insert(key, make_value("value 3"));

        assert_eq!(env.borrow().bindings, map);
    }

    #[test]
    fn test_make_env_basic_get() {
        let env = Env::new(vec![]);
        add(&env, "key 1", "value 1");
        add(&env, "key 2", "value 2");
        add(&env, "key 3", "value 3");

        let key2 = Symbol("key 2".to_string());
        assert_eq!(
            env.borrow().get(key2.clone()).ok_or("asd").unwrap(),
            make_value("value 2")
        );
    }

    fn get_key(env: &EnvRef, key: &str) -> Rc<Value> {
        env.borrow().get(Symbol(key.to_string())).unwrap()
    }

    #[test]
    fn test_make_env_basic_get_vertical_parents() {
        let ground_env = Env::new(vec![]);

        let parent1_1 = Env::new(vec![ground_env.clone()]);
        let parent1_2 = Env::new(vec![parent1_1.clone()]);
        let parent1_3 = Env::new(vec![parent1_2.clone()]);

        let env = Env::new(vec![parent1_3.clone()]);

        add(&ground_env, "key", "ground");
        assert_eq!(get_key(&env, "key"), make_value("ground"));

        add(&parent1_1, "key", "parent 1 1");
        assert_eq!(get_key(&env, "key"), make_value("parent 1 1"));

        add(&parent1_2, "key", "parent 1 2");
        assert_eq!(get_key(&env, "key"), make_value("parent 1 2"));

        add(&parent1_3, "key", "parent 1 3");
        assert_eq!(get_key(&env, "key"), make_value("parent 1 3"));

        add(&env, "key", "env");
        assert_eq!(get_key(&env, "key"), make_value("env"));
    }

    #[test]
    fn test_make_env_basic_get_horizontal_parents() {
        let ground_env = Env::new(vec![]);

        let parent1 = Env::new(vec![ground_env.clone()]);
        let parent2 = Env::new(vec![ground_env.clone()]);
        let parent3 = Env::new(vec![ground_env.clone()]);

        let env = Env::new(vec![parent1.clone(), parent2.clone(), parent3.clone()]);

        add(&parent3, "key", "parent3");
        assert_eq!(get_key(&env, "key"), make_value("parent3"));

        add(&parent2, "key", "parent2");
        assert_eq!(get_key(&env, "key"), make_value("parent2"));

        add(&parent1, "key", "parent1");
        assert_eq!(get_key(&env, "key"), make_value("parent1"));

        add(&env, "key", "env");
        assert_eq!(get_key(&env, "key"), make_value("env"));
    }

    #[test]
    fn test_make_env_basic_get_mixed_parents() {
        let ground_env = Env::new(vec![]);

        let parent1_1 = Env::new(vec![ground_env.clone()]);
        let parent1_2 = Env::new(vec![parent1_1.clone()]);
        let parent1_3 = Env::new(vec![parent1_2.clone()]);

        let parent2_1 = Env::new(vec![ground_env.clone()]);
        let parent2_2 = Env::new(vec![parent2_1.clone()]);
        let parent2_3 = Env::new(vec![parent2_2.clone()]);

        let parent3_1 = Env::new(vec![ground_env.clone()]);
        let parent3_2 = Env::new(vec![parent3_1.clone()]);
        let parent3_3 = Env::new(vec![parent3_2.clone()]);


        let env = Env::new(vec![parent1_3.clone(), parent2_3.clone(), parent3_3.clone()]);

        add(&parent3_1, "key", "parent3_1");
        assert_eq!(get_key(&env, "key"), make_value("parent3_1"));

        add(&parent3_3, "key", "parent3_3");
        assert_eq!(get_key(&env, "key"), make_value("parent3_3"));

        add(&parent2_2, "key", "parent2_2");
        assert_eq!(get_key(&env, "key"), make_value("parent2_2"));

        add(&parent1_1, "key", "parent1_1");
        assert_eq!(get_key(&env, "key"), make_value("parent1_1"));

        add(&env, "key", "env");
        assert_eq!(get_key(&env, "key"), make_value("env"));
    }

    #[test]
    fn test_is_env() {
        for val in sample_values() {
            match val {
                Value::Env(_) => assert_eq!(val.is_env(), Value::boolean(true)),
                _ => assert_eq!(val.is_env(), Value::boolean(false)),
            }
        }
    }

    #[parameterized(
        empty_envs = { Env::new(vec![]), Env::new(vec![]) },
        with_parents = { Env::new(vec![Env::new(vec![])]), Env::new(vec![]) },
    )]
    fn test_is_not_eq(env1: EnvRef, env2: EnvRef) {
        let val1 = Rc::new(Value::Env(env1));
        let val2 = Rc::new(Value::Env(env2));

        assert_eq!(val1.is_eq(&val2), Value::boolean(false));
        assert_eq!(val2.is_eq(&val1), Value::boolean(false));
    }

    #[parameterized(
        empty_env = { Env::new(vec![]) },
        with_parent = { Env::new(vec![Env::new(vec![])]) },
    )]
    fn test_is_eq(env: EnvRef) {
        let val = Rc::new(Value::Env(env));
        assert_eq!(val.is_eq(&val), Value::boolean(true));
    }
}
