use std::{fmt, ops::Deref};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::{
    values::{Symbol, Value, ValueResult, Constant, Combiner, gen_sym, is_val},
    errors::{ RuntimeError, ErrorTypes },
    values::eval::eval,
};

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

    /// Set (mutate) an existing binding, searching through parent environments
    /// Returns true if the binding was found and updated, false otherwise
    pub fn set_existing(&mut self, symbol: &Symbol, value: Rc<Value>) -> bool {
        if self.bindings.contains_key(symbol) {
            self.bindings.insert(symbol.clone(), value);
            true
        } else {
            for parent in self.parents.iter() {
                if parent.borrow_mut().set_existing(symbol, value.clone()) {
                    return true;
                }
            }
            false
        }
    }

    pub fn is_eq(&self, other: EnvRef) -> bool {
        *self == *other.borrow()
    }

    pub fn set(&mut self, formals: Rc<Value>, vals_expr: Rc<Value>) -> ValueResult {
        match (formals.deref(), vals_expr.deref()) {
            (Value::Constant(Constant::Null), Value::Constant(Constant::Null)) => (),
            (Value::Constant(Constant::Ignore), _) => (),
            (Value::Symbol(s), v) => self.bind(s.clone(), v.into()),
            (Value::Pair(_), Value::Pair(_)) => {
                self.set(formals.car()?, vals_expr.car()?)?;
                self.set(formals.cdr()?, vals_expr.cdr()?)?;
            },
            _ => return RuntimeError::type_error(format!("Invalid parameter tree provided - got {formals} and {vals_expr}")),
        }
        Value::make_const(Constant::Inert).ok()
    }

    /// Mutate an existing binding, searching parent chain. Used by $set!
    pub fn mutate(&mut self, formals: Rc<Value>, vals_expr: Rc<Value>) -> ValueResult {
        match (formals.deref(), vals_expr.deref()) {
            (Value::Constant(Constant::Null), Value::Constant(Constant::Null)) => (),
            (Value::Constant(Constant::Ignore), _) => (),
            (Value::Symbol(s), v) => {
                if !self.set_existing(s, v.into()) {
                    return RuntimeError::type_error(format!("$set!: symbol {} is not bound", s.0));
                }
            },
            (Value::Pair(_), Value::Pair(_)) => {
                self.mutate(formals.car()?, vals_expr.car()?)?;
                self.mutate(formals.cdr()?, vals_expr.cdr()?)?;
            },
            _ => return RuntimeError::type_error(format!("Invalid parameter tree provided - got {formals} and {vals_expr}")),
        }
        Value::make_const(Constant::Inert).ok()
    }
}

impl Value {
    pub fn is_env(items: Rc<Value>) -> ValueResult {
        is_val(items, &|val| matches!(val.deref(), Value::Env(_)))
    }

    pub fn define(&self, formals: Rc<Value>, vals_expr: Rc<Value>) -> ValueResult {
        if let Value::Env(e) = self {
            let vals = eval(vals_expr, self.into())?;
            e.deref().borrow_mut().set(formals, vals)
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "$define! requires an env as its first argument"))
        }
    }

    pub fn assign(&self, formals: Rc<Value>, vals_expr: Rc<Value>) -> ValueResult {
        if let Value::Env(e) = self {
            let vals = eval(vals_expr, self.into())?;
            e.deref().borrow_mut().mutate(formals, vals)
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "$set! requires an env as its first argument"))
        }
    }

    pub fn make_environment(val: Rc<Value>) -> ValueResult {
        match val.deref() {
            Value::Constant(Constant::Null) => Value::make_environment(Value::ground_env().as_pair()),
            Value::Pair(_) => {
                let mut parents: Vec<EnvRef> = Vec::new();
                for (i, node) in val.iter().enumerate() {
                    match node.deref() {
                        Value::Env(env) => parents.push(env.clone()),
                        Value::Constant(Constant::Null) => (),
                        _ => return RuntimeError::type_error(format!("argument {i} to make-environment is not an Environment")),
                    }
                }
                Value::Env(Env::new(parents)).ok()
            },
            _ => Err(RuntimeError::new(ErrorTypes::TypeError, "make-environment expects a list of parents"))
        }
    }

    pub fn ground_env() -> Rc<Value> {
        let env = Env::new(vec![]);
        Combiner::bind_ground(&env);
        Rc::new(Value::Env(env))
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
    use std::ops::Deref;
    use std::collections::HashMap;
    use crate::errors::RuntimeError;
    use crate::values::{ Constant, Number, Symbol, Value, tests::sample_values };
    use crate::values::envs::Env;
    use yare::parameterized;

    use super::EnvRef;

    fn make_value(string: &str) -> Rc<Value> {
        Rc::new(Value::Symbol(Symbol(string.to_string())))
    }
    fn add(env: &EnvRef, key: &str, val: &str) {
        env.borrow_mut().bind(Symbol(key.to_string()), make_value(val));
    }

    #[test]
    fn test_is_env() {
        for val in sample_values() {
            let listified = val.as_pair();
            let is_type = Value::is_env(listified).expect("ok").is_true();
            match val.deref() {
                Value::Env(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_env_multi() {
        for val in sample_values() {
            let listified = Value::cons(
                val.clone(),
                Value::cons(
                    val.clone(),
                    Value::cons(
                        val.clone(),
                        val.as_pair()
                    ).unwrap().into(),
                ).unwrap().into(),
            ).unwrap().into();
            let is_type = Value::is_env(listified).expect("ok").is_true();
            match val.deref() {
                Value::Env(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
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

    #[parameterized(
        empty_envs = { Env::new(vec![]), Env::new(vec![]) },
        with_parents = { Env::new(vec![Env::new(vec![])]), Env::new(vec![]) },
    )]
    fn test_is_not_eq(env1: EnvRef, env2: EnvRef) {
        let val1 = Rc::new(Value::Env(env1));
        let val2 = Rc::new(Value::Env(env2));

        assert!(!val1.clone().is_eq(val2.clone()).expect("ok").is_true());
        assert!(!val2.is_eq(val1).expect("ok").is_true());
    }

    #[parameterized(
        empty_env = { Env::new(vec![]) },
        with_parent = { Env::new(vec![Env::new(vec![])]) },
    )]
    fn test_is_eq(env: EnvRef) {
        let val = Rc::new(Value::Env(env));
        assert!(val.clone().is_eq(val).expect("ok").is_true());
    }

    #[parameterized(
        with_parents = { vec![Env::new(vec![Env::new(vec![])]), Env::new(vec![])] },
    )]
    fn test_make_env(parents: Vec<EnvRef>) {
        let root = Value::cons(
            Value::make_const(Constant::Null),
            Value::make_const(Constant::Null),
        ).unwrap();

        let mut prev = root.clone();
        for parent in parents.iter() {
            let next = Value::cons(
                Rc::new(Value::Env(parent.clone())),
                Value::make_const(Constant::Null),
            ).unwrap();
            Value::set_cdr(prev.into(), next.clone().into()).expect("This should work");
            prev = next;
        }

        let env: Rc<Value> = Value::make_environment(root.cdr().unwrap().into()).expect("invalid parents provided?").into();
        assert!(Value::is_env(env.as_pair()).unwrap().is_true());
        match env.deref() {
            Value::Env(e) => assert_eq!(e.borrow().parents, parents),
            _ => panic!("Make environment didn't return an env"),
        }
    }

    #[test]
    fn test_make_env_empty() {
        let ground = Value::ground_env();
        let env: Rc<Value> = Value::make_environment(Value::make_null()).expect("invalid parents provided?").into();
        assert!(Value::is_env(env.as_pair()).unwrap().is_true());
        match (ground.deref(), env.deref()) {
            (Value::Env(g), Value::Env(e)) => assert_eq!(e.borrow().parents[0].borrow().bindings, g.clone().borrow().bindings),
            _ => panic!("Make environment didn't return an env"),
        }
    }

    #[test]
    fn test_make_env_fails() {
        let parents = Value::to_list(vec![
            Rc::new(Value::Env(Env::new(vec![]))),
            Value::make_string("bla"),
            Rc::new(Value::Env(Env::new(vec![]))),
            Value::make_string("bla"),
            Value::make_string("bla"),
        ]).unwrap();
        let err = Value::make_environment(parents);
        assert_eq!(err, RuntimeError::type_error("argument 1 to make-environment is not an Environment"));
    }

    #[parameterized(
        null = { Value::make_null(), Value::make_null(), vec![] },
        ignore = { Value::make_ignore(), Value::make_ignore(), vec![] },
        symbol = { Value::make_symbol("bla"), Number::int(1), vec![(Symbol("bla".to_string()), Number::int(1))] },
        pair = {
            Value::cons(
                Value::make_symbol("bla"),
                Value::make_symbol("ble")
            ).unwrap(),
            Value::cons(
                Number::int(1),
                Number::int(2),
            ).unwrap(),
            vec![
                (Symbol("bla".to_string()), Number::int(1)),
                (Symbol("ble".to_string()), Number::int(2)),
            ]
        },
        nested = {
            Value::cons(
                Value::cons(
                    Value::make_symbol("sym1"),
                    Value::cons(
                        Value::make_symbol("sym2"),
                        Value::make_symbol("sym3")
                    ).unwrap()
                ).unwrap(),
                Value::cons(
                    Value::cons(
                        Value::make_symbol("sym4"),
                        Value::make_null()
                    ).unwrap(),
                    Value::make_symbol("sym5")
                ).unwrap()
            ).unwrap(),
            Value::cons(
                Value::cons(
                    Number::int(1),
                    Value::cons(
                        Number::int(2),
                        Number::int(3),
                    ).unwrap(),
                ).unwrap(),
                Value::cons(
                    Value::cons(
                        Number::int(4),
                        Value::make_null()
                    ).unwrap(),
                    Number::int(5),
                ).unwrap()
            ).unwrap(),
            vec![
                (Symbol("sym1".to_string()), Number::int(1)),
                (Symbol("sym2".to_string()), Number::int(2)),
                (Symbol("sym3".to_string()), Number::int(3)),
                (Symbol("sym4".to_string()), Number::int(4)),
                (Symbol("sym5".to_string()), Number::int(5)),
            ]
        },
    )]
    fn test_env_set(defined: Rc<Value>, exprs: Rc<Value>, expected: Vec<(Symbol, Rc<Value>)>) {
        let env = Env::new(vec![]);
        assert_eq!(env.deref().borrow_mut().set(defined, exprs).unwrap(), Value::make_inert());

        let mut bindings = HashMap::new();
        for (symbol, value) in expected {
            bindings.insert(symbol, value);
        }

        assert_eq!(env.borrow().bindings, bindings);
    }
}
