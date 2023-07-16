use std::rc::Rc;
use std::fmt;

use crate::values::envs::{Env, EnvRef};

pub mod bools;
pub mod envs;

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct Symbol(pub String);

#[derive(Debug, PartialEq)]
pub enum Value {
    Symbol(Symbol),
    Env(EnvRef),
    Bool(bool),
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Env(_) => "#env",
            Value::Symbol(s) => &s.0,
            Value::Bool(b) => match b {
                true => "#t",
                false => "#f",
            },
        })
    }
}


pub fn tester() {
    let ground_env = Env::new(vec![]);

    let parent1_1 = Env::new(vec![ground_env.clone()]);
    let parent1_2 = Env::new(vec![parent1_1.clone()]);
    let parent1_3 = Env::new(vec![parent1_2.clone()]);

    let env = Env::new(vec![parent1_3.clone()]);

    fn add(env: EnvRef, val: &str) {
        env.borrow_mut().bind(
            Symbol("key".to_string()),
            Rc::new(Value::Symbol(Symbol(val.to_string())))
        );
    }
    add(ground_env.clone(), "ground");
    add(parent1_1.clone(), "parent 1 1");
    // add(parent1_2.clone(), "parent 1 2");
    // add(parent1_3.clone(), "parent 1 3");
    // add(env.clone(), "env");

    dbg!(env.borrow().get(Symbol("key".to_string())));
    // dbg!(env);
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::values::{Value, Symbol, Env};

    #[parameterized(
        same = { Symbol("bla".to_string()), Symbol("bla".to_string()) },
        same_empty = { Symbol("".to_string()), Symbol("".to_string()) },
    )]
    fn test_same_symbol(sym1: Symbol, sym2: Symbol) {
        assert_eq!(sym1, sym2)
    }

    #[parameterized(
        diff1 = { Symbol("bla".to_string()), Symbol("blaaaa".to_string()) },
        diff2 = { Symbol("bLA".to_string()), Symbol("Bla".to_string()) },
        diff3 = { Symbol("a".to_string()), Symbol("".to_string()) },
    )]
    fn test_different_symbol(sym1: Symbol, sym2: Symbol) {
        assert_ne!(sym1, sym2)
    }

    #[parameterized(
        env = { Value::Env(Env::new(vec![])), "#env" },
        symbol = { Value::Symbol(Symbol("bla".to_string())), "bla" },
        true_ = { Value::Bool(true), "#t" },
        fals_ = { Value::Bool(false), "#f" },
    )]
    fn test_format(val: Value, expected: &str) {
        assert_eq!(format!("{val}"), expected)
    }
}
