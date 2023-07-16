use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;
use std::fmt;

use crate::values::envs::{Env, EnvRef};
use crate::values::symbols::Symbol;
use crate::values::strings::Str;
use crate::values::bools::Bool;

pub mod bools;
pub mod envs;
pub mod symbols;
pub mod strings;

#[derive(Debug, PartialEq)]
pub enum Value {
    Bool(Bool),
    Env(EnvRef),
    String(Str),
    Symbol(Symbol),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Env(e) => e.borrow().to_string(),
            Value::Symbol(s) => s.to_string(),
            Value::String(s) => s.to_string(),
            Value::Bool(b) => b.to_string(),
        })
    }
}

static ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
fn gen_sym() -> usize {
    ID_COUNTER.fetch_add(1, Ordering::SeqCst)
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

    let sym = Str::new("");
    let sym2 = Str::new("");
    dbg!(sym.0.as_ptr(), sym2.0.as_ptr());
    // dbg!(val);
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::values::{Value, Symbol, Env, Str, Bool};

    #[parameterized(
        env = { Value::Env(Env::new(vec![])), "#env" },
        symbol = { Value::Symbol(Symbol("bla".to_string())), "bla" },
        string = { Value::String(Str::new("bla")), "\"bla\"" },
        true_ = { Value::Bool(Bool::True), "#t" },
        fals_ = { Value::Bool(Bool::False), "#f" },
    )]
    fn test_format(val: Value, expected: &str) {
        assert_eq!(format!("{val}"), expected)
    }
}
