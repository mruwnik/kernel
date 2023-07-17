use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;
use std::fmt;

use crate::values::bools::Bool;
use crate::values::constants::Constant;
use crate::values::envs::{Env, EnvRef};
use crate::values::numbers::Number;
use crate::values::pairs::{Pair, PairRef};
use crate::values::strings::Str;
use crate::values::symbols::Symbol;
use crate::errors::RuntimeError;

pub mod bools;
pub mod constants;
pub mod envs;
pub mod pairs;
pub mod numbers;
pub mod strings;
pub mod symbols;

#[derive(Debug, PartialEq)]
pub enum Value {
    Bool(Bool),
    Env(EnvRef),
    Constant(Constant),
    Number(Number),
    Pair(PairRef),
    String(Str),
    Symbol(Symbol),
}
type CallResult = Result<Rc<Value>, RuntimeError>;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Bool(b) => b.to_string(),
            Value::Constant(c) => c.to_string(),
            Value::Env(e) => e.borrow().to_string(),
            Value::Pair(p) => p.borrow().to_string(),
            Value::Number(n) => n.to_string(),
            Value::Symbol(s) => s.to_string(),
            Value::String(s) => s.to_string(),
        })
    }
}

static ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
fn gen_sym() -> usize {
    ID_COUNTER.fetch_add(1, Ordering::SeqCst)
}

pub fn tester() {
    // let ground_env = Env::new(vec![]);

    // let parent1_1 = Env::new(vec![ground_env.clone()]);
    // let parent1_2 = Env::new(vec![parent1_1.clone()]);
    // let parent1_3 = Env::new(vec![parent1_2.clone()]);

    // let env = Env::new(vec![parent1_3.clone()]);

    // fn add(env: EnvRef, val: &str) {
    //     env.borrow_mut().bind(
    //         Symbol("key".to_string()),
    //         Rc::new(Value::Symbol(Symbol(val.to_string())))
    //     );
    // }
    // add(ground_env.clone(), "ground");
    // add(parent1_1.clone(), "parent 1 1");
    // add(parent1_2.clone(), "parent 1 2");
    // add(parent1_3.clone(), "parent 1 3");
    // add(env.clone(), "env");

    // let pair = Pair::new(
    //     Rc::new(Value::Number(Number::Int(1))),
    //     Rc::new(Value::Constant(Constant::Null))
    // );
    // let pair2 = Pair::new(
    //     Rc::new(Value::Number(Number::Int(1))),
    //     Rc::new(Value::Number(Number::Int(2))),
    // );
    // let pair3 = Pair::new(
    //     Rc::new(Value::Number(Number::Int(1))),
    //     Rc::new(Value::Pair(Pair::new(
    //         Rc::new(Value::Number(Number::Int(2))),
    //         Rc::new(Value::Constant(Constant::Null))
    //     )))
    // );
    // println!("pair {pair}");
    // println!("pair2 {pair2}");
    // println!("pair3 {pair3}");
    // dbg!(pair.eq(&pair2));
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::values::{ Bool, Constant, Env, Number, Str, Symbol, Value };

    #[parameterized(
        true_ = { Value::Bool(Bool::True), "#t" },
        fals_ = { Value::Bool(Bool::False), "#f" },
        ignore = { Value::Constant(Constant::Ignore), "#ignore" },
        inert = { Value::Constant(Constant::Inert), "#inert" },
        null = { Value::Constant(Constant::Null), "()" },
        env = { Value::Env(Env::new(vec![])), "#env" },
        int = { Value::Number(Number::Int(123)), "123" },
        float = { Value::Number(Number::Float(32.1)), "32.1" },

        string = { Value::String(Str::new("bla")), "\"bla\"" },
        symbol = { Value::Symbol(Symbol("bla".to_string())), "bla" },
    )]
    fn test_format(val: Value, expected: &str) {
        assert_eq!(format!("{val}"), expected)
    }
}
