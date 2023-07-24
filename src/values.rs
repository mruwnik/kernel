use std::sync::atomic::{AtomicUsize, Ordering};
use std::ops::Deref;
use std::rc::Rc;
use std::fmt;

use crate::values::bools::Bool;
use crate::values::combiners::Combiner;
use crate::values::constants::Constant;
use crate::values::envs::EnvRef;
use crate::values::numbers::Number;
use crate::values::pairs::PairRef;
use crate::values::strings::Str;
use crate::values::symbols::Symbol;
use crate::errors::RuntimeError;

pub mod bools;
pub mod constants;
pub mod combiners;
pub mod envs;
pub mod eval;
pub mod pairs;
pub mod numbers;
pub mod strings;
pub mod symbols;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Bool(Bool),
    Combiner(Combiner),
    Constant(Constant),
    Env(EnvRef),
    Number(Number),
    Pair(PairRef),
    String(Str),
    Symbol(Symbol),
}

pub type ValueResult = Result<Rc<Value>, RuntimeError>;

impl From<&Value> for Rc<Value> {
    fn from(val: &Value) -> Self {
        Rc::new(val.clone())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Bool(b) => b.to_string(),
            Value::Constant(c) => c.to_string(),
            Value::Combiner(c) => c.to_string(),
            Value::Env(e) => e.borrow().to_string(),
            Value::Pair(p) => p.borrow().to_string(),
            Value::Number(n) => n.to_string(),
            Value::Symbol(s) => s.to_string(),
            Value::String(s) => s.to_string(),
        })
    }
}

impl Value {
    fn ok(&self) -> ValueResult {
        Ok(self.into())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CallResultType {
    Call(Rc<Value>),
    Value(Rc<Value>),
}

pub type CallResult = Result<CallResultType, RuntimeError>;

impl CallResultType {
    pub fn is_true(&self) -> bool {
        match self {
            CallResultType::Call(_) => false,
            CallResultType::Value(v) => v.is_true(),
        }
    }
}

impl fmt::Display for CallResultType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            CallResultType::Call(c) => c.to_string(),
            CallResultType::Value(v) => v.to_string(),
        })
    }
}

impl From<CallResultType> for Rc<Value> {
    fn from(val: CallResultType) -> Rc<Value> {
        match val {
            CallResultType::Call(c) => c.clone(),
            CallResultType::Value(c) => c.clone(),
        }
    }
}

static ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
fn gen_sym() -> usize {
    ID_COUNTER.fetch_add(1, Ordering::SeqCst)
}


fn is_val(items: Rc<Value>, checker: &dyn Fn(Rc<Value>) -> bool) -> ValueResult {
    Value::boolean(
        match items.deref() {
            // this will return true when called without args
            Value::Constant(Constant::Null) => true,
            Value::Pair(_) => {
                let car: Rc<Value> = items.car()?.into();
                let cdr: Rc<Value> = items.cdr()?.into();
                checker(car) && is_val(cdr, checker)?.is_true()
            },
            _ => false,
        }
    ).ok()
}


#[cfg(test)]
mod tests {
    use yare::parameterized;
    use std::rc::Rc;

    use crate::values::{ Bool, Constant, Number, Str, Symbol, Value };
    use crate::values::envs::Env;

    // Make a vector of sample values, one of each kind
    pub fn sample_values() -> Vec<Rc<Value>> {
        vec![
            Rc::new(Value::Bool(Bool::True)),
            Rc::new(Value::Bool(Bool::False)),
            Value::new_applicative(
                "tester",
                &|_exprs, _env| Value::make_null().as_val(),
                Rc::new(Value::Constant(Constant::Null))
            ),
            Rc::new(Value::Constant(Constant::Ignore)),
            Rc::new(Value::Constant(Constant::Inert)),
            Rc::new(Value::Constant(Constant::Null)),
            Value::cons(
                Value::make_null(),
                Value::make_null(),
            ).unwrap().into(),
            Rc::new(Value::Env(Env::new(vec![]))),
            Rc::new(Value::Number(Number::Int(123))),
            Rc::new(Value::String(Str::new("bla"))),
            Rc::new(Value::Symbol(Symbol("bla".to_string()))),
        ]
    }

    #[parameterized(
        true_ = { Value::Bool(Bool::True), "#t" },
        fals_ = { Value::Bool(Bool::False), "#f" },
        ignore = { Value::Constant(Constant::Ignore), "#ignore" },
        inert = { Value::Constant(Constant::Inert), "#inert" },
        null = { Value::Constant(Constant::Null), "()" },
        env = { Value::Env(Env::new(vec![])), "#env" },
        int = { Value::Number(Number::Int(123)), "123" },
        float = { Value::Number(Number::Float(32.1)), "32.1" },
        // pairs are in their own module
        string = { Value::String(Str::new("bla")), "\"bla\"" },
        symbol = { Value::Symbol(Symbol("bla".to_string())), "bla" },
    )]
    fn test_format(val: Value, expected: &str) {
        assert_eq!(format!("{val}"), expected)
    }
}
