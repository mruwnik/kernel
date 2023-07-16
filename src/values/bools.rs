use std::{fmt, rc::Rc, ops::Deref};
use crate::values::Value;

#[derive(Debug, PartialEq, Eq)]
pub enum Bool {
    True, False
}

impl Value {
    pub fn boolean(val: bool) -> Rc<Value> {
        Rc::new(Value::Bool(
            if val {
                Bool::True
            } else {
                Bool::False
            }))
    }

    pub fn is_bool(&self) -> Rc<Self> {
        Value::boolean(matches!(self, Value::Bool(_)))
    }

    pub fn is_eq(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Value::boolean(
            match (self.deref(), other.deref()) {
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Env(a), Value::Env(b)) => a.borrow().is_eq(b.clone()),
                (Value::Symbol(a), Value::Symbol(b)) => a.is_eq(b),
                (Value::String(a), Value::String(b)) => a.is_eq(b),
                _ => false
            }
        )
    }

    pub fn is_equal(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Value::boolean(
            match (self.deref(), other.deref()) {
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Env(a), Value::Env(b)) => a.borrow().is_eq(b.clone()),
                (Value::Symbol(a), Value::Symbol(b)) => a.is_eq(b),
                (Value::String(a), Value::String(b)) => a.is_equal(b),
                _ => false
            }
        )
    }
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Bool::True => "#t",
            Bool::False => "#f",
        })
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use crate::values::{Value, Env, Symbol, Str, Bool, Number};

    fn sample_values() -> Vec<Value> {
        vec![
            Value::Bool(Bool::True),
            Value::Env(Env::new(vec![])),
            Value::Number(Number::Int(123)),
            Value::String(Str::new("bla")),
            Value::Symbol(Symbol("bla".to_string())),
        ]
    }

    #[test]
    fn test_is_bool() {
        for val in sample_values() {
            match val {
                Value::Bool(_) => assert_eq!(val.is_bool(), Value::boolean(true)),
                _ => assert_eq!(val.is_bool(), Value::boolean(false)),
            }
        }
    }

    #[parameterized(
        true_ = { Value::boolean(true) },
        false_ = { Value::boolean(false) },
    )]
    fn test_is_eq_self(val: Rc<Value>) {
        assert_eq!(val.is_eq(&val), Value::boolean(true));
    }

    #[parameterized(
        true_ = { true },
        false_ = { false },
    )]
    fn test_is_eq(val: bool) {
        let val1 = Value::boolean(val);
        let val2 = Value::boolean(val);

        assert_eq!(val1.is_eq(&val2), Value::boolean(true));
        assert_eq!(val2.is_eq(&val1), Value::boolean(true));
    }
}
