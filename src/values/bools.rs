use std::{rc::Rc, ops::Deref};
use crate::values::Value;

impl Value {
    pub fn boolean(val: bool) -> Rc<Value> {
        Rc::new(Value::Bool(val))
    }

    pub fn is_bool(&self) -> Rc<Self> {
        Rc::new(Value::Bool(matches!(self, Value::Bool(_))))
    }

    pub fn is_eq(self: &Rc<Self>, other: &Rc<Self>) -> Rc<Self> {
        Value::boolean(
            match (self.deref(), other.deref()) {
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Env(a), Value::Env(b)) => a == b,
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                _ => false
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use crate::values::{Value, Env, Symbol};

    fn sample_values() -> Vec<Value> {
        vec![
            Value::Symbol(Symbol("bla".to_string())),
            Value::Env(Env::new(vec![])),
            Value::Bool(true),
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
