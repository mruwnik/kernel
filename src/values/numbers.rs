use std::fmt;
use crate::values::{ Value, CallResult };

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Number::Int(n) => n.to_string(),
            Number::Float(n) => n.to_string(),
        })
    }
}

impl Value {
    pub fn is_number(&self) -> CallResult {
        Ok(Value::boolean(matches!(self, Value::Number(_))))
    }
}

impl Number {
    pub fn is_eq(self: &Self, other: &Self) -> bool {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => a == b,
            (Number::Float(a), Number::Float(b)) => a == b,
            _ => false
        }
    }

    pub fn is_equal(self: &Self, other: &Self) -> bool {
        self.eq(other)
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use crate::values::{ Bool, Env, Constant, Pair, Str, Symbol, Value };
    use crate::values::numbers::Number;

    fn sample_values() -> Vec<Value> {
        vec![
            Value::Bool(Bool::True),
            Value::Constant(Constant::Ignore),
            Value::Constant(Constant::Inert),
            Value::Constant(Constant::Null),
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null)),
                true
            )),
            Value::Env(Env::new(vec![])),
            Value::Number(Number::Int(123)),
            Value::String(Str::new("bla")),
            Value::Symbol(Symbol("bla".to_string())),
        ]
    }

    #[test]
    fn test_is_number() {
        for val in sample_values() {
            match val {
                Value::Number(_) => assert_eq!(val.is_number().expect("ok"), Value::boolean(true)),
                _ => assert_eq!(val.is_number().expect("ok"), Value::boolean(false)),
            }
        }
    }

    #[parameterized(
        int = { Number::Int(123) },
        float = { Number::Float(12.32) },
    )]
    fn test_is_eq_self(val: Number) {
        assert!(val.is_eq(&val));
    }

    #[parameterized(
        same_ints = { Number::Int(123), Number::Int(123) },
        same_floats = { Number::Float(432.1), Number::Float(432.1) },
    )]
    fn test_is_eq(val1: Number, val2: Number) {
        assert!(val1.is_eq(&val2));
        assert!(val2.is_eq(&val1));
    }

    #[parameterized(
        different_ints = { Number::Int(123), Number::Int(321) },
        different_floats = { Number::Float(32.1), Number::Float(432.1) },
        different_nums = { Number::Float(32.0), Number::Int(12) },
        different_nums2 = { Number::Int(43), Number::Float(12.0) },
        same_nums = { Number::Float(12.0), Number::Int(12) },
        same_nums2 = { Number::Int(12), Number::Float(12.0) },
    )]
    fn test_is_eq_not(val1: Number, val2: Number) {
        assert!(!val1.is_eq(&val2));
        assert!(!val2.is_eq(&val1));
    }
}
