use std::rc::Rc;
use std::ops::Deref;
use std::fmt;
use crate::values::{ Value, CallResult, is_val };

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
    pub fn is_number(items: Rc<Value>) -> CallResult {
        is_val(items, &|val| matches!(val.deref(), Value::Number(_)))
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

    use std::ops::Deref;
    use crate::values::{ Value, tests::sample_values };
    use crate::values::numbers::Number;

    #[test]
    fn test_is_number() {
        for val in sample_values() {
            let listified = Value::as_pair(val.clone());
            let is_type = Value::is_true(Value::is_number(listified.clone()).expect("ok"));
            match val.deref() {
                Value::Number(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_number_multi() {
        for val in sample_values() {
            let listified = Value::cons(
                val.clone(),
                Value::cons(
                    val.clone(),
                    Value::cons(
                        val.clone(),
                        Value::as_pair(val.clone())
                    ).unwrap(),
                ).unwrap(),
            ).unwrap();
            let is_type = Value::is_true(Value::is_number(listified).expect("ok"));
            match val.deref() {
                Value::Number(_) => assert!(is_type),
                _ => assert!(!is_type),
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
