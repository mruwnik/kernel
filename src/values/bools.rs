use std::{fmt, rc::Rc, ops::Deref};
use crate::values::{ CallResult, Value, is_val };

#[derive(Debug, PartialEq, Eq)]
pub enum Bool {
    True, False
}

impl Value {
    // helpers
    pub fn boolean(val: bool) -> Rc<Value> {
        Rc::new(Value::Bool(
            if val {
                Bool::True
            } else {
                Bool::False
            }))
    }

    pub fn is_true(val: Rc<Value>) -> bool {
        val == Value::boolean(true)
    }

    // primatives
    pub fn is_boolean(items: Rc<Value>) -> CallResult {
        is_val(items, &|val| matches!(val.deref(), Value::Bool(_)))
    }

    pub fn is_eq(self: &Rc<Self>, other: &Rc<Self>) -> CallResult {
        Ok(Value::boolean(
            match (self.deref(), other.deref()) {
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Combiner(a), Value::Combiner(b)) => a.is_eq(b)?,
                (Value::Constant(a), Value::Constant(b)) => a.is_eq(b),
                (Value::Env(a), Value::Env(b)) => a.borrow().is_eq(b.clone()),
                (Value::Number(a), Value::Number(b)) => a.is_eq(b),
                (Value::Pair(a), Value::Pair(b)) => a.borrow().is_eq(b)?,
                (Value::Symbol(a), Value::Symbol(b)) => a.is_eq(b),
                (Value::String(a), Value::String(b)) => a.is_eq(b),
                _ => false
            }
        ))
    }

    pub fn is_equal(self: &Rc<Self>, other: &Rc<Self>) -> CallResult {
        Ok(Value::boolean(
            match (self.deref(), other.deref()) {
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Combiner(a), Value::Combiner(b)) => a.is_eq(b)?,
                (Value::Constant(a), Value::Constant(b)) => a.is_eq(b),
                (Value::Env(a), Value::Env(b)) => a.borrow().is_eq(b.clone()),
                (Value::Number(a), Value::Number(b)) => a.is_equal(b),
                (Value::Pair(a), Value::Pair(b)) => a.borrow().is_equal(b)?,
                (Value::Symbol(a), Value::Symbol(b)) => a.is_eq(b),
                (Value::String(a), Value::String(b)) => a.is_equal(b),
                _ => false
            }
        ))
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

    use std::ops::Deref;
    use std::rc::Rc;
    use crate::values::{ Value, tests::sample_values };

    #[test]
    fn test_is_boolean_single() {
        for val in sample_values() {
            let listified = Value::as_pair(val.clone());
            let is_type = Value::is_true(Value::is_boolean(listified).expect("ok"));
            match val.deref() {
                Value::Bool(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_boolean_multi() {
        for val in sample_values() {
            dbg!("val is");
            dbg!(val.clone());
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
            match val.deref() {
                Value::Bool(_) => assert!(Value::is_true(Value::is_boolean(listified).expect("ok"))),
                _ => assert!(!Value::is_true(Value::is_boolean(listified).expect("ok"))),
            }
        }
    }

    #[parameterized(
        true_ = { Value::boolean(true) },
        false_ = { Value::boolean(false) },
    )]
    fn test_is_eq_self(val: Rc<Value>) {
        assert_eq!(val.is_eq(&val).expect("ok"), Value::boolean(true));
    }

    #[parameterized(
        true_ = { true },
        false_ = { false },
    )]
    fn test_is_eq(val: bool) {
        let val1 = Value::boolean(val);
        let val2 = Value::boolean(val);

        assert_eq!(val1.is_eq(&val2).expect("ok"), Value::boolean(true));
        assert_eq!(val2.is_eq(&val1).expect("ok"), Value::boolean(true));
    }
}
