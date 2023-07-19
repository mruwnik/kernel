use std::{fmt, ops::Deref};
use std::rc::Rc;
use crate::values::{ Value, ValueResult, is_val };

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Constant {
    Ignore,
    Inert,
    Null,
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Constant::Ignore => "#ignore",
            Constant::Inert => "#inert",
            Constant::Null => "()"
        })
    }
}

impl Value {
    pub fn is_inert(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Constant(Constant::Inert)))
    }

    pub fn is_ignore(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Constant(Constant::Ignore)))
    }

    pub fn is_null(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Constant(Constant::Null)))
    }

    pub fn make_const(c: Constant) -> Rc<Value> {
        Rc::new(Value::Constant(c))
    }

    pub fn make_null() -> Rc<Value> {
        Rc::new(Value::Constant(Constant::Null))
    }

    pub fn make_ignore() -> Rc<Value> {
        Rc::new(Value::Constant(Constant::Ignore))
    }

    pub fn make_inert() -> Rc<Value> {
        Rc::new(Value::Constant(Constant::Inert))
    }
}

impl Constant {
    pub fn is_eq(self: &Self, other: &Self) -> bool {
        self == other
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::ops::Deref;
    use crate::values::{ Value, tests::sample_values };
    use crate::values::constants::Constant;

    #[test]
    fn test_is_constant() {
        for val in sample_values() {
            let listified = val.as_pair();
            match val.deref() {
                Value::Constant(Constant::Inert) => assert!(Value::is_inert(listified).expect("ok").is_true()),
                Value::Constant(Constant::Ignore) => assert!(Value::is_ignore(listified).expect("ok").is_true()),
                Value::Constant(Constant::Null) => assert!(Value::is_null(listified).expect("ok").is_true()),
                _ => {
                    assert!(!Value::is_inert(listified.clone()).expect("ok").is_true());
                    assert!(!Value::is_ignore(listified.clone()).expect("ok").is_true());
                    assert!(!Value::is_null(listified.clone()).expect("ok").is_true());
                },
            }
        }
    }

    #[test]
    fn test_is_constant_multi() {
        for val in sample_values() {
            let listified = Value::cons(
                val.clone(),
                Value::cons(
                    val.clone(),
                    Value::cons(
                        val.clone(),
                        val.as_pair(),
                    ).unwrap().into(),
                ).unwrap().into(),
            ).unwrap().into();
            match val.deref() {
                Value::Constant(Constant::Inert) => assert!(Value::is_inert(listified).expect("ok").is_true()),
                Value::Constant(Constant::Ignore) => assert!(Value::is_ignore(listified).expect("ok").is_true()),
                Value::Constant(Constant::Null) => assert!(Value::is_null(listified).expect("ok").is_true()),
                _ => {
                    assert!(!Value::is_inert(listified.clone()).expect("ok").is_true());
                    assert!(!Value::is_ignore(listified.clone()).expect("ok").is_true());
                    assert!(!Value::is_null(listified.clone()).expect("ok").is_true());
                },
            }
        }
    }

    #[parameterized(
        inert = { Constant::Inert },
        ignore = { Constant::Ignore },
    )]
    fn test_is_eq_self(val: Constant) {
        assert!(val.is_eq(&val));
    }

    #[parameterized(
        inert = { Constant::Inert, Constant::Inert },
        ignore = { Constant::Ignore, Constant::Ignore },
    )]
    fn test_is_eq(val1: Constant, val2: Constant) {
        assert!(val1.is_eq(&val2));
        assert!(val2.is_eq(&val1));
    }

    #[test]
    fn test_is_eq_not() {
        assert!(!Constant::Ignore.is_eq(&Constant::Inert));
        assert!(!Constant::Inert.is_eq(&Constant::Ignore));
    }
}
