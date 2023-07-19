use std::ops::Deref;
use std::fmt;
use std::rc::Rc;

use super::{ Value, gen_sym, is_val, ValueResult };

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct Str(pub String, usize);

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", &self.0)
    }
}

impl Value {
    pub fn is_string(items: Rc<Value>) -> ValueResult {
        is_val(items, &|val| matches!(val.deref(), Value::String(_)))
    }

    pub fn make_string(val: impl Into<String>) -> Rc<Value> {
        Rc::new(Value::String(Str::new(val)))
    }
}

impl Str {
    pub fn is_eq(self: &Self, other: &Self) -> bool {
        self.1 == other.1
    }

    pub fn is_equal(self: &Self, other: &Self) -> bool {
        self.0 == other.0
    }

    pub fn new(string: impl Into<String>) -> Self {
        Self(string.into(), gen_sym())
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::ops::Deref;
    use crate::values::{ Value, tests::sample_values };
    use crate::values::strings::Str;

    #[test]
    fn test_is_string() {
        for val in sample_values() {
            let listified = val.as_pair();
            let is_type = Value::is_string(listified).expect("ok").is_true();
            match val.deref() {
                Value::String(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_string_multi() {
        for val in sample_values() {
            let listified = Value::cons(
                val.clone(),
                Value::cons(
                    val.clone(),
                    Value::cons(
                        val.clone(),
                        val.as_pair()
                    ).unwrap().into(),
                ).unwrap().into(),
            ).unwrap();
            let is_type = Value::is_string(listified.into()).expect("ok").is_true();
            match val.deref() {
                Value::String(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[parameterized(
        true_ = { Str::new("bla") },
        false_ = { Str::new("BlEEee") },
    )]
    fn test_is_eq_self(val: Str) {
        assert!(val.is_eq(&val));
    }

    #[parameterized(
        val1 = { Str::new(""), Str::new("") },
        val2 = { Str::new("bla"), Str::new("bla") },
    )]
    fn test_is_eq_not_same_obj(val1: Str, val2: Str) {
        assert!(!val1.is_eq(&val2));
        assert!(!val2.is_eq(&val1));
    }

    #[parameterized(
        val1 = { Str::new("  "), Str::new("") },
        val2 = { Str::new("bla"), Str::new("fewfwefwefwf") },
        val3 = { Str::new("bla"), Str::new("BLA") },
        val4 = { Str::new("BLA"), Str::new("bla") },
        val5 = { Str::new("bLa"), Str::new("BlA") },
    )]
    fn test_is_eq_not(val1: Str, val2: Str) {
        assert!(!val1.is_eq(&val2));
        assert!(!val2.is_eq(&val1));
    }

    #[parameterized(
        true_ = { Str::new("bla") },
        false_ = { Str::new("BlEEee") },
    )]
    fn test_is_equal_self(val: Str) {
        assert!(val.is_equal(&val));
    }

    #[parameterized(
        val1 = { Str::new(""), Str::new("") },
        val2 = { Str::new("bla"), Str::new("bla") },
    )]
    fn test_is_equal_not_same_item(val1: Str, val2: Str) {
        assert!(val1.is_equal(&val2));
        assert!(val2.is_equal(&val1));
    }

    #[parameterized(
        val1 = { Str::new("  "), Str::new("") },
        val2 = { Str::new("bla"), Str::new("fewfwefwefwf") },
        val3 = { Str::new("bla"), Str::new("BLA") },
        val4 = { Str::new("BLA"), Str::new("bla") },
        val5 = { Str::new("bLa"), Str::new("BlA") },
    )]
    fn test_is_equal_not(val1: Str, val2: Str) {
        assert!(!val1.is_equal(&val2));
        assert!(!val2.is_equal(&val1));
    }
}
