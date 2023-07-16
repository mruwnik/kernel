use std::fmt;
use std::rc::Rc;
use crate::values::Value;

use super::gen_sym;

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct Str(pub String, usize);

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", &self.0)
    }
}

impl Value {
    pub fn is_string(&self) -> Rc<Self> {
        Value::boolean(matches!(self, Value::String(_)))
    }
}

impl Str {
    pub fn is_eq(self: &Self, other: &Self) -> bool {
        self.1 == other.1
    }

    pub fn is_equal(self: &Self, other: &Self) -> bool {
        self.0 == other.0
    }

    pub fn new(string: &str) -> Self {
        Self(string.to_string(), gen_sym())
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::values::{Value, Env, Symbol, Bool};
    use crate::values::strings::Str;

    fn sample_values() -> Vec<Value> {
        vec![
            Value::Symbol(Symbol("bla".to_string())),
            Value::Env(Env::new(vec![])),
            Value::Bool(Bool::True),
            Value::String(Str::new("bla")),
        ]
    }


    #[test]
    fn test_is_string() {
        for val in sample_values() {
            match val {
                Value::String(_) => assert_eq!(val.is_string(), Value::boolean(true)),
                _ => assert_eq!(val.is_string(), Value::boolean(false)),
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
