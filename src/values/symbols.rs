use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::ops::Deref;

use super::{ ValueResult, Value, is_val };

#[derive(Debug, Clone)]
pub struct Symbol(pub String);

// Case-insensitive comparison for HashMap lookup
impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_lowercase() == other.0.to_lowercase()
    }
}

impl Eq for Symbol {}

// Case-insensitive hash to match PartialEq
impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_lowercase().hash(state);
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

impl Value {
    pub fn is_symbol(items: Rc<Value>) -> ValueResult {
        is_val(items, &|val| matches!(val.deref(), Value::Symbol(_)))
    }

    pub fn make_symbol(name: impl Into<String>) -> Rc<Value> {
        Rc::new(Value::Symbol(Symbol(name.into())))
    }
}

impl Symbol {
    pub fn is_eq(self: &Self, other: &Self) -> bool {
        self.0.to_lowercase() == other.0.to_lowercase()
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::ops::Deref;
    use crate::values::{ Value, tests::sample_values };
    use crate::values::symbols::Symbol;

    #[test]
    fn test_is_symbol() {
        for val in sample_values() {
            let listified = val.as_pair();
            let is_type = Value::is_symbol(listified).expect("ok").is_true();
            match val.deref() {
                Value::Symbol(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_symbol_multi() {
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
            let is_type = Value::is_symbol(listified.into()).expect("ok").is_true();
            match val.deref() {
                Value::Symbol(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[parameterized(
        true_ = { Symbol("bla".to_string()) },
        false_ = { Symbol("BlEEee".to_string()) },
    )]
    fn test_is_eq_self(val: Symbol) {
        assert!(val.is_eq(&val));
    }

    #[parameterized(
        val1 = { Symbol("".to_string()), Symbol("".to_string()) },
        val2 = { Symbol("bla".to_string()), Symbol("bla".to_string()) },
        val3 = { Symbol("bla".to_string()), Symbol("BLA".to_string()) },
        val4 = { Symbol("BLA".to_string()), Symbol("bla".to_string()) },
        val5 = { Symbol("bLa".to_string()), Symbol("BlA".to_string()) },
    )]
    fn test_is_eq(val1: Symbol, val2: Symbol) {
        assert!(val1.is_eq(&val2));
        assert!(val2.is_eq(&val1));
    }

    #[parameterized(
        val1 = { Symbol("  ".to_string()), Symbol("".to_string()) },
        val2 = { Symbol("bla".to_string()), Symbol("fewfwefwefwf".to_string()) },
    )]
    fn test_is_eq_not(val1: Symbol, val2: Symbol) {
        assert!(!val1.is_eq(&val2));
        assert!(!val2.is_eq(&val1));
    }
}
