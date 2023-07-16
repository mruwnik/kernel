use std::fmt;
use std::rc::Rc;
use crate::values::Value;

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct Symbol(pub String);

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

impl Value {
    pub fn is_symbol(&self) -> Rc<Self> {
        Value::boolean(matches!(self, Value::Symbol(_)))
    }
}

impl Symbol {
    pub fn is_eq(self: &Self, other: &Self) -> Rc<Value> {
        dbg!(self.0.to_lowercase());
        dbg!(other.0.to_lowercase());
        Value::boolean(self.0.to_lowercase() == other.0.to_lowercase())
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::values::{Value, Env};
    use crate::values::symbols::Symbol;

    fn sample_values() -> Vec<Value> {
        vec![
            Value::Symbol(Symbol("bla".to_string())),
            Value::Env(Env::new(vec![])),
            Value::Bool(true),
        ]
    }

    #[parameterized(
        same = { Symbol("bla".to_string()), Symbol("bla".to_string()) },
        same_empty = { Symbol("".to_string()), Symbol("".to_string()) },
    )]
    fn test_same_symbol(sym1: Symbol, sym2: Symbol) {
        assert_eq!(sym1, sym2)
    }

    #[parameterized(
        diff1 = { Symbol("bla".to_string()), Symbol("blaaaa".to_string()) },
        diff2 = { Symbol("bLA".to_string()), Symbol("Bla".to_string()) },
        diff3 = { Symbol("a".to_string()), Symbol("".to_string()) },
    )]
    fn test_different_symbol(sym1: Symbol, sym2: Symbol) {
        assert_ne!(sym1, sym2)
    }

    #[test]
    fn test_is_symbol() {
        for val in sample_values() {
            match val {
                Value::Symbol(_) => assert_eq!(val.is_symbol(), Value::boolean(true)),
                _ => assert_eq!(val.is_symbol(), Value::boolean(false)),
            }
        }
    }

    #[parameterized(
        true_ = { Symbol("bla".to_string()) },
        false_ = { Symbol("BlEEee".to_string()) },
    )]
    fn test_is_eq_self(val: Symbol) {
        assert_eq!(val.is_eq(&val), Value::boolean(true));
    }

    #[parameterized(
        val1 = { Symbol("".to_string()), Symbol("".to_string()) },
        val2 = { Symbol("bla".to_string()), Symbol("bla".to_string()) },
        val3 = { Symbol("bla".to_string()), Symbol("BLA".to_string()) },
        val4 = { Symbol("BLA".to_string()), Symbol("bla".to_string()) },
        val5 = { Symbol("bLa".to_string()), Symbol("BlA".to_string()) },
    )]
    fn test_is_eq(val1: Symbol, val2: Symbol) {
        assert_eq!(val1.is_eq(&val2), Value::boolean(true));
        assert_eq!(val2.is_eq(&val1), Value::boolean(true));
    }

    #[parameterized(
        val1 = { Symbol("  ".to_string()), Symbol("".to_string()) },
        val2 = { Symbol("bla".to_string()), Symbol("fewfwefwefwf".to_string()) },
    )]
    fn test_is_eq_not(val1: Symbol, val2: Symbol) {
        assert_eq!(val1.is_eq(&val2), Value::boolean(false));
        assert_eq!(val2.is_eq(&val1), Value::boolean(false));
    }
}
