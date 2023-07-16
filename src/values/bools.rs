use std::rc::Rc;
use crate::values::Value;

impl Value {
    pub fn is_bool(&self) -> Rc<Self> {
        Rc::new(Value::Bool(matches!(self, Value::Bool(_))))
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
        ]
    }

    #[test]
    fn test_is_bool() {
        for val in sample_values() {
            match val {
                Value::Bool(_) => assert_eq!(val.is_bool(), Rc::new(Value::Bool(true))),
                _ => assert_eq!(val.is_bool(), Rc::new(Value::Bool(false))),
            }
        }
    }

    #[parameterized(
        env = { Value::Env(Env::new(vec![])), "#env" },
        symbol = { Value::Symbol(Symbol("bla".to_string())), "bla" },
        true_ = { Value::Bool(true), "#t" },
        fals_ = { Value::Bool(false), "#f" },
    )]
    fn test_format(val: Value, expected: &str) {
        assert_eq!(format!("{val}"), expected)
    }
}
