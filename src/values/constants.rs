use std::fmt;
use std::rc::Rc;
use crate::values::{ Value, CallResult };

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    pub fn is_inert(&self) -> CallResult {
        Ok(Value::boolean(matches!(self, Value::Constant(Constant::Inert))))
    }

    pub fn is_ignore(&self) -> CallResult {
        Ok(Value::boolean(matches!(self, Value::Constant(Constant::Ignore))))
    }

    pub fn is_null(&self) -> CallResult {
        Ok(Value::boolean(matches!(self, Value::Constant(Constant::Null))))
    }

    pub fn make_const(c: Constant) -> Rc<Value> {
        Rc::new(Value::Constant(c))
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

    use std::rc::Rc;
    use crate::values::{ Bool, Env, Number, Pair, Str, Symbol, Value };
    use crate::values::constants::Constant;

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
    fn test_is_constant() {
        for val in sample_values() {
            match val {
                Value::Constant(Constant::Inert) => assert_eq!(val.is_inert().expect("ok"), Value::boolean(true)),
                Value::Constant(Constant::Ignore) => assert_eq!(val.is_ignore().expect("ok"), Value::boolean(true)),
                Value::Constant(Constant::Null) => assert_eq!(val.is_null().expect("ok"), Value::boolean(true)),
                _ => {
                    assert_eq!(val.is_inert().expect("ok"), Value::boolean(false));
                    assert_eq!(val.is_ignore().expect("ok"), Value::boolean(false));
                    assert_eq!(val.is_null().expect("ok"), Value::boolean(false));
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
