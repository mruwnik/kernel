use std::{fmt, ops::Deref};
use std::rc::Rc;
use crate::errors::{RuntimeError};
use crate::values::{ Value, CallResult, is_val };

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum CombinerType {
    Operative,
    Applicative,
}

#[derive(Clone)]
pub struct Combiner {
    c_type: CombinerType,
    expr: Rc<Value>,
    name: String,
    func: &'static dyn FnMut(Rc<Value>) -> CallResult,
}

impl fmt::Display for Combiner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl fmt::Debug for Combiner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Combiner")
            .field("type", &self.c_type)
            .field("name", &self.name)
            .field("expr", &self.expr)
            .finish()
    }
}

impl PartialEq for Combiner {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.expr == other.expr
    }
}

impl Combiner {
    fn new(
        name: impl Into<String>,
        func: &'static dyn FnMut(Rc<Value>) -> CallResult,
        expr: Rc<Value>,
        c_type: CombinerType,
    ) -> Rc<Value> {
        Rc::new(Value::Combiner(Combiner { c_type, name: name.into(), func, expr }))
    }

    pub fn is_eq(self: &Self, other: &Self) -> Result<bool, RuntimeError> {
        let same_exprs = Value::is_eq(&self.expr, &other.expr)?;
        Ok(self.name == other.name && self.c_type == other.c_type && Value::is_true(same_exprs))
    }
}


impl Value {
    pub fn new_applicative(
        name: impl Into<String>,
        func: &'static dyn FnMut(Rc<Value>) -> CallResult,
        expr: Rc<Value>,
    ) -> Rc<Value> {
        Combiner::new(name, func, expr, CombinerType::Applicative)
    }

    pub fn new_operative(
        name: impl Into<String>,
        func: &'static dyn FnMut(Rc<Value>) -> CallResult,
        expr: Rc<Value>,
    ) -> Rc<Value> {
        Combiner::new(name, func, expr, CombinerType::Operative)
    }

    pub fn is_operative(val: Rc<Value>) -> CallResult {
        is_val(val, &|val| matches!(val.deref(), Value::Combiner(Combiner{ c_type: CombinerType::Operative, .. })))
    }

    pub fn is_applicative(val: Rc<Value>) -> CallResult {
        is_val(val, &|val| matches!(val.deref(), Value::Combiner(Combiner{ c_type: CombinerType::Applicative, .. })))
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use std::ops::Deref;
    use crate::values::{ Constant, Value, tests::sample_values };
    use crate::values::Combiner;

    use super::CombinerType;

    #[test]
    fn test_is_combiner() {
        for val in sample_values() {
            let listified = Value::as_pair(val.clone());
            let is_applicative = Value::is_true(Value::is_applicative(listified.clone()).expect("ok"));
            let is_operative = Value::is_true(Value::is_operative(listified.clone()).expect("ok"));
            match val.deref() {
                Value::Combiner(Combiner{ c_type: CombinerType::Applicative, ..}) => assert!(is_applicative && !is_operative),
                Value::Combiner(Combiner{ c_type: CombinerType::Operative, ..}) => assert!(is_operative && !is_applicative),
                _ => {
                    assert!(!is_operative);
                    assert!(!is_applicative);
                },
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
                        Value::as_pair(val.clone())
                    ).unwrap(),
                ).unwrap(),
            ).unwrap();
            let is_applicative = Value::is_true(Value::is_applicative(listified.clone()).expect("ok"));
            let is_operative = Value::is_true(Value::is_operative(listified.clone()).expect("ok"));
            match val.deref() {
                Value::Combiner(Combiner{ c_type: CombinerType::Applicative, ..}) => assert!(is_applicative && !is_operative),
                Value::Combiner(Combiner{ c_type: CombinerType::Operative, ..}) => assert!(is_operative && !is_applicative),
                _ => {
                    assert!(!is_operative);
                    assert!(!is_applicative);
                },
            }
        }
    }

    #[parameterized(
        applicative = {
            Combiner{
                name: "applicative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        operative = {
            Combiner{
                name: "operative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
    )]
    fn test_is_eq_self(val: Combiner) {
        assert!(val.is_eq(&val).unwrap());
    }

    #[parameterized(
        applicatives = {
            Combiner{
                name: "applicative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "applicative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        operatives = {
            Combiner{
                name: "operative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            },
            Combiner{
                name: "operative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
    )]
    fn test_is_eq(val1: Combiner, val2: Combiner) {
        assert!(val1.is_eq(&val2).unwrap());
        assert!(val2.is_eq(&val1).unwrap());
    }

    #[parameterized(
        applicative_different_names = {
            Combiner{
                name: "applicativebla".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "applicative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        applicatives_differnt_expr = {
            Combiner{
                name: "applicative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Inert)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "applicative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        operatives_diff_names = {
            Combiner{
                name: "operativebla".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            },
            Combiner{
                name: "operative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
        operatives_diff_exprs = {
            Combiner{
                name: "operative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Inert)),
                c_type: CombinerType::Operative,
            },
            Combiner{
                name: "operative".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
        different_types = {
            Combiner{
                name: "bla".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "bla".to_string(),
                func: &|_| Ok(Rc::new(Value::Constant(Constant::Null))),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
    )]
    fn test_is_eq_not(val1: Combiner, val2: Combiner) {
        assert!(!val1.is_eq(&val2).unwrap());
        assert!(!val2.is_eq(&val1).unwrap());
    }
}
