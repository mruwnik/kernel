use std::{fmt, rc::Rc, ops::Deref};
use std::collections::HashSet;
use crate::values::{ ValueResult, Value, is_val };
use crate::values::pairs::PairRef;

#[derive(Debug, PartialEq, Eq, Clone)]
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

    pub fn is_true(&self) -> bool {
        self == Value::boolean(true).deref()
    }

    // primatives
    pub fn is_boolean(items: Rc<Value>) -> ValueResult {
        is_val(items, &|val| matches!(val.deref(), Value::Bool(_)))
    }

    pub fn is_eq(self: Rc<Self>, other: Rc<Self>) -> ValueResult {
        Value::boolean(
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
        ).ok()
    }

    pub fn is_equal(self: Rc<Self>, other: Rc<Self>) -> ValueResult {
        let mut visited: HashSet<(usize, usize)> = HashSet::new();
        Self::is_equal_impl(self, other, &mut visited)
    }

    pub fn is_equal_impl(
        self_val: Rc<Self>,
        other: Rc<Self>,
        visited: &mut HashSet<(usize, usize)>,
    ) -> ValueResult {
        Value::boolean(
            match (self_val.deref(), other.deref()) {
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Combiner(a), Value::Combiner(b)) => a.is_eq(b)?,
                (Value::Constant(a), Value::Constant(b)) => a.is_eq(b),
                (Value::Env(a), Value::Env(b)) => a.borrow().is_eq(b.clone()),
                (Value::Number(a), Value::Number(b)) => a.is_equal(b),
                (Value::Pair(a), Value::Pair(b)) => is_equal_pairs(a, b, visited)?,
                (Value::Symbol(a), Value::Symbol(b)) => a.is_eq(b),
                (Value::String(a), Value::String(b)) => a.is_equal(b),
                _ => false
            }
        ).ok()
    }
}

fn is_equal_pairs(
    a: &PairRef,
    b: &PairRef,
    visited: &mut HashSet<(usize, usize)>,
) -> Result<bool, crate::errors::RuntimeError> {
    let ptr_a = Rc::as_ptr(a) as usize;
    let ptr_b = Rc::as_ptr(b) as usize;
    let key = (ptr_a.min(ptr_b), ptr_a.max(ptr_b));

    if !visited.insert(key) {
        return Ok(true);
    }

    let car_a = a.borrow().car();
    let car_b = b.borrow().car();
    let cdr_a = a.borrow().cdr();
    let cdr_b = b.borrow().cdr();

    Ok(
        Value::is_equal_impl(car_a, car_b, visited)?.is_true() &&
        Value::is_equal_impl(cdr_a, cdr_b, visited)?.is_true()
    )
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
            let listified = val.clone().as_pair();
            let is_type = Value::is_boolean(listified).expect("ok").is_true();
            match val.deref() {
                Value::Bool(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_boolean_multi() {
        for val in sample_values() {
            let listified = Value::cons(
                val.clone(),
                Value::cons(
                    val.clone(),
                    Value::cons(
                        val.clone(),
                        val.clone().as_pair()
                    ).unwrap().into(),
                ).unwrap().into(),
            ).unwrap().into();
            match val.deref() {
                Value::Bool(_) => assert!(Value::is_boolean(listified).expect("ok").is_true()),
                _ => assert!(!Value::is_boolean(listified).expect("ok").is_true()),
            }
        }
    }

    #[parameterized(
        true_ = { Value::boolean(true) },
        false_ = { Value::boolean(false) },
    )]
    fn test_is_eq_self(val: Rc<Value>) {
        assert!(val.clone().is_eq(val).expect("ok").is_true());
    }

    #[parameterized(
        true_ = { true },
        false_ = { false },
    )]
    fn test_is_eq(val: bool) {
        let val1 = Value::boolean(val);
        let val2 = Value::boolean(val);

        assert!(val1.clone().is_eq(val2.clone()).expect("ok").is_true());
        assert!(val2.is_eq(val1).expect("ok").is_true());
    }

    #[test]
    fn test_is_equal_circular_list() {
        // Create a circular list: (1 . <self>)
        let pair1: Rc<Value> = Value::cons(
            Rc::new(Value::Number(crate::values::Number::Int(1))),
            Value::make_null(),
        ).unwrap().into();
        // Make it circular by setting cdr to itself
        Value::set_cdr(pair1.clone(), pair1.clone()).unwrap();

        // Create another identical circular list
        let pair2: Rc<Value> = Value::cons(
            Rc::new(Value::Number(crate::values::Number::Int(1))),
            Value::make_null(),
        ).unwrap().into();
        Value::set_cdr(pair2.clone(), pair2.clone()).unwrap();

        // This should not hang and should return true
        assert!(pair1.clone().is_equal(pair2.clone()).expect("ok").is_true());

        // Test self-equality (same circular list)
        assert!(pair1.clone().is_equal(pair1.clone()).expect("ok").is_true());
    }

    #[test]
    fn test_is_equal_circular_list_different() {
        // Create circular list with 1
        let pair1: Rc<Value> = Value::cons(
            Rc::new(Value::Number(crate::values::Number::Int(1))),
            Value::make_null(),
        ).unwrap().into();
        Value::set_cdr(pair1.clone(), pair1.clone()).unwrap();

        // Create circular list with 2
        let pair2: Rc<Value> = Value::cons(
            Rc::new(Value::Number(crate::values::Number::Int(2))),
            Value::make_null(),
        ).unwrap().into();
        Value::set_cdr(pair2.clone(), pair2.clone()).unwrap();

        // These should not be equal
        assert!(!pair1.clone().is_equal(pair2).expect("ok").is_true());
    }
}
