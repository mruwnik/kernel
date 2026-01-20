use std::{rc::Rc};
use std::ops::Deref;
use std::fmt;
use crate::{values::{ Value, ValueResult, Constant, is_val }, errors::RuntimeError};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Number::Int(n) => n.to_string(),
            Number::Float(n) => n.to_string(),
        })
    }
}

fn process(items: Rc<Value>, func: &dyn Fn(Rc<Value>, Rc<Value>) -> ValueResult) -> ValueResult {
    match items.deref() {
        Value::Constant(Constant::Null) => Number::int(0).ok(),
        Value::Pair(_) => {
            let num1: Rc<Value> = items.car()?.into();
            let cdr: Rc<Value> = items.cdr()?.into();
            match cdr.deref() {
                Value::Constant(Constant::Null) => num1.ok(),
                Value::Number(_) => Number::add(num1, cdr),
                Value::Pair(_) => {
                    let num2: Rc<Value> = cdr.car()?.into();
                    Value::cons(
                        func(num1, num2)?.into(),
                        cdr.cdr()?.into()
                    )
                }
                _ => RuntimeError::type_error("+ only works on numbers"),
            }
        },
        _ => RuntimeError::type_error("+ only works on numbers"),
    }
}

impl Value {
    pub fn is_number(items: Rc<Value>) -> ValueResult {
        is_val(items, &|val| matches!(val.deref(), Value::Number(_)))
    }

    pub fn is_integer(items: Rc<Value>) -> ValueResult {
        is_val(items, &|val| matches!(val.deref(), Value::Number(Number::Int(_))))
    }

    // Adds the first 2 items, returning a list of all remaining items to sum, or the final sum if fewer
    // than 2 items provided
    pub fn add(items: Rc<Value>) -> ValueResult {
        process(items, &Number::add)
    }

    pub fn minus(items: Rc<Value>) -> ValueResult {
        process(items, &Number::minus)
    }

    pub fn multiply(items: Rc<Value>) -> ValueResult {
        process_mult(items, &Number::multiply)
    }

    pub fn divide(items: Rc<Value>) -> ValueResult {
        process_div(items, &Number::divide)
    }

    pub fn modulo(items: Rc<Value>) -> ValueResult {
        process(items, &Number::modulo)
    }
}

fn process_mult(items: Rc<Value>, func: &dyn Fn(Rc<Value>, Rc<Value>) -> ValueResult) -> ValueResult {
    match items.deref() {
        Value::Constant(Constant::Null) => Number::int(1).ok(), // Identity for multiplication
        Value::Pair(_) => {
            let num1: Rc<Value> = items.car()?.into();
            let cdr: Rc<Value> = items.cdr()?.into();
            match cdr.deref() {
                Value::Constant(Constant::Null) => num1.ok(),
                Value::Number(_) => func(num1, cdr),
                Value::Pair(_) => {
                    let num2: Rc<Value> = cdr.car()?.into();
                    Value::cons(
                        func(num1, num2)?.into(),
                        cdr.cdr()?.into()
                    )
                }
                _ => RuntimeError::type_error("* only works on numbers"),
            }
        },
        _ => RuntimeError::type_error("* only works on numbers"),
    }
}

fn process_div(items: Rc<Value>, func: &dyn Fn(Rc<Value>, Rc<Value>) -> ValueResult) -> ValueResult {
    match items.deref() {
        Value::Constant(Constant::Null) => RuntimeError::type_error("/ requires at least one argument"),
        Value::Pair(_) => {
            let num1: Rc<Value> = items.car()?.into();
            let cdr: Rc<Value> = items.cdr()?.into();
            match cdr.deref() {
                // Single argument: (/ n) = 1/n (reciprocal)
                Value::Constant(Constant::Null) => func(Number::int(1), num1),
                Value::Number(_) => func(num1, cdr),
                Value::Pair(_) => {
                    let num2: Rc<Value> = cdr.car()?.into();
                    let rest: Rc<Value> = cdr.cdr()?.into();
                    let result = func(num1, num2)?;
                    // If no more args, return the result directly
                    match rest.deref() {
                        Value::Constant(Constant::Null) => result.ok(),
                        _ => Value::cons(result.into(), rest)
                    }
                }
                _ => RuntimeError::type_error("/ only works on numbers"),
            }
        },
        _ => RuntimeError::type_error("/ only works on numbers"),
    }
}

impl Number {
    // helpers
    pub fn int(val: i64) -> Rc<Value> {
        Rc::new(Value::Number(Number::Int(val)))
    }

    // primitives
    pub fn is_eq(self: &Self, other: &Self) -> bool {
        match (self, other) {
            (Number::Int(a), Number::Int(b)) => a == b,
            (Number::Float(a), Number::Float(b)) => a == b,
            _ => false
        }
    }

    pub fn is_equal(self: &Self, other: &Self) -> bool {
        self.eq(other)
    }

    // functions
    fn add(val1: Rc<Value>, val2: Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let num = match (n1, n2) {
                    (Number::Int(n1), Number::Int(n2)) => Number::Int(n1 + n2),
                    (Number::Float(n1), Number::Int(n2)) => Number::Float(n1 + *n2 as f64),
                    (Number::Int(n1), Number::Float(n2)) => Number::Float(*n1 as f64 + n2),
                    (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 + n2),
                };
                Value::Number(num).ok()
            },
            _ => RuntimeError::type_error(format!("+ only works on numbers - got (+ {val1} {val2})")),
        }
    }

    fn minus(val1: Rc<Value>, val2: Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let num = match (n1, n2) {
                    (Number::Int(n1), Number::Int(n2)) => Number::Int(n1 - n2),
                    (Number::Float(n1), Number::Int(n2)) => Number::Float(n1 - *n2 as f64),
                    (Number::Int(n1), Number::Float(n2)) => Number::Float(*n1 as f64 - n2),
                    (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 - n2),
                };
                Value::Number(num).ok()
            },
            _ => RuntimeError::type_error(format!("- only works on numbers - got (- {val1} {val2})")),
        }
    }

    fn multiply(val1: Rc<Value>, val2: Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let num = match (n1, n2) {
                    (Number::Int(n1), Number::Int(n2)) => Number::Int(n1 * n2),
                    (Number::Float(n1), Number::Int(n2)) => Number::Float(n1 * *n2 as f64),
                    (Number::Int(n1), Number::Float(n2)) => Number::Float(*n1 as f64 * n2),
                    (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 * n2),
                };
                Value::Number(num).ok()
            },
            _ => RuntimeError::type_error(format!("* only works on numbers - got (* {val1} {val2})")),
        }
    }

    fn divide(val1: Rc<Value>, val2: Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                // Check for division by zero
                let is_zero = match n2 {
                    Number::Int(0) => true,
                    Number::Float(f) if *f == 0.0 => true,
                    _ => false,
                };
                if is_zero {
                    return RuntimeError::type_error("division by zero");
                }
                let num = match (n1, n2) {
                    (Number::Int(n1), Number::Int(n2)) => Number::Int(n1 / n2), // Integer division
                    (Number::Float(n1), Number::Int(n2)) => Number::Float(n1 / *n2 as f64),
                    (Number::Int(n1), Number::Float(n2)) => Number::Float(*n1 as f64 / n2),
                    (Number::Float(n1), Number::Float(n2)) => Number::Float(n1 / n2),
                };
                Value::Number(num).ok()
            },
            _ => RuntimeError::type_error(format!("/ only works on numbers - got (/ {val1} {val2})")),
        }
    }

    fn modulo(val1: Rc<Value>, val2: Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(Number::Int(n1)), Value::Number(Number::Int(n2))) => {
                if *n2 == 0 {
                    return RuntimeError::type_error("modulo by zero");
                }
                Value::Number(Number::Int(n1 % n2)).ok()
            },
            _ => RuntimeError::type_error("mod only works on integers"),
        }
    }

    // Comparisons
    pub fn less_than(val1: &Rc<Value>, val2: &Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let result = match (n1, n2) {
                    (Number::Int(a), Number::Int(b)) => a < b,
                    (Number::Float(a), Number::Float(b)) => a < b,
                    (Number::Int(a), Number::Float(b)) => (*a as f64) < *b,
                    (Number::Float(a), Number::Int(b)) => *a < (*b as f64),
                };
                Value::boolean(result).ok()
            },
            _ => RuntimeError::type_error("< only works on numbers"),
        }
    }

    pub fn less_than_or_equal(val1: &Rc<Value>, val2: &Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let result = match (n1, n2) {
                    (Number::Int(a), Number::Int(b)) => a <= b,
                    (Number::Float(a), Number::Float(b)) => a <= b,
                    (Number::Int(a), Number::Float(b)) => (*a as f64) <= *b,
                    (Number::Float(a), Number::Int(b)) => *a <= (*b as f64),
                };
                Value::boolean(result).ok()
            },
            _ => RuntimeError::type_error("<= only works on numbers"),
        }
    }

    pub fn greater_than(val1: &Rc<Value>, val2: &Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let result = match (n1, n2) {
                    (Number::Int(a), Number::Int(b)) => a > b,
                    (Number::Float(a), Number::Float(b)) => a > b,
                    (Number::Int(a), Number::Float(b)) => (*a as f64) > *b,
                    (Number::Float(a), Number::Int(b)) => *a > (*b as f64),
                };
                Value::boolean(result).ok()
            },
            _ => RuntimeError::type_error("> only works on numbers"),
        }
    }

    pub fn greater_than_or_equal(val1: &Rc<Value>, val2: &Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let result = match (n1, n2) {
                    (Number::Int(a), Number::Int(b)) => a >= b,
                    (Number::Float(a), Number::Float(b)) => a >= b,
                    (Number::Int(a), Number::Float(b)) => (*a as f64) >= *b,
                    (Number::Float(a), Number::Int(b)) => *a >= (*b as f64),
                };
                Value::boolean(result).ok()
            },
            _ => RuntimeError::type_error(">= only works on numbers"),
        }
    }

    pub fn numeric_equal(val1: &Rc<Value>, val2: &Rc<Value>) -> ValueResult {
        match (val1.deref(), val2.deref()) {
            (Value::Number(n1), Value::Number(n2)) => {
                let result = match (n1, n2) {
                    (Number::Int(a), Number::Int(b)) => a == b,
                    (Number::Float(a), Number::Float(b)) => a == b,
                    (Number::Int(a), Number::Float(b)) => (*a as f64) == *b,
                    (Number::Float(a), Number::Int(b)) => *a == (*b as f64),
                };
                Value::boolean(result).ok()
            },
            _ => RuntimeError::type_error("=? only works on numbers"),
        }
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use std::ops::Deref;
    use crate::values::{ Value, tests::sample_values };
    use crate::values::numbers::Number;

    #[test]
    fn test_is_number() {
        for val in sample_values() {
            let listified = val.as_pair();
            let is_type = Value::is_number(listified.clone()).expect("ok").is_true();
            match val.deref() {
                Value::Number(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_number_multi() {
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
            let is_type = Value::is_number(listified.into()).expect("ok").is_true();
            match val.deref() {
                Value::Number(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[parameterized(
        int = { Number::Int(123) },
        float = { Number::Float(12.32) },
    )]
    fn test_is_eq_self(val: Number) {
        assert!(val.is_eq(&val));
    }

    #[parameterized(
        same_ints = { Number::Int(123), Number::Int(123) },
        same_floats = { Number::Float(432.1), Number::Float(432.1) },
    )]
    fn test_is_eq(val1: Number, val2: Number) {
        assert!(val1.is_eq(&val2));
        assert!(val2.is_eq(&val1));
    }

    #[parameterized(
        different_ints = { Number::Int(123), Number::Int(321) },
        different_floats = { Number::Float(32.1), Number::Float(432.1) },
        different_nums = { Number::Float(32.0), Number::Int(12) },
        different_nums2 = { Number::Int(43), Number::Float(12.0) },
        same_nums = { Number::Float(12.0), Number::Int(12) },
        same_nums2 = { Number::Int(12), Number::Float(12.0) },
    )]
    fn test_is_eq_not(val1: Number, val2: Number) {
        assert!(!val1.is_eq(&val2));
        assert!(!val2.is_eq(&val1));
    }

    #[parameterized(
        ints = { Rc::new(Value::Number(Number::Int(1))), Rc::new(Value::Number(Number::Int(2))), "3" },
        floats = { Rc::new(Value::Number(Number::Float(1.23))), Rc::new(Value::Number(Number::Float(2.32))), "3.55" },
        int_float = { Rc::new(Value::Number(Number::Int(1))), Rc::new(Value::Number(Number::Float(2.32))), "3.32" },
        float_int = { Rc::new(Value::Number(Number::Float(1.23))), Rc::new(Value::Number(Number::Int(2))), "3.23" },
    )]
    fn test_number_add(num1: Rc<Value>, num2: Rc<Value>, expected: &str) {
        assert_eq!(Number::add(num1, num2).unwrap().to_string(), format!("{expected}"));
    }

    #[parameterized(
        empty = { Value::make_null(), "0" },
        single = { Value::to_list(vec![Rc::new(Value::Number(Number::Int(1)))]).unwrap().into(), "1" },
        multi = { Value::to_list(vec![
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Number(Number::Int(2))),
            Rc::new(Value::Number(Number::Int(3))),
            Rc::new(Value::Number(Number::Int(4))),
        ]).unwrap().into(), "(3 3 4)" },
    )]
    fn test_add(vals: Rc<Value>, expected: &str) {
        assert_eq!(Value::add(vals).unwrap().to_string(), format!("{expected}"));
    }

    #[parameterized(
        ints = { Rc::new(Value::Number(Number::Int(1))), Rc::new(Value::Number(Number::Int(2))), "-1" },
        floats = { Rc::new(Value::Number(Number::Float(3.23))), Rc::new(Value::Number(Number::Float(2.32))), "0.9100000000000001" },
        int_float = { Rc::new(Value::Number(Number::Int(1))), Rc::new(Value::Number(Number::Float(2.32))), "-1.3199999999999998" },
        float_int = { Rc::new(Value::Number(Number::Float(4.23))), Rc::new(Value::Number(Number::Int(2))), "2.2300000000000004" },
    )]
    fn test_number_minue(num1: Rc<Value>, num2: Rc<Value>, expected: &str) {
        assert_eq!(Number::minus(num1, num2).unwrap().to_string(), format!("{expected}"));
    }

    #[parameterized(
        empty = { Value::make_null(), "0" },
        single = { Value::to_list(vec![Rc::new(Value::Number(Number::Int(1)))]).unwrap().into(), "1" },
        multi = { Value::to_list(vec![
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Number(Number::Int(2))),
            Rc::new(Value::Number(Number::Int(3))),
            Rc::new(Value::Number(Number::Int(4))),
        ]).unwrap().into(), "(-1 3 4)" },
    )]
    fn test_minus(vals: Rc<Value>, expected: &str) {
        assert_eq!(Value::minus(vals).unwrap().to_string(), format!("{expected}"));
    }
}
