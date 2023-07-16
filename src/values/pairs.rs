use std::{fmt, ops::Deref};
use std::rc::Rc;
use std::cell::RefCell;
use crate::errors::{RuntimeError, ErrorTypes};
use crate::values::{Value, Constant};

#[derive(Debug, Clone, PartialEq)]
pub struct Pair {
    car: Rc<Value>,
    cdr: Rc<Value>,
    mutable: bool,
}
pub type PairRef = Rc<RefCell<Pair>>;

pub struct PairIter {
    node: Option<Rc<Pair>>,
}

impl Iterator for PairIter {
    type Item = Rc<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.node.as_ref();
        if node.is_some() {
            let val = node.unwrap().clone();
            self.node = match Pair::cdr(&val).deref() {
                Value::Pair(p) => Some(Rc::new(p.borrow().clone())),
                _ => None
            };
            Some(val.car().clone())
        } else {
            None
        }
    }
}

impl fmt::Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let car = &self.car();
        let cdr = &self.cdr();
        write!(f, "({car}{})", match cdr.deref() {
            Value::Pair(p) => {
                let mut string = String::new();
                for item in p.borrow().iter() {
                    string.push_str(&format!(" {item}"));
                }
                string
            },
            Value::Constant(Constant::Null) => "".to_string(),
            _ => format!(" . {cdr}"),
        })
    }
}


impl Pair {
    pub fn new(car: Rc<Value>, cdr: Rc<Value>) -> PairRef {
        Rc::new(RefCell::new(Pair { car, cdr, mutable: true }))
    }

    pub fn is_eq(self: &Self, other: &PairRef) -> bool {
        self.mutable == other.borrow().mutable &&
        self.car().is_eq(&other.borrow().car().clone()) == Value::boolean(true) &&
            self.cdr().is_eq(&other.borrow().cdr().clone()) == Value::boolean(true)
    }

    pub fn is_equal(self: &Self, other: &PairRef) -> bool {
        self.cdr().is_equal(&other.borrow().car().clone()) == Value::boolean(true) &&
            self.cdr().is_equal(&other.borrow().cdr().clone()) == Value::boolean(true)
    }

    fn car(self: &Self) -> Rc<Value> {
        self.car.clone()
    }

    fn cdr(self: &Self) -> Rc<Value> {
        self.cdr.clone()
    }

    fn set_car(self: &mut Self, val: Rc<Value>) -> Result<(), RuntimeError> {
        if self.mutable {
            self.car = val;
            Ok(())
        } else {
            Err(RuntimeError::new(ErrorTypes::ImmutableError, &"This pair is immutable"))
        }
    }

    fn set_cdr(self: &mut Self, val: Rc<Value>) -> Result<(), RuntimeError> {
        if self.mutable {
            self.cdr = val;
            Ok(())
        } else {
            Err(RuntimeError::new(ErrorTypes::ImmutableError, &"This pair is immutable"))
        }
    }

    pub fn iter(self: &Self) -> PairIter {
        PairIter { node: Some(Rc::new(self.clone())) }
    }
}

impl Value {
    pub fn is_pair(&self) -> Rc<Self> {
        Value::boolean(matches!(self, Value::Pair(_)))
    }

    pub fn cons(val1: Rc<Value>, val2: Rc<Value>) -> Rc<Value> {
        Rc::new(Value::Pair(Pair::new(val1, val2)))
    }

    pub fn set_car(pair: Rc<Value>, val: Rc<Value>) -> Result<Rc<Value>, RuntimeError> {
        if let Value::Pair(p) = pair.deref() {
            p.borrow_mut().set_car(val)?;
            Ok(Value::make_const(Constant::Inert))
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, &"set-car! expects a pair"))
        }
    }

    pub fn set_cdr(pair: Rc<Value>, val: Rc<Value>) -> Result<Rc<Value>, RuntimeError> {
        if let Value::Pair(p) = pair.deref() {
            p.borrow_mut().set_cdr(val)?;
            Ok(Value::make_const(Constant::Inert))
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, &"set-cdr! expects a pair"))
        }
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use crate::values::{ Bool, Constant, Env, Number, Str, Symbol, Value };
    use crate::values::pairs::{Pair, PairIter};

    use super::PairRef;

    fn make_list(mut items: Vec<Rc<Value>>) -> PairRef {
        items.reverse();
        let mut iterator = items.iter();
        let i = iterator.next().unwrap();
        let mut node = Pair::new(i.clone(), Rc::new(Value::Constant(Constant::Null)));
        for i in iterator {
            node = Pair::new(i.clone(), Rc::new(Value::Pair(node)));
        }
        node
    }
    fn number_list(items: Vec<i64>) -> PairRef {
        make_list(items.iter().map(|i| Rc::new(Value::Number(Number::Int(*i)))).collect())
    }

    fn sample_values() -> Vec<Value> {
        vec![
            Value::Bool(Bool::True),
            Value::Constant(Constant::Ignore),
            Value::Constant(Constant::Inert),
            Value::Constant(Constant::Null),
            Value::Env(Env::new(vec![])),
            Value::Number(Number::Int(123)),
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null))
            )),
            Value::String(Str::new("bla")),
            Value::Symbol(Symbol("bla".to_string())),
        ]
    }

    #[test]
    fn test_is_pair() {
        for val in sample_values() {
            match val {
                Value::Pair(_) => assert_eq!(val.is_pair(), Value::boolean(true)),
                _ => {
                    assert_eq!(val.is_pair(), Value::boolean(false));
                },
            }
        }
    }

    #[test]
    fn test_is_eq_self() {
        let val = Pair::new(
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Constant(Constant::Null))
        );
        assert!(val.borrow().is_eq(&val));
    }

    #[parameterized(
        basic = { number_list(vec![1, 2, 3]), number_list(vec![1, 2, 3]) },
        nested = {
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                )),
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                )),
        },
    )]
    fn test_is_eq(val1: PairRef, val2: PairRef) {
        assert!(val1.borrow().is_eq(&val2));
        assert!(val2.borrow().is_eq(&val1));
    }

    #[parameterized(
        basic = { number_list(vec![1, 2, 3]), number_list(vec![1, 2]) },
    )]
    fn test_is_eq_not(val1: PairRef, val2: PairRef) {
        assert!(!val1.borrow().is_eq(&val2));
        assert!(!val2.borrow().is_eq(&val1));
    }

    #[parameterized(
        pair_single = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null))
            )),
            "(1)"
        },
        pair_dotted = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Number(Number::Int(2))),
            )),
            "(1 . 2)"
        },
        pair_double = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Rc::new(Value::Constant(Constant::Null))
                )
            )),
            "(1 2)"
        },
        pair_multi = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Value::cons(
                        Rc::new(Value::Number(Number::Int(3))),
                        Value::cons(
                            Rc::new(Value::Number(Number::Int(4))),
                            Rc::new(Value::Constant(Constant::Null))
                        )
                    )
                )
            )),
            "(1 2 3 4)"
        },
        pair_multi_dotted = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null))
            )),
            "(1)"
        },
    )]
    fn test_format(val: Value, expected: &str) {
        assert_eq!(format!("{val}"), expected)
    }

    #[test]
    fn test_set_car() {
        let pair = Value::cons(
            Rc::new(Value::Number(Number::Int(4))),
            Rc::new(Value::Constant(Constant::Null))
        );
        assert_eq!(format!("{pair}"), "(4)");

        assert_eq!(
            Value::set_car(pair.clone(), Value::boolean(true)).expect("Could not set"),
            Value::make_const(Constant::Inert)
        );
        assert_eq!(format!("{pair}"), "(#t)");
    }

    #[test]
    fn test_set_car_error() {
        for val in sample_values() {
            match val {
                Value::Pair(_) => (),
                _ => {
                    Value::set_car(
                        Rc::new(val),
                        Value::boolean(true)
                    ).expect_err("should have raised an error!");
                },
            }
        }
    }

    #[test]
    fn test_set_cdr() {
        let pair = Value::cons(
            Rc::new(Value::Number(Number::Int(4))),
            Rc::new(Value::Constant(Constant::Null))
        );
        assert_eq!(format!("{pair}"), "(4)");

        assert_eq!(
            Value::set_cdr(pair.clone(), Value::boolean(true)).expect("Could not set"),
            Value::make_const(Constant::Inert)
        );
        assert_eq!(format!("{pair}"), "(4 . #t)");
    }

    #[test]
    fn test_set_cdr_error() {
        for val in sample_values() {
            match val {
                Value::Pair(_) => (),
                _ => {
                    Value::set_cdr(
                        Rc::new(val),
                        Value::boolean(true)
                    ).expect_err("should have raised an error!");
                },
            }
        }
    }
}
