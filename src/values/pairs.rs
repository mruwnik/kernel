use std::fmt::format;
use std::{fmt, ops::Deref};
use std::rc::Rc;
use std::cell::RefCell;
use crate::errors::{RuntimeError, ErrorTypes};
use crate::values::{ CallResult, Constant, Value, is_val };

#[derive(Debug, Clone, PartialEq)]
pub struct Pair {
    car: Rc<Value>,
    cdr: Rc<Value>,
    mutable: bool,
}
pub type PairRef = Rc<RefCell<Pair>>;

pub struct ValueIter {
    node: Option<Rc<Value>>,
}

impl Iterator for ValueIter {
    type Item = Rc<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.node.as_deref() {
            None => None,
            Some(v) => {
                let val = Rc::new(v.clone());
                match v.deref() {
                    Value::Pair(_) => {
                        self.node = Some(Value::cdr(val.clone()).unwrap());
                        Some(Value::car(val.clone()).unwrap())
                    },
                    _ => {
                        self.node = None;
                        Some(val.clone())
                    }
                }
            }
        }
    }
}

impl fmt::Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let car = &self.car();
        let cdr = &self.cdr();
        let vals: Vec<Rc<Value>> = cdr.iter().collect();
        write!(f, "({car}{})", match vals.split_last() {
            None => ")".to_string(),
            Some((last, others)) => {
                let mut rest = others.iter().map(|v| format!("{v}")).collect::<Vec<String>>().join(" ");
                if !rest.is_empty() {
                    rest.insert_str(0, " ");
                }
                match last.deref() {
                    Value::Constant(Constant::Null) => format!("{rest}"),
                    _ => format!("{rest} . {last}"),
                }
            }
        })
    }
}

impl Pair {
    pub fn new(car: Rc<Value>, cdr: Rc<Value>, mutable: bool) -> PairRef {
        Rc::new(RefCell::new(Pair { car, cdr, mutable }))
    }

    pub fn is_eq(self: &Self, other: &PairRef) -> Result<bool, RuntimeError> {
        Ok(
            self.mutable == other.borrow().mutable &&
                self.car().is_eq(&other.borrow().car().clone())? == Value::boolean(true) &&
                self.cdr().is_eq(&other.borrow().cdr().clone())? == Value::boolean(true)
        )
    }

    pub fn is_equal(self: &Self, other: &PairRef) -> Result<bool, RuntimeError> {
        Ok(
                self.car().is_equal(&other.borrow().car().clone())? == Value::boolean(true) &&
                self.cdr().is_equal(&other.borrow().cdr().clone())? == Value::boolean(true)
        )
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
            Err(RuntimeError::new(ErrorTypes::ImmutableError, "This pair is immutable"))
        }
    }

    fn set_cdr(self: &mut Self, val: Rc<Value>) -> Result<(), RuntimeError> {
        if self.mutable {
            self.cdr = val;
            Ok(())
        } else {
            Err(RuntimeError::new(ErrorTypes::ImmutableError, "This pair is immutable"))
        }
    }
}

impl Value {
    // Helper funcs
    pub fn as_pair(val: Rc<Value>) -> Rc<Value> {
        Value::cons(val, Value::make_const(Constant::Null)).unwrap()
    }

    pub fn iter(self: &Self) -> ValueIter {
        ValueIter { node: Some(Rc::new(self.clone())) }
    }

    // primatives
    pub fn is_pair(items: Rc<Value>) -> CallResult {
        is_val(items, &|val| matches!(val.deref(), Value::Pair(_)))
    }

    pub fn cons(val1: Rc<Value>, val2: Rc<Value>) -> CallResult {
        Ok(Rc::new(Value::Pair(Pair::new(val1, val2, true))))
    }

    pub fn set_car(self: Rc<Value>, val: Rc<Value>) -> CallResult {
        if let Value::Pair(p) = self.deref() {
            p.borrow_mut().set_car(val)?;
            Ok(Value::make_const(Constant::Inert))
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "set-car! expects a pair"))
        }
    }

    pub fn set_cdr(self: Rc<Value>, val: Rc<Value>) -> CallResult {
        if let Value::Pair(p) = self.deref() {
            p.borrow_mut().set_cdr(val)?;
            Ok(Value::make_const(Constant::Inert))
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "set-cdr! expects a pair"))
        }
    }

    pub fn copy_es_immutable(self: Rc<Value>) -> CallResult {
        Ok(
            match self.deref() {
                Value::Pair(p) => Rc::new(Value::Pair(
                    Pair::new(
                        p.borrow().car().copy_es_immutable()?,
                        p.borrow().cdr().copy_es_immutable()?,
                        false
                    ))
                ),
                _ => self.clone(),
            }
        )
    }

    // library funcs
    pub fn copy_es(self: Rc<Value>) -> CallResult {
        Ok(
            match self.deref() {
                Value::Pair(p) => Rc::new(Value::Pair(
                    Pair::new(
                        p.borrow().car().copy_es_immutable()?,
                        p.borrow().cdr().copy_es_immutable()?,
                        true
                    ))
                ),
                _ => self.clone(),
            }
        )
    }

    pub fn car(pair: Rc<Value>) -> CallResult {
        if let Value::Pair(p) = pair.deref() {
            Ok(p.borrow().car())
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "car expects a pair"))
        }
    }

    pub fn cdr(pair: Rc<Value>) -> CallResult {
        if let Value::Pair(p) = pair.deref() {
            Ok(p.borrow().cdr())
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "cdr expects a pair"))
        }
    }

    // pub fn append(lists: Rc<Value>) -> CallResult {
    //     match lists.deref() {
    //         Value::Pair(_) => {
    //             let car = Value::copy_es(Value::car(lists.clone())?)?;
    //             let cdr = Value::cdr(lists.clone())?;
    //             match car.clone().deref() {
    //                 Value::Pair(_) => {
    //                     Value::set_cdr(car.clone(), cdr)?;
    //                     Ok(car)
    //                 }
    //                 _ => Ok(car),
    //             }
    //         },
    //        _ =>  Ok(lists),
    //     }
    // }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::ops::Deref;
    use std::rc::Rc;
    use crate::errors::RuntimeError;
    use crate::values::{  Constant,  Number, Value, tests::sample_values };
    use crate::values::pairs::Pair;

    use super::PairRef;

    fn make_list(mut items: Vec<Rc<Value>>) -> PairRef {
        items.reverse();
        let mut iterator = items.iter();
        let i = iterator.next().unwrap();
        let mut node = Pair::new(i.clone(), Rc::new(Value::Constant(Constant::Null)), true);
        for i in iterator {
            node = Pair::new(i.clone(), Rc::new(Value::Pair(node)), true);
        }
        node
    }
    fn number_list(items: Vec<i64>) -> PairRef {
        make_list(items.iter().map(|i| Rc::new(Value::Number(Number::Int(*i)))).collect())
    }
    fn make_immutable(p: PairRef) -> PairRef {
        let copied = Value::copy_es_immutable(Rc::new(Value::Pair(p))).expect("should work");
        if let Value::Pair(pair) = copied.deref() {
            pair.clone()
        } else {
            Pair::new(
                Rc::new(Value::Constant(Constant::Null)),
                Rc::new(Value::Constant(Constant::Null)),
                false,
            )
        }
    }

    #[test]
    fn test_is_pair() {
        for val in sample_values() {
            let listified = Value::as_pair(val.clone());
            let is_type = Value::is_true(Value::is_pair(listified).expect("ok"));
            match val.deref() {
                Value::Pair(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_pair_multi() {
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
            let is_type = Value::is_true(Value::is_pair(listified.clone()).expect("ok"));
            match val.deref() {
                Value::Pair(_) => assert!(is_type),
                _ => assert!(!is_type),
            }
        }
    }

    #[test]
    fn test_is_eq_self() {
        let val = Pair::new(
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Constant(Constant::Null)),
            true
        );
        assert!(val.borrow().is_eq(&val).expect("ok"));
    }

    #[parameterized(
        basic = { number_list(vec![1, 2, 3]), number_list(vec![1, 2, 3]) },
        nested = {
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok"),
                true
            ),
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok"),
                true
            ),
        },
    )]
    fn test_is_eq(val1: PairRef, val2: PairRef) {
        assert!(val1.borrow().is_eq(&val2).expect("ok"));
        assert!(val2.borrow().is_eq(&val1).expect("ok"));
    }

    #[parameterized(
        basic = { number_list(vec![1, 2, 3]), number_list(vec![1, 2]) },
        mutability = { number_list(vec![1, 2, 3]), make_immutable(number_list(vec![1, 2, 3])) },
    )]
    fn test_is_eq_not(val1: PairRef, val2: PairRef) {
        assert!(!val1.borrow().is_eq(&val2).expect("ok"));
        assert!(!val2.borrow().is_eq(&val1).expect("ok"));
    }

    #[parameterized(
        basic = { number_list(vec![1, 2, 3]), number_list(vec![1, 2, 3]) },
        nested = {
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok"),
                true
            ),
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok"),
                true
            ),
        },
        mutability = { number_list(vec![1, 2, 3]), make_immutable(number_list(vec![1, 2, 3])) },
    )]
    fn test_is_equal(val1: PairRef, val2: PairRef) {
        assert!(val1.borrow().is_equal(&val2).expect("ok"));
        assert!(val2.borrow().is_equal(&val1).expect("ok"));
    }

    #[parameterized(
        basic = { number_list(vec![1, 2, 3]), number_list(vec![1, 2]) },
    )]
    fn test_is_equal_not(val1: PairRef, val2: PairRef) {
        assert!(!val1.borrow().is_equal(&val2).expect("ok"));
        assert!(!val2.borrow().is_equal(&val1).expect("ok"));
    }

    #[parameterized(
        pair_single = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null)),
                true
            )),
            "(1)"
        },
        pair_dotted = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Number(Number::Int(2))),
                true
            )),
            "(1 . 2)"
        },
        pair_double = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok"),
                true
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
                        ).expect("ok")
                    ).expect("ok")
                ).expect("ok"),
                true
            )),
            "(1 2 3 4)"
        },
        pair_multi_dotted = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Value::cons(
                        Rc::new(Value::Number(Number::Int(3))),
                        Rc::new(Value::Number(Number::Int(4))),
                    ).expect("ok")
                ).expect("ok"),
                true
            )),
            "(1 2 3 . 4)"
        },
        pair_nested = {
            Value::Pair(Pair::new(
                Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![4, 5, 6]))),
                    Value::cons(
                        Rc::new(Value::Pair(number_list(vec![7, 8, 9]))),
                        Value::cons(
                            Rc::new(Value::Pair(number_list(vec![10, 11, 12]))),
                            Rc::new(Value::Constant(Constant::Null))
                        ).expect("ok")
                    ).expect("ok")
                ).expect("ok"),
                true
            )),
            "((1 2 3) (4 5 6) (7 8 9) (10 11 12))"
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
        ).expect("ok");
        assert_eq!(format!("{pair}"), "(4)");

        assert_eq!(
            Value::set_car(pair.clone(), Value::boolean(true)).expect("Could not set"),
            Value::make_const(Constant::Inert)
        );
        assert_eq!(format!("{pair}"), "(#t)");
    }

    #[test]
    fn test_set_car_bad_type() {
        let expected_error = RuntimeError::new(
            crate::errors::ErrorTypes::TypeError,
            "set-car! expects a pair"
        );
        for val in sample_values() {
            match val.deref() {
                Value::Pair(_) => (),
                _ => {
                    let err = Value::set_car(val, Value::boolean(true));
                    assert_eq!(err.expect_err("error"), expected_error);
                },
            }
        }
    }

    #[test]
    fn test_set_car_immutable() {
        let pair = Rc::new(Value::Pair(Pair::new(
            Rc::new(Value::Number(Number::Int(4))),
            Rc::new(Value::Constant(Constant::Null)),
            false,
        )));
        let expected_error = RuntimeError::new(
            crate::errors::ErrorTypes::ImmutableError,
            "This pair is immutable"
        );
        let err = Value::set_car(
            pair,
            Value::boolean(true)
        ).expect_err("should have raised an error!");
        assert_eq!(err, expected_error);
    }

    #[test]
    fn test_set_cdr() {
        let pair = Value::cons(
            Rc::new(Value::Number(Number::Int(4))),
            Rc::new(Value::Constant(Constant::Null))
        ).expect("ok");
        assert_eq!(format!("{pair}"), "(4)");

        assert_eq!(
            Value::set_cdr(pair.clone(), Value::boolean(true)).expect("Could not set"),
            Value::make_const(Constant::Inert)
        );
        assert_eq!(format!("{pair}"), "(4 . #t)");
    }

    #[test]
    fn test_set_cdr_bad_type() {
        let expected_error = RuntimeError::new(
            crate::errors::ErrorTypes::TypeError,
            "set-cdr! expects a pair"
        );
        for val in sample_values() {
            match val.deref() {
                Value::Pair(_) => (),
                _ => {
                    let err = Value::set_cdr(val, Value::boolean(true));
                    assert_eq!(err.expect_err("error"), expected_error);
                },
            }
        }
    }

    #[test]
    fn test_set_cdr_immutable() {
        let pair = Rc::new(Value::Pair(Pair::new(
            Rc::new(Value::Number(Number::Int(4))),
            Rc::new(Value::Constant(Constant::Null)),
            false,
        )));
        let expected_error = RuntimeError::new(
            crate::errors::ErrorTypes::ImmutableError,
            "This pair is immutable"
        );
        let err = Value::set_cdr(pair, Value::boolean(true)).expect_err("should have raised an error!");
        assert_eq!(err, expected_error);
    }

    #[parameterized(
        pair_single = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null)),
                true
            )),
            "(1)"
        },
        pair_dotted = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Number(Number::Int(2))),
                true
            )),
            "(1 . 2)"
        },
        pair_double = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok"),
                true
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
                        ).expect("ok")
                    ).expect("ok")
                ).expect("ok"),
                true
            )),
            "(1 2 3 4)"
        },
        pair_multi_dotted = {
            Value::Pair(Pair::new(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null)),
                true
            )),
            "(1)"
        },
        pair_nested = {
            Value::Pair(Pair::new(
                Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![4, 5, 6]))),
                    Value::cons(
                        Rc::new(Value::Pair(number_list(vec![7, 8, 9]))),
                        Value::cons(
                            Rc::new(Value::Pair(number_list(vec![10, 11, 12]))),
                            Rc::new(Value::Constant(Constant::Null))
                        ).expect("ok")
                    ).expect("ok")
                ).expect("ok"),
                true
            )),
            "((1 2 3) (4 5 6) (7 8 9) (10 11 12))"
        },
    )]
    fn test_copy_es_immutable(val: Value, expected: &str) {
        let expected_error = RuntimeError::new(crate::errors::ErrorTypes::ImmutableError, "This pair is immutable");
        let copies = Value::copy_es_immutable(Rc::new(val)).expect("should work");
        assert_eq!(format!("{copies}"), expected);

        // Go through all the pairs and make sure they return errors when modified
        if let Value::Pair(pair) = copies.deref() {
            for node in copies.iter() {
                if let Value::Pair(_) = node.deref() {
                    let err = Value::set_cdr(node, Value::boolean(true)).expect_err("should have raised an error!");
                    assert_eq!(err, expected_error);
                }
            }
        } else {
            panic!("copies is not a pair");
        }
    }

    #[parameterized(
        pair_single = {
            Value::cons(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Constant(Constant::Null)),
            ).expect("ok"),
            "(1)"
        },
        pair_dotted = {
            Value::cons(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Number(Number::Int(2))),
            ).expect("ok"),
            "(1 . 2)"
        },
        pair_double = {
            Value::cons(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok"),
            ).expect("ok"),
            "(1 2)"
        },
        pair_multi = {
            Value::cons(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Value::cons(
                        Rc::new(Value::Number(Number::Int(3))),
                        Value::cons(
                            Rc::new(Value::Number(Number::Int(4))),
                            Rc::new(Value::Constant(Constant::Null))
                        ).expect("ok")
                    ).expect("ok")
                ).expect("ok"),
            ).expect("ok"),
            "(1 2 3 4)"
        },
        pair_multi_dotted = {
            Value::cons(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Value::cons(
                        Rc::new(Value::Number(Number::Int(3))),
                        Rc::new(Value::Number(Number::Int(4))),
                    ).expect("ok")
                ).expect("ok"),
            ).expect("ok"),
            "(1 2 3 . 4)"
        },
        pair_nested = {
            Value::cons(
                Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![4, 5, 6]))),
                    Value::cons(
                        Rc::new(Value::Pair(number_list(vec![7, 8, 9]))),
                        Value::cons(
                            Rc::new(Value::Pair(number_list(vec![10, 11, 12]))),
                            Rc::new(Value::Constant(Constant::Null))
                        ).expect("ok")
                    ).expect("ok")
                ).expect("ok"),
            ).expect("ok"),
            "((1 2 3) (4 5 6) (7 8 9) (10 11 12))"
        },
    )]
    fn test_iterate(val: Rc<Value>, expected: &str) {
        let mut bits: Vec<Rc<Value>> = Vec::new();
        let mut bit = val.clone();
        while Value::is_true(Value::is_pair(Value::as_pair(bit.clone())).unwrap()) {
            bits.push(Value::car(bit.clone()).unwrap().clone());
            bit = Value::cdr(bit.clone()).unwrap();
        }
        bits.push(bit.clone());

        assert_eq!(val.iter().collect::<Vec<Rc<Value>>>(), bits);
        assert_eq!(format!("{val}"), expected);
    }

    // #[parameterized(
    //     empty = { Rc::new(Value::Constant(Constant::Null)), "()" },
    //     pair_single = {
    //         Value::cons(
    //             Rc::new(Value::Number(Number::Int(1))),
    //             Rc::new(Value::Constant(Constant::Null)),
    //         ).expect("ok"),
    //         "1"
    //     },
    //     basic_append = {
    //         Value::cons(
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(1))),
    //                 Value::cons(
    //                     Rc::new(Value::Number(Number::Int(2))),
    //                     Rc::new(Value::Constant(Constant::Null))
    //                 ).expect("ok"),
    //             ).expect("ok"),
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(3))),
    //                 Value::cons(
    //                     Rc::new(Value::Number(Number::Int(4))),
    //                     Rc::new(Value::Constant(Constant::Null))
    //                 ).expect("ok")
    //             ).expect("ok")
    //         ).expect("ok"),
    //         "(1 2 3 4)"
    //     },
    //     pair_double = {
    //         Value::cons(
    //             Rc::new(Value::Number(Number::Int(1))),
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(2))),
    //                 Rc::new(Value::Constant(Constant::Null))
    //             ).expect("ok"),
    //         ).expect("ok"),
    //         "(1 2)"
    //     },
    //     basic_list = {
    //         Value::cons(
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(1))),
    //                 Value::cons(
    //                     Rc::new(Value::Number(Number::Int(2))),
    //                     Rc::new(Value::Constant(Constant::Null))
    //                 ).expect("ok"),
    //             ).expect("ok"),
    //             Value::cons(
    //                 Value::cons(
    //                     Rc::new(Value::Number(Number::Int(3))),
    //                     Value::cons(
    //                         Rc::new(Value::Number(Number::Int(4))),
    //                         Rc::new(Value::Constant(Constant::Null))
    //                     ).expect("ok")
    //                 ).expect("ok"),
    //                 Rc::new(Value::Constant(Constant::Null))
    //             ).expect("ok"),
    //         ).expect("ok"),
    //         "(1 2 (3 4))"
    //     },
    //     pair_dotted = {
    //         Value::cons(
    //             Rc::new(Value::Number(Number::Int(1))),
    //             Rc::new(Value::Number(Number::Int(2))),
    //         ).expect("ok"),
    //         "(1 . 2)"
    //     },

    //     pair_multi_dotted = {
    //         Value::cons(
    //             Rc::new(Value::Number(Number::Int(1))),
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(2))),
    //                 Value::cons(
    //                     Rc::new(Value::Number(Number::Int(3))),
    //                     Rc::new(Value::Constant(Constant::Null)),
    //                 ).expect("ok"),
    //             ).expect("ok"),
    //         ).expect("ok"),
    //         "(1 2 . 3)"
    //     },
    //     pair_nested = {
    //         Value::cons(
    //             Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
    //             Value::cons(
    //                 Rc::new(Value::Pair(number_list(vec![4, 5, 6]))),
    //                 Value::cons(
    //                     Rc::new(Value::Pair(number_list(vec![7, 8, 9]))),
    //                     Rc::new(Value::Pair(number_list(vec![10, 11, 12]))),
    //                 ).expect("ok")
    //             ).expect("ok"),
    //         ).expect("ok"),
    //         "(1 2 3 (4 5 6) (7 8 9) (10 11 12))"
    //     },
    // )]
    // fn test_append(lists: Rc<Value>, expected: &str) {
    //     let appended = Value::append(lists).expect("should work");
    //     assert_eq!(format!("{appended}"), expected)
    // }

}
