use std::{fmt, ops::Deref};
use std::rc::Rc;
use std::cell::RefCell;
use crate::errors::{RuntimeError, ErrorTypes};
use crate::values::{ ValueResult, Constant, Value, is_val };

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
                        self.node = Some(Value::cdr(val.clone()).unwrap().into());
                        Some(Value::car(val.clone()).unwrap().into())
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
                self.car().is_eq(&other.borrow().car().clone())?.is_true() &&
                self.cdr().is_eq(&other.borrow().cdr().clone())?.is_true()
        )
    }

    pub fn is_equal(self: &Self, other: &PairRef) -> Result<bool, RuntimeError> {
        Ok(
            self.car().is_equal(&other.borrow().car().clone())?.is_true() &&
                self.cdr().is_equal(&other.borrow().cdr().clone())?.is_true()
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
    pub fn as_pair(&self) -> Rc<Value> {
        Value::cons(self.into(), Value::make_const(Constant::Null)).unwrap().into()
    }

    pub fn as_list(&self) -> Rc<Value> {
        match self.clone() {
            Value::Constant(Constant::Null) => self.into(),
            Value::Pair(_) => self.into(),
            _ => Value::as_pair(self.into()),
        }
    }

    pub fn is_last(&self) -> bool {
        match self {
            Value::Pair(p) => {
                match p.borrow().cdr().deref() {
                    Value::Constant(Constant::Null) => true,
                    _ => false,
                }
            },
            _ => true
        }
    }

    pub fn last(&self) -> Rc<Value> {
        match self.clone() {
            Value::Pair(p) => {
                if self.is_last() {
                    self.into()
                } else {
                    p.borrow().cdr().last()
                }
            }
            _ => self.into(),
        }
    }

    pub fn iter(self: &Self) -> ValueIter {
        ValueIter { node: Some(Rc::new(self.clone())) }
    }

    pub fn to_list(vals: Vec<Rc<Value>>) -> ValueResult {
        let root = Value::make_null().as_pair();
        let mut last = root.clone();
        for item in vals {
            let next = item.as_pair();
            Value::set_cdr(last.clone(), next.clone())?;
            last = next;
        }
        Value::cdr(root)
    }

    // primatives
    pub fn is_pair(items: Rc<Value>) -> ValueResult {
        is_val(items, &|val| matches!(val.deref(), Value::Pair(_)))
    }

    pub fn cons(val1: Rc<Value>, val2: Rc<Value>) -> ValueResult {
        Value::Pair(Pair::new(val1, val2, true)).ok()
    }

    pub fn set_car(self: Rc<Value>, val: Rc<Value>) -> ValueResult {
        if let Value::Pair(p) = self.deref() {
            p.borrow_mut().set_car(val)?;
            Value::make_inert().ok()
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "set-car! expects a pair"))
        }
    }

    pub fn set_cdr(self: Rc<Value>, val: Rc<Value>) -> ValueResult {
        if let Value::Pair(p) = self.deref() {
            p.borrow_mut().set_cdr(val)?;
            Value::make_inert().ok()
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "set-cdr! expects a pair"))
        }
    }

    pub fn copy_es_immutable(self: Rc<Value>) -> ValueResult {
        match self.deref() {
            Value::Pair(p) => Rc::new(Value::Pair(
                Pair::new(
                    p.borrow().car().copy_es_immutable()?.into(),
                    p.borrow().cdr().copy_es_immutable()?.into(),
                    false
                ))
            ),
            _ => self.clone(),
        }.ok()
    }

    // library funcs
    pub fn copy_es(self: Rc<Value>) -> ValueResult {
        match self.deref() {
            Value::Pair(p) => Rc::new(Value::Pair(
                Pair::new(
                    p.borrow().car().copy_es()?.into(),
                    p.borrow().cdr().copy_es()?.into(),
                    true
                ))
            ),
            _ => self.clone(),
        }.ok()
    }

    pub fn car(pair: Rc<Value>) -> ValueResult {
        if let Value::Pair(p) = pair.deref() {
            p.borrow().car().ok()
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "car expects a pair"))
        }
    }

    pub fn cdr(pair: Rc<Value>) -> ValueResult {
        if let Value::Pair(p) = pair.deref() {
            p.borrow().cdr().ok()
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "cdr expects a pair"))
        }
    }

    // pub fn append(lists: Rc<Value>) -> ValueResult {
    //     match lists.deref() {
    //         Value::Pair(p) => {
    //             let cdr = Value::cdr(lists.clone())?;
    //             let mut car = Value::car(lists.clone())?;
    //             println!("{car}");
    //             if let Value::Pair(_) = cdr.deref() {
    //                 car = Value::copy_es(car.clone())?.as_list();
    //                 let last = car.last();
    //                 println!("{car}");
    //                 dbg!(car.clone());
    //                 Value::set_cdr(last.clone(), cdr.clone())?;
    //                 println!("{lists}");
    //                 println!("{cdr}");
    //                 println!("{car}");
    //             }
    //             Ok(car)

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
        let copied: Rc<Value> = Value::copy_es_immutable(Rc::new(Value::Pair(p))).expect("should work").into();
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
            let listified = val.as_pair();
            let is_type = Value::is_pair(listified).expect("ok").is_true();
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
                        val.as_pair()
                    ).unwrap().into(),
                ).unwrap().into(),
            ).unwrap();
            let is_type = Value::is_pair(listified.into()).expect("ok").is_true();
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
                ).expect("ok").into(),
                true
            ),
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok").into(),
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
                ).expect("ok").into(),
                true
            ),
            Pair::new(
                Rc::new(Value::Pair(number_list(vec![5, 3, 2]))),
                Value::cons(
                    Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok").into(),
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
                ).expect("ok").into(),
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
                        ).expect("ok").into()
                    ).expect("ok").into()
                ).expect("ok").into(),
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
                    ).expect("ok").into()
                ).expect("ok").into(),
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
                        ).expect("ok").into()
                    ).expect("ok").into()
                ).expect("ok").into(),
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

        let res: Rc<Value> = Value::set_car(pair.clone().into(), Value::boolean(true)).expect("Could not set").into();
        assert_eq!(res, Value::make_const(Constant::Inert));
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

        let res: Rc<Value> = Value::set_cdr(pair.clone().into(), Value::boolean(true)).expect("Could not set").into();
        assert_eq!(res, Value::make_const(Constant::Inert));
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
                ).expect("ok").into(),
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
                        ).expect("ok").into()
                    ).expect("ok").into()
                ).expect("ok").into(),
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
                        ).expect("ok").into()
                    ).expect("ok").into()
                ).expect("ok").into(),
                true
            )),
            "((1 2 3) (4 5 6) (7 8 9) (10 11 12))"
        },
    )]
    fn test_copy_es_immutable(val: Value, expected: &str) {
        let expected_error = RuntimeError::new(crate::errors::ErrorTypes::ImmutableError, "This pair is immutable");
        let copies: Rc<Value> = Value::copy_es_immutable(Rc::new(val)).expect("should work").into();
        assert_eq!(format!("{copies}"), expected);

        // Go through all the pairs and make sure they return errors when modified
        if let Value::Pair(_) = copies.deref() {
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
            ).expect("ok").into(),
            "(1)"
        },
        pair_dotted = {
            Value::cons(
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Number(Number::Int(2))),
            ).expect("ok").into(),
            "(1 . 2)"
        },
        pair_double = {
            Value::cons(
                Rc::new(Value::Number(Number::Int(1))),
                Value::cons(
                    Rc::new(Value::Number(Number::Int(2))),
                    Rc::new(Value::Constant(Constant::Null))
                ).expect("ok").into(),
            ).expect("ok").into(),
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
                        ).expect("ok").into()
                    ).expect("ok").into()
                ).expect("ok").into(),
            ).expect("ok").into(),
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
                    ).expect("ok").into()
                ).expect("ok").into(),
            ).expect("ok").into(),
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
                        ).expect("ok").into()
                    ).expect("ok").into()
                ).expect("ok").into(),
            ).expect("ok").into(),
            "((1 2 3) (4 5 6) (7 8 9) (10 11 12))"
        },
    )]
    fn test_iterate(val: Rc<Value>, expected: &str) {
        let mut bits: Vec<Rc<Value>> = Vec::new();
        let mut bit = val.clone();
        while Value::is_pair(bit.as_pair()).unwrap().is_true() {
            bits.push(Value::car(bit.clone()).unwrap().into());
            bit = Value::cdr(bit.clone()).unwrap().into();
        }
        bits.push(bit.clone());

        assert_eq!(val.iter().collect::<Vec<Rc<Value>>>(), bits);
        assert_eq!(format!("{val}"), expected);
    }

    #[parameterized(
        empty = {vec![], "()"},
        single = {
            vec![Rc::new(Value::Number(Number::Int(1)))],
            "(1)"
        },
        multi = {
            vec![
                Rc::new(Value::Number(Number::Int(1))),
                Rc::new(Value::Number(Number::Int(2))),
                Rc::new(Value::Number(Number::Int(3))),
                Rc::new(Value::Number(Number::Int(4))),
            ],
            "(1 2 3 4)"
        },

        mulit_nested = {
            vec![
                Rc::new(Value::Pair(number_list(vec![1, 2, 3]))),
                Rc::new(Value::Pair(number_list(vec![4, 5, 6]))),
                Rc::new(Value::Pair(number_list(vec![7, 8, 9]))),
            ],
            "((1 2 3) (4 5 6) (7 8 9))"
        },
    )]
    fn test_to_list(vals: Vec<Rc<Value>>, expected: &str) {
        assert_eq!(Value::to_list(vals).expect("ok").to_string(), expected);
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
    //     to_empty = {
    //         Value::cons(
    //             Value::make_null(),
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(2))),
    //                 Value::cons(
    //                     Rc::new(Value::Number(Number::Int(2))),
    //                     Rc::new(Value::Constant(Constant::Null))
    //                 ).expect("ok"),
    //             ).expect("ok"),
    //         ).expect("ok"),
    //         "(1 2)"
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

    //     pair_null_dotted = {
    //         Value::cons(
    //             Value::make_null(),
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(2))),
    //                 Rc::new(Value::Number(Number::Int(3))),
    //             ).expect("ok"),
    //         ).expect("ok"),
    //         "(2 . 3)"
    //     },
    //     pair_multi_dotted = {
    //         Value::cons(
    //             Rc::new(Value::Number(Number::Int(1))),
    //             Value::cons(
    //                 Rc::new(Value::Number(Number::Int(2))),
    //                 Rc::new(Value::Number(Number::Int(3))),
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
    //                     Value::as_pair(Rc::new(Value::Pair(number_list(vec![10, 11, 12])))),
    //                 ).expect("ok")
    //             ).expect("ok"),
    //         ).expect("ok"),
    //         "(1 2 3 (4 5 6) (7 8 9) (10 11 12))"
    //     },
    // )]
    // fn test_append(lists: Rc<Value>, expected: &str) {
    //     println!("{expected}");
    //     let appended = Value::append(lists).expect("should work");
    //     assert_eq!(format!("{appended}"), expected)
    // }

}
