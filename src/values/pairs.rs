use std::{fmt, ops::Deref};
use std::rc::Rc;
use crate::values::{Value, Constant};

#[derive(Debug, Clone, PartialEq)]
pub struct Pair(Rc<Value>, Rc<Value>);

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
                Value::Pair(p) => Some(Rc::new(p.clone())),
                _ => None
            };
            Some(val.0.clone())
        } else {
            None
        }
    }
}

impl fmt::Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let car = &self.0;
        let cdr = &self.1;
        write!(f, "({car}{})", match cdr.deref() {
            Value::Pair(p) => {
                let mut string = String::new();
                for item in p.iter() {
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
    pub fn new(car: Rc<Value>, cdr: Rc<Value>) -> Pair {
        Pair(car, cdr)
    }

    pub fn is_eq(self: &Self, other: &Self) -> bool {
        self.0.is_eq(&other.0.clone()) == Value::boolean(true) &&
            self.1.is_eq(&other.1.clone()) == Value::boolean(true)
    }

    pub fn is_equal(self: &Self, other: &Self) -> bool {
        self.0.is_equal(&other.0.clone()) == Value::boolean(true) &&
            self.1.is_equal(&other.1.clone()) == Value::boolean(true)
    }

    pub fn cdr(self: &Self) -> Rc<Value> {
        self.1.clone()
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
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use crate::values::{ Bool, Constant, Env, Number, Str, Symbol, Value };
    use crate::values::pairs::{Pair, PairIter};

    fn make_list(mut items: Vec<Rc<Value>>) -> Pair {
        items.reverse();
        let mut iterator = items.iter();
        let i = iterator.next().unwrap();
        let mut node = Pair::new(i.clone(), Rc::new(Value::Constant(Constant::Null)));
        for i in iterator {
            node = Pair::new(i.clone(), Rc::new(Value::Pair(node)));
        }
        node
    }
    fn number_list(items: Vec<i64>) -> Pair {
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
        assert!(val.is_eq(&val));
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
    fn test_is_eq(val1: Pair, val2: Pair) {
        assert!(val1.is_eq(&val2));
        assert!(val2.is_eq(&val1));
    }

    #[parameterized(
        basic = { number_list(vec![1, 2, 3]), number_list(vec![1, 2]) },
    )]
    fn test_is_eq_not(val1: Pair, val2: Pair) {
        assert!(!val1.is_eq(&val2));
        assert!(!val2.is_eq(&val1));
    }

}
