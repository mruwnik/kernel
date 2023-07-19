use std::{fmt, ops::Deref};
use std::rc::Rc;
use crate::errors::{ RuntimeError, ErrorTypes };
use crate::values::{ Value, ValueResult, CallResult, CallResultType, is_val };
use crate::values::eval::eval;

use super::envs::EnvRef;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum CombinerType {
    Operative,
    Applicative,
}

type Func = &'static dyn Fn(Rc<Value>, EnvRef) -> CallResult;
#[derive(Clone)]
pub struct Combiner {
    c_type: CombinerType,
    expr: Rc<Value>,
    name: String,
    func: Func,
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

pub fn number_applicative(vals: Rc<Value>, func: &dyn Fn(Rc<Value>) -> ValueResult, symbol: impl Into<String>) -> CallResult {
    let res: Rc<Value> = func(vals.clone())?.into();
    match res.deref() {
        Value::Number(_) => res.as_val(),
        Value::Pair(_) => Value::cons(
            Value::make_symbol(symbol),
            res,
        )?.as_tail_call(),
        _ => RuntimeError::type_error("+ can only handle numbers and lists"),
    }
}

impl Combiner {
    fn new(name: impl Into<String>, func: Func, expr: Rc<Value>, c_type: CombinerType) -> Rc<Value> {
        Rc::new(Value::Combiner(Combiner { c_type, name: name.into(), func, expr }))
    }

    pub fn is_eq(self: &Self, other: &Self) -> Result<bool, RuntimeError> {
        let same_exprs = Value::is_eq(&self.expr, &other.expr)?.is_true();
        Ok(self.name == other.name && self.c_type == other.c_type && same_exprs)
    }

    // proper underlying combiner implementations
    pub fn add(vals: Rc<Value>, _: EnvRef) -> CallResult {
        number_applicative(vals, &Value::add, "+")
    }

    pub fn minus(vals: Rc<Value>, _: EnvRef) -> CallResult {
        number_applicative(vals, &Value::minus, "-")
    }
}


impl Value {
    // helpers
    pub fn new_applicative(name: impl Into<String>, func: Func, expr: Rc<Value>) -> Rc<Value> {
        Combiner::new(name, func, expr, CombinerType::Applicative)
    }

    pub fn new_operative(name: impl Into<String>, func: Func, expr: Rc<Value>) -> Rc<Value> {
        Combiner::new(name, func, expr, CombinerType::Operative)
    }

    pub fn call(fun: Rc<Value>, env: Rc<Value>, params: Rc<Value>) -> CallResult {
        if let Value::Env(e) = env.deref() {
            match fun.deref() {
                Value::Combiner(Combiner{ c_type: CombinerType::Operative, func, ..}) => {
                    func(params, e.clone())
                },
                Value::Combiner(Combiner{ c_type: CombinerType::Applicative, func, ..}) => {
                    let mut params = params.iter()
                        .map(|v| eval(v, env.clone()))
                        .collect::<Result<Vec<Rc<Value>>, RuntimeError>>()?;
                    params.pop();
                    func(Value::to_list(params)?, e.clone())
                },
                _ => Err(RuntimeError::new(ErrorTypes::TypeError, "eval tried to call a non combiner")),
            }
        } else {
            RuntimeError::type_error("eval got a non environment")
        }
    }

    // primatives
    pub fn is_operative(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Combiner(Combiner{ c_type: CombinerType::Operative, .. })))
    }

    pub fn is_applicative(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Combiner(Combiner{ c_type: CombinerType::Applicative, .. })))
    }

    pub fn as_tail_call(&self) -> CallResult {
        Ok(CallResultType::Call(self.into()))
    }

    pub fn as_val(&self) -> CallResult {
        Ok(CallResultType::Value(self.into()))
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use std::ops::Deref;
    use crate::values::symbols::Symbol;
    use crate::values::{ Constant, Number, Value, tests::sample_values };
    use crate::values::envs::Env;
    use crate::values::eval::eval;
    use crate::values::Combiner;

    use super::CombinerType;

    #[test]
    fn test_is_combiner() {
        for val in sample_values() {
            let listified = val.as_pair();
            let is_applicative = Value::is_applicative(listified.clone()).expect("ok").is_true();
            let is_operative = Value::is_operative(listified.clone()).expect("ok").is_true();
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
                        val.as_pair()
                    ).unwrap().into(),
                ).unwrap().into(),
            ).unwrap();
            let is_applicative = Value::is_applicative(listified.clone().into()).expect("ok").is_true();
            let is_operative = Value::is_operative(listified.into()).expect("ok").is_true();
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
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        operative = {
            Combiner{
                name: "operative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
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
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "applicative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        operatives = {
            Combiner{
                name: "operative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            },
            Combiner{
                name: "operative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
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
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "applicative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        applicatives_differnt_expr = {
            Combiner{
                name: "applicative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Inert)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "applicative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            }
        },
        operatives_diff_names = {
            Combiner{
                name: "operativebla".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            },
            Combiner{
                name: "operative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
        operatives_diff_exprs = {
            Combiner{
                name: "operative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Inert)),
                c_type: CombinerType::Operative,
            },
            Combiner{
                name: "operative".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
        different_types = {
            Combiner{
                name: "bla".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Applicative,
            },
            Combiner{
                name: "bla".to_string(),
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
                expr: Rc::new(Value::Constant(Constant::Null)),
                c_type: CombinerType::Operative,
            }
        },
    )]
    fn test_is_eq_not(val1: Combiner, val2: Combiner) {
        assert!(!val1.is_eq(&val2).unwrap());
        assert!(!val2.is_eq(&val1).unwrap());
    }

    #[parameterized(
        empty = { Value::make_null(), "0" },
        single = { Value::to_list(vec![Rc::new(Value::Number(Number::Int(1)))]).unwrap().into(), "1" },
        multi = { Value::to_list(vec![
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Number(Number::Int(2))),
            Rc::new(Value::Number(Number::Int(3))),
            Rc::new(Value::Number(Number::Int(4))),
        ]).unwrap().into(), "10" },
    )]
    fn test_add(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        if let Value::Env(env_obj) = env.deref() {
            let adder = eval(Combiner::add(vals, env_obj.clone()).unwrap().into(), env).expect("ok");
            assert_eq!(adder.to_string(), format!("{expected}"));
        }
    }

    #[parameterized(
        empty = { Value::make_null(), "0" },
        single = { Value::to_list(vec![Rc::new(Value::Number(Number::Int(1)))]).unwrap().into(), "1" },
        multi = { Value::to_list(vec![
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Number(Number::Int(2))),
            Rc::new(Value::Number(Number::Int(3))),
            Rc::new(Value::Number(Number::Int(4))),
        ]).unwrap().into(), "-8" },
    )]
    fn test_minus(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        if let Value::Env(env_obj) = env.deref() {
            let subtractor = eval(Combiner::minus(vals, env_obj.clone()).unwrap().into(), env).expect("ok");
            assert_eq!(subtractor.to_string(), format!("{expected}"));
        }
    }
}
