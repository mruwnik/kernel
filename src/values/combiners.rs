use std::{fmt, ops::Deref};
use std::rc::Rc;
use crate::errors::{ RuntimeError, ErrorTypes };
use crate::values::{ Value, ValueResult, CallResult, Symbol, CallResultType, is_val };
use crate::values::eval::eval;

use super::envs::EnvRef;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CombinerType {
    Operative,
    Applicative,
}

type Func = &'static dyn Fn(Rc<Value>, EnvRef) -> CallResult;
type ValueFunc = &'static dyn Fn(Rc<Value>, Rc<Value>) -> ValueResult;
type Method = &'static dyn Fn(Rc<Value>) -> ValueResult;

#[derive(Clone)]
pub struct Combiner {
    c_type: CombinerType,
    expr: Rc<Value>,
    name: String,
    func: Func,
}

impl fmt::Display for Combiner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#func({} {})", self.name, self.expr)
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

pub fn two_val_fn(params: Vec<Rc<Value>>, name: impl Into<String>, func: ValueFunc) -> ValueResult {
    match &params[..] {
        [val1, val2] => {
            func(val1.clone(), val2.clone())
        },
        _ => RuntimeError::type_error(format!("{} requires 2 arguments", name.into())),
    }
}


fn method(val: Rc<Value>, func: Method) -> ValueResult {
    func(val)?.car()
}

impl Combiner {
    fn new(name: impl Into<String>, func: Func, expr: Rc<Value>, c_type: CombinerType) -> Rc<Value> {
        Rc::new(Value::Combiner(Combiner { c_type, name: name.into(), func, expr }))
    }

    pub fn is_eq(self: &Self, other: &Self) -> Result<bool, RuntimeError> {
        let same_exprs = Value::is_eq(self.expr.clone(), other.expr.clone())?.is_true();
        Ok(self.name == other.name && self.c_type == other.c_type && same_exprs)
    }

    // proper underlying combiner implementations
    fn if_(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let env_val = Rc::new(Value::Env(env.clone()));
        let params = vals.operands()?;
        match &params[..] {
            [test, branch1, branch2] => {
                if eval(test.clone(), env_val.clone())?.is_true() {
                    branch1.eval(env_val)
                } else {
                    branch2.eval(env_val)
                }
            },
            _ => RuntimeError::type_error("$if requires 3 arguments"),
        }
    }

    fn eval(vals: Rc<Value>, _: EnvRef) -> CallResult {
        let params = vals.operands()?;
        match &params[..] {
            [val1, val2] => {
                val1.eval(val2.clone())
            },
            _ => RuntimeError::type_error(format!("eval requires 2 arguments")),
        }
    }

    fn define(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let env_val = Rc::new(Value::Env(env.clone()));
        let params = vals.operands()?;
        match &params[..] {
            [val1, val2] => {
                env_val.define(val1.clone(), val2.clone())?.as_val()
            },
            _ => RuntimeError::type_error(format!("$define! requires 2 arguments")),
        }
    }

    pub fn bind_ground(env: &EnvRef) {
        fn bind(env: &EnvRef, name: impl Into<String>, type_: CombinerType, func: Func) {
            let name = name.into();
            env.borrow_mut().bind(
                Symbol(name.clone()),
                Combiner::new(name, func, Value::make_null(), type_)
            );
        }

        // primatives
        bind(&env, "$if", CombinerType::Operative, &Combiner::if_);
        bind(&env, "eq?", CombinerType::Operative, &|vals, _| two_val_fn(vals.operands()?, "eq?", &Value::is_eq)?.as_val());
        bind(&env, "equal?", CombinerType::Operative, &|vals, _| two_val_fn(vals.operands()?, "equal?", &Value::is_equal)?.as_val());
        bind(&env, "cons", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "cons", &Value::cons)?.as_val());
        bind(&env, "set-car!", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "set-car!", &Value::set_car)?.as_val());
        bind(&env, "set-cdr!", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "set-cdr!", &Value::set_cdr)?.as_val());
        bind(&env, "copy-es-immutable", CombinerType::Applicative, &|vals, _| method(vals, &Value::copy_es_immutable)?.as_val());
        bind(&env, "make-environment", CombinerType::Applicative, &|vals, _| Value::make_environment(vals)?.as_val());
        bind(&env, "eval", CombinerType::Applicative, &Combiner::eval);
        bind(&env, "$define!", CombinerType::Operative, &Combiner::define);

        // Type checkers
        bind(&env, "boolean?", CombinerType::Operative, &|vals, _| Value::is_boolean(vals)?.as_val());
        bind(&env, "applicative?", CombinerType::Operative, &|vals, _| Value::is_applicative(vals)?.as_val());
        bind(&env, "operative?", CombinerType::Operative, &|vals, _| Value::is_operative(vals)?.as_val());
        bind(&env, "inert?", CombinerType::Operative, &|vals, _| Value::is_inert(vals)?.as_val());
        bind(&env, "ignore?", CombinerType::Operative, &|vals, _| Value::is_ignore(vals)?.as_val());
        bind(&env, "null?", CombinerType::Operative, &|vals, _| Value::is_null(vals)?.as_val());
        bind(&env, "env?", CombinerType::Operative, &|vals, _| Value::is_env(vals)?.as_val());
        bind(&env, "number?", CombinerType::Operative, &|vals, _| Value::is_number(vals)?.as_val());
        bind(&env, "pair?", CombinerType::Operative, &|vals, _| Value::is_pair(vals)?.as_val());
        bind(&env, "string?", CombinerType::Operative, &|vals, _| Value::is_string(vals)?.as_val());
        bind(&env, "symbol?", CombinerType::Operative, &|vals, _| Value::is_symbol(vals)?.as_val());

        // library
        bind(&env, "+", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::add, "+"));
        bind(&env, "-", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::minus, "-"));
        bind(&env, "car", CombinerType::Applicative, &|vals, _| vals.car()?.car()?.as_val());
        bind(&env, "cdr", CombinerType::Applicative, &|vals, _| vals.car()?.cdr()?.as_val());
        bind(&env, "list", CombinerType::Applicative, &|vals, _| vals.as_val());
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

    fn arguments(&self, env: Rc<Value>) -> Result<Vec<Rc<Value>>, RuntimeError> {
        let mut params = self.iter()
            .map(|v| eval(v, env.clone()))
            .collect::<Result<Vec<Rc<Value>>, RuntimeError>>()?;
        params.pop();
        Ok(params)
    }

    fn operands(&self) -> Result<Vec<Rc<Value>>, RuntimeError> {
        let mut params: Vec<Rc<Value>> = self.iter().collect();
        params.pop();
        Ok(params)
    }

    pub fn call(fun: Rc<Value>, env: Rc<Value>, params: Rc<Value>) -> CallResult {
        if let Value::Env(e) = env.deref() {
            match fun.deref() {
                Value::Combiner(Combiner{ c_type: CombinerType::Operative, func, ..}) => {
                    func(params, e.clone())
                },
                Value::Combiner(Combiner{ c_type: CombinerType::Applicative, func, ..}) => {
                    let args = params.arguments(env.clone())?;
                    func(Value::to_list(args)?, e.clone())
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
    use crate::values::{ Constant, Number, Value, tests::sample_values };
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
    fn test_is_combiner_multi() {
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
        yes = { Value::to_list(
            vec![Value::boolean(true), Value::make_null(), Value::make_ignore()]).unwrap().into(), "()"
        },
        no = { Value::to_list(vec![
            Value::boolean(false), Value::make_null(), Value::make_ignore(),
        ]).unwrap().into(), "#ignore" },
    )]
    fn test_if(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        if let Value::Env(env_obj) = env.deref() {
            let func = eval(Combiner::if_(vals, env_obj.clone()).unwrap().into(), env).expect("ok");
            assert_eq!(func.to_string(), format!("{expected}"));
        }
    }

}
