use std::ops::Deref;
use std::rc::Rc;
use crate::errors::RuntimeError;
use crate::values::{ Value, ValueResult, CallResult, CallResultType };

pub fn eval(val: Rc<Value>, env: Rc<Value>) -> ValueResult {
    let mut current = val;
    loop {
        match current.eval(env.clone())? {
            CallResultType::Value(v) => {
                return v.ok()
            },
            CallResultType::Call(c) => {
                current = c;
            }
        }
    }
}

impl Value {
    pub fn eval(&self, env: Rc<Value>) -> CallResult {
        if let Value::Env(e) = env.deref() {
            match self.clone() {
                // If it's a symbol find it in the env and return it
                Value::Symbol(s) => match e.borrow().get(s.clone()) {
                    None => RuntimeError::lookup_error(format!("Could not find symbol {s}")),
                    Some(v) => v.as_val(),
                },
                // pairs should be evaluated such that the car gets evaluated and then is called with the cdr
                Value::Pair(_) => {
                    let func = eval(Value::car(self.into())?.into(), env.clone())?.into();
                    let params = Value::cdr(self.into())?.into();
                    Value::call(func, env, params)
                },
                // anything else should just be returned
                _ => self.as_val(),
            }
        } else {
            RuntimeError::type_error("eval expects an environment as its second param")
        }
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::ops::Deref;
    use std::rc::Rc;
    use crate::errors::{ RuntimeError, ErrorTypes };
    use crate::values::{ Bool, Constant, Number, Str, Symbol, Value };
    use crate::values::envs::Env;
    use crate::values::tests::sample_values;
    use super::eval;

    #[parameterized(
        true_ = { Rc::new(Value::Bool(Bool::True)) },
        false_ = { Rc::new(Value::Bool(Bool::False)) },
        ignore = { Rc::new(Value::Constant(Constant::Ignore)) },
        inert = { Rc::new(Value::Constant(Constant::Inert)) },
        null = { Rc::new(Value::Constant(Constant::Null)) },
        env = { Rc::new(Value::Env(Env::new(vec![]))) },
        number = { Rc::new(Value::Number(Number::Int(123))) },
        string = { Rc::new(Value::String(Str::new("bla"))) },
    )]
    fn test_eval_self(val: Rc<Value>) {
        let env = Rc::new(Value::Env(Env::new(vec![])));
        let evaled = val.eval(env).expect("This should evaluate");
        assert!(Value::is_eq(val, evaled.into()).unwrap().is_true());
    }

    #[test]
    fn test_eval_non_env() {
        let val = Rc::new(Value::Bool(Bool::True));
        let evaled = val.eval(val.clone()).expect_err("This should fail");
        assert_eq!(evaled, RuntimeError::new(ErrorTypes::TypeError, "eval expects an environment as its second param"))
    }

    #[test]
    fn test_eval_non_env_lookup() {
        let val = Rc::new(Value::Symbol(Symbol("bla".to_string())));
        let env = Rc::new(Value::Env(Env::new(vec![])));
        let evaled = val.eval(env).expect_err("This should fail");
        assert_eq!(evaled, RuntimeError::new(ErrorTypes::LookupError, "Could not find symbol bla"))
    }

    #[test]
    fn test_eval_env_lookup() {
        let val = Rc::new(Value::Symbol(Symbol("bla".to_string())));
        let env = Rc::new(Value::Env(Env::new(vec![])));
        let _ = Value::env_set(env.clone(), val.clone(), Value::make_const(Constant::Null));
        let evaled: Rc<Value> = val.eval(env).expect("This should work").into();
        assert_eq!(evaled, Value::make_const(Constant::Null));
    }

    #[test]
    fn test_eval_call() {
        let val: Rc<Value> = Value::cons(
            Value::new_applicative(
                "tester",
                &|_exprs, _env| Value::Constant(Constant::Null).as_val(),
                Rc::new(Value::Constant(Constant::Null))
            ),
            Rc::new(Value::Constant(Constant::Null)),
        ).unwrap().into();
        let env = Rc::new(Value::Env(Env::new(vec![])));
        let evaled: Rc<Value> = val.eval(env).expect("This should work").into();
        assert_eq!(evaled, Value::make_const(Constant::Null));
    }

    #[test]
    fn test_is_type() {
        let env = Value::ground_env();
        fn checker(name: impl Into<String>, vals: Rc<Value>, env: Rc<Value>) {
            let expr = Value::cons(Value::make_symbol(name), vals).unwrap();
            assert!(eval(expr, env).expect("da").is_true());
        }
        for val in sample_values() {
            let vals = val.as_pair();
            match val.deref() {
                Value::Bool(_) => checker("boolean?", vals, env.clone()),
                Value::Constant(Constant::Ignore) => checker("ignore?", vals, env.clone()),
                Value::Constant(Constant::Inert) => checker("inert?", vals, env.clone()),
                Value::Constant(Constant::Null) => checker("null?", vals, env.clone()),
                Value::Env(_) => checker("env?", vals, env.clone()),
                Value::Number(_) => checker("number?", vals, env.clone()),
                Value::Pair(_) => checker("pair?", vals, env.clone()),
                Value::String(_) => checker("string?", vals, env.clone()),
                Value::Symbol(_) => checker("symbol?", vals, env.clone()),
                Value::Combiner(_) => if Value::is_applicative(vals.clone()).unwrap().is_true() {
                    checker("applicative?", vals, env.clone())
                } else {
                    checker("operative?", vals, env.clone())
                }
            }
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
        ]).unwrap().into(), "10" },
    )]
    fn test_add(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("+"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
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
        let vals = Value::cons(Value::make_symbol("-"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[parameterized(
        yes = { Value::to_list(vec![Value::boolean(true), Value::boolean(true)]).unwrap().into(), "#t" },
        no = { Value::to_list(vec![Value::boolean(true), Value::boolean(false)]).unwrap().into(), "#f" },
        no_switched = { Value::to_list(vec![Value::boolean(false), Value::boolean(true)]).unwrap().into(), "#f" },
    )]
    fn test_eq(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("eq?"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[parameterized(
        yes = { Value::to_list(vec![Value::boolean(true), Value::boolean(true)]).unwrap().into(), "#t" },
        no = { Value::to_list(vec![Value::boolean(true), Value::boolean(false)]).unwrap().into(), "#f" },
        no_switched = { Value::to_list(vec![Value::boolean(false), Value::boolean(true)]).unwrap().into(), "#f" },
    )]
    fn test_equal(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("equal?"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[parameterized(
        basic = { Value::to_list(vec![Value::boolean(true), Value::boolean(true)]).unwrap().into(), "(#t . #t)"},
        nested_car = {
            Value::to_list(vec![
                Value::to_list(vec![Value::boolean(true), Value::boolean(false)]).unwrap().into(),
                Value::make_ignore(),
            ]).unwrap().into(),
            "((#t #f) . #ignore)"
        },
        nested_cdr = {
            Value::to_list(vec![
                Value::make_ignore(),
                Value::to_list(vec![Value::boolean(true), Value::boolean(false)]).unwrap().into(),
            ]).unwrap().into(),
            "(#ignore #t #f)"
        },
    )]
    fn test_cons(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("cons"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }
}
