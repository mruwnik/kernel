use std::ops::Deref;
use std::rc::Rc;
use crate::errors::{ RuntimeError, ErrorTypes };
use crate::values::{ Value, CallResult };


impl Value {
    pub fn eval(exprs: Rc<Value>, env: Rc<Value>) -> CallResult {
        if let Value::Env(e) = env.deref() {
            match exprs.clone().deref() {
                Value::Symbol(s) => match e.borrow().get(s.clone()) {
                    None => Err(RuntimeError::new(ErrorTypes::LookupError, format!("Could not find symbol {s}"))),
                    Some(v) => Ok(v),
                },
                Value::Pair(_) => Value::call(
                    Value::eval(Value::car(exprs.clone())?, env.clone())?,
                    e.clone(),
                    Value::cdr(exprs)?
                ),
                _ => Ok(exprs),
            }
        } else {
            Err(RuntimeError::new(ErrorTypes::TypeError, "eval expects an environment as its second param") )
        }
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use std::ops::Deref;
    use crate::errors::{ RuntimeError, ErrorTypes };
    use crate::values::{ Bool, Constant, Number, Str, Symbol, Value, Combiner };
    use crate::values::envs::Env;

    // pub fn self_eval_values() -> Vec<Rc<Value>> {
    //     vec![
    //         Rc::new(Value::Bool(Bool::True)),
    //         Rc::new(Value::Bool(Bool::False)),
    //         Value::new_applicative(
    //             "tester",
    //             &|_exprs, _env| Ok(Rc::new(Value::Constant(Constant::Null))),
    //             Rc::new(Value::Constant(Constant::Null))
    //         ),
    //         Rc::new(Value::Constant(Constant::Ignore)),
    //         Rc::new(Value::Constant(Constant::Inert)),
    //         Rc::new(Value::Constant(Constant::Null)),
    //         Value::cons(
    //             Rc::new(Value::Constant(Constant::Null)),
    //             Rc::new(Value::Constant(Constant::Null)),
    //         ).unwrap(),
    //         Rc::new(Value::Env(Env::new(vec![]))),
    //         Rc::new(Value::Number(Number::Int(123))),
    //         Rc::new(Value::String(Str::new("bla"))),
    //         Rc::new(Value::Symbol(Symbol("bla".to_string()))),
    //     ]
    // }

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
        let evaled = Value::eval(val.clone(), env).expect("This should evaluate");
        assert!(Value::is_true(Value::is_eq(&val, &evaled).unwrap()));
    }

    #[test]
    fn test_eval_non_env() {
        let val = Rc::new(Value::Bool(Bool::True));
        let evaled = Value::eval(val.clone(), val).expect_err("This should fail");
        assert_eq!(evaled, RuntimeError::new(ErrorTypes::TypeError, "eval expects an environment as its second param"))
    }

    #[test]
    fn test_eval_non_env_lookup() {
        let val = Rc::new(Value::Symbol(Symbol("bla".to_string())));
        let env = Rc::new(Value::Env(Env::new(vec![])));
        let evaled = Value::eval(val.clone(), env).expect_err("This should fail");
        assert_eq!(evaled, RuntimeError::new(ErrorTypes::LookupError, "Could not find symbol bla"))
    }

    #[test]
    fn test_eval_env_lookup() {
        let val = Rc::new(Value::Symbol(Symbol("bla".to_string())));
        let env = Rc::new(Value::Env(Env::new(vec![])));
        let _ = Value::env_set(env.clone(), val.clone(), Value::make_const(Constant::Null));
        let evaled = Value::eval(val.clone(), env).expect("This should work");
        assert_eq!(evaled, Value::make_const(Constant::Null));
    }

    #[test]
    fn test_eval_call() {
        let val = Value::cons(
            Value::new_applicative(
                "tester",
                &|_exprs, _env| Ok(Rc::new(Value::Constant(Constant::Null))),
                Rc::new(Value::Constant(Constant::Null))
            ),
            Rc::new(Value::Constant(Constant::Null)),
        ).unwrap();
        let env = Rc::new(Value::Env(Env::new(vec![])));
        let evaled = Value::eval(val, env).expect("This should work");
        assert_eq!(evaled, Value::make_const(Constant::Null));
    }
}
