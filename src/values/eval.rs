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
                    let car: Rc<Value> = eval(Value::car(self.into())?.into(), env.clone())?;
                    Value::call(
                        car,
                        // eval the car properly
                        e.clone(),
                        Value::cdr(self.into())?.into()
                    )
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

    use std::rc::Rc;
    use crate::errors::{ RuntimeError, ErrorTypes };
    use crate::values::{ Bool, Constant, Number, Str, Symbol, Value, Combiner };
    use crate::values::envs::Env;

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
        assert!(Value::is_eq(&val, &evaled.into()).unwrap().is_true());
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
}
