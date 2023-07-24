use std::ops::Deref;
use std::rc::Rc;
use crate::errors::RuntimeError;
use crate::values::{ Value, ValueResult, CallResult, CallResultType };

pub fn eval(val: Rc<Value>, env: Rc<Value>) -> ValueResult {
    let mut current = val.clone();
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
                    let func = eval(Value::car(self.into())?.into(), env.clone())?;
                    let params = Value::cdr(self.into())?;
                    Value::call(func.into(), env, params)
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

    fn list(vals: Vec<Rc<Value>>) -> Rc<Value> {
        Value::to_list(vals).unwrap()
    }

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
        let _ = env.define(val.clone(), Value::make_const(Constant::Null));
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
        single = { list(vec![Rc::new(Value::Number(Number::Int(1)))]), "1" },
        multi = { list(vec![
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Number(Number::Int(2))),
            Rc::new(Value::Number(Number::Int(3))),
            Rc::new(Value::Number(Number::Int(4))),
        ]), "10" },
    )]
    fn test_add(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("+"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[parameterized(
        empty = { Value::make_null(), "0" },
        single = { list(vec![Rc::new(Value::Number(Number::Int(1)))]).into(), "1" },
        multi = { list(vec![
            Rc::new(Value::Number(Number::Int(1))),
            Rc::new(Value::Number(Number::Int(2))),
            Rc::new(Value::Number(Number::Int(3))),
            Rc::new(Value::Number(Number::Int(4))),
        ]).into(), "-8" },
    )]
    fn test_minus(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("-"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[parameterized(
        yes = { list(vec![Value::boolean(true), Value::boolean(true)]).into(), "#t" },
        no = { list(vec![Value::boolean(true), Value::boolean(false)]).into(), "#f" },
        no_switched = { list(vec![Value::boolean(false), Value::boolean(true)]).into(), "#f" },
    )]
    fn test_eq(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("eq?"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[parameterized(
        yes = { list(vec![Value::boolean(true), Value::boolean(true)]).into(), "#t" },
        no = { list(vec![Value::boolean(true), Value::boolean(false)]).into(), "#f" },
        no_switched = { list(vec![Value::boolean(false), Value::boolean(true)]).into(), "#f" },
    )]
    fn test_equal(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("equal?"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[parameterized(
        basic = { list(vec![Value::boolean(true), Value::boolean(true)]).into(), "(#t . #t)"},
        nested_car = {
            list(vec![
                list(vec![Value::make_symbol("cons"), Value::boolean(true), Value::boolean(false)]).into(),
                Value::make_ignore(),
            ]).into(),
            "((#t . #f) . #ignore)"
        },
        nested_cdr = {
            list(vec![
                Value::make_ignore(),
                list(vec![
                    Value::make_symbol("cons"),
                    Value::boolean(true),
                    list(vec![
                        Value::make_symbol("cons"),
                        Value::boolean(false),
                        Value::make_null(),
                    ])
                ]),
            ]),
            "(#ignore #t #f)"
        },
        nested_cons = {
            list(vec![
                Number::int(1),
                list(vec![
                    Value::make_symbol("cons"),
                    Number::int(2),
                    list(vec![
                        Value::make_symbol("cons"),
                        Number::int(3),
                        list(vec![
                            Value::make_symbol("cons"),
                            Number::int(4),
                            Value::make_null(),
                        ]),
                    ]),
                ]),
            ]),
            "(1 2 3 4)"
        }
    )]
    fn test_cons(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let vals = Value::cons(Value::make_symbol("cons"), vals).unwrap();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), format!("{expected}"));
    }

    #[test]
    fn test_set_car() {
        let env = Value::ground_env();
        let target = list(vec![Value::make_symbol("cons"), Value::make_string("initial value"), Value::make_null()]);
        env.define(Value::make_symbol("target"), target.clone()).unwrap();

        let vals = list(vec![
            Value::make_symbol("set-car!"),
            Value::make_symbol("target"),
            Value::make_string("bla"),
        ]);

        assert_eq!(eval(Value::make_symbol("target"), env.clone()).unwrap().to_string(), "(\"initial value\")");
        let res = eval(vals, env.clone()).expect("ok");
        assert_eq!(res.to_string(), "#inert".to_string());
        assert_eq!(eval(Value::make_symbol("target"), env.clone()).unwrap().to_string(), "(\"bla\")");
    }

    #[test]
    fn test_set_cdr() {
        let env = Value::ground_env();
        let target = list(vec![Value::make_symbol("cons"), Value::make_string("initial value"), Value::make_null()]);
        env.define(Value::make_symbol("target"), target.clone()).unwrap();

        let vals = list(vec![
            Value::make_symbol("set-cdr!"),
            Value::make_symbol("target"),
            Value::make_string("bla"),
        ]);

        assert_eq!(eval(Value::make_symbol("target"), env.clone()).unwrap().to_string(), "(\"initial value\")");
        let res = eval(vals, env.clone()).expect("ok");
        assert_eq!(res.to_string(), "#inert".to_string());
        assert_eq!(eval(Value::make_symbol("target"), env.clone()).unwrap().to_string(), "(\"initial value\" . \"bla\")");
    }

    #[parameterized(
        cons = { list(vec![Value::make_symbol("cons"), Value::make_symbol("bla"), Value::make_string("ble")]) },
        list_ = {
            list(vec![
                Value::make_symbol("list"),
                Value::make_symbol("bla"),
                Value::make_symbol("bla"),
                Value::make_symbol("bla"),
            ])
        },
        nested_list = {
            list(vec![
                Value::make_symbol("list"),
                list(vec![Value::make_symbol("list"), Value::make_symbol("bla"), Value::make_symbol("bla")]),
                list(vec![Value::make_symbol("list"), Value::make_symbol("bla"), Value::make_symbol("bla")]),
            ])
        },
    )]
    fn test_copy_es_immutable(vals: Rc<Value>) {
        let env = Value::ground_env();
        env.define(Value::make_symbol("bla"), Value::make_string("blee")).unwrap();
        env.define(Value::make_symbol("target"), vals.clone()).unwrap();

        let expr = Value::cons(Value::make_symbol("copy-es-immutable"), Value::make_symbol("target").as_list()).unwrap();
        let res = eval(expr, env.clone()).expect("ok");
        assert_eq!(res.to_string(), eval(vals, env).expect("ok").to_string());

        let expected_error = RuntimeError::new(crate::errors::ErrorTypes::ImmutableError, "This pair is immutable");
        if let Value::Pair(_) = res.deref() {
            for node in res.iter() {
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
        basic_first = {
            list(vec![Value::make_symbol("$if"), Value::boolean(true), Number::int(1), Number::int(2),]),
            "1"
        },
        basic_second = {
            list(vec![Value::make_symbol("$if"), Value::boolean(false), Number::int(1), Number::int(2),]),
            "2"
        },
        first_eval_cond = {
            list(vec![
                Value::make_symbol("$if"),
                list(vec![
                    Value::make_symbol("boolean?"),
                    Value::boolean(true),
                ]),
                Number::int(1), Number::int(2)
            ]),
            "1"
        },
        second_eval_cond = {
            list(vec![
                Value::make_symbol("$if"),
                list(vec![
                    Value::make_symbol("boolean?"),
                    Value::make_null(),
                ]),
                Number::int(1), Number::int(2)
            ]),
            "2"
        },
        first_eval_branch = {
            list(vec![
                Value::make_symbol("$if"),
                Value::boolean(true),
                list(vec![
                    Value::make_symbol("boolean?"),
                    Value::boolean(true),
                ]),
                Number::int(2)
            ]),
            "#t"
        },
        second_eval_branch = {
            list(vec![
                Value::make_symbol("$if"),
                Value::boolean(false),
                Number::int(2),
                list(vec![
                    Value::make_symbol("boolean?"),
                    Value::boolean(true),
                ]),
            ]),
            "#t"
        },
        first_branch_other_not_evaled = {
            list(vec![
                Value::make_symbol("$if"),
                Value::boolean(true),
                Number::int(2),
                list(vec![
                    Value::make_symbol("set-car!"),
                    Value::boolean(true),
                ]),
            ]),
            "2"
        },
        second_branch_other_not_evaled = {
            list(vec![
                Value::make_symbol("$if"),
                Value::boolean(false),
                list(vec![
                    Value::make_symbol("set-car!"),
                    Value::boolean(true),
                ]),
                Number::int(2),
            ]),
            "2"
        },
    )]
    fn test_if(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        let res = eval(vals, env).expect("ok");
        assert_eq!(res.to_string(), expected);
    }

    #[test]
    fn test_make_environment() {
        let env = Value::ground_env();
        env.define(Value::make_symbol("bla"), Value::make_string("ble")).unwrap();

        let expr = Value::cons(
            Value::make_symbol("make-environment"),
            env.as_pair(),
        ).unwrap();
        let res = eval(expr, env).expect("ok");
        assert!(Value::is_env(res.as_list()).unwrap().is_true());
        assert_eq!(Value::make_symbol("bla").eval(res).unwrap().to_string(), "\"ble\"");
    }

    #[parameterized(
        basic_pair = { Value::to_list(vec![
            Value::make_symbol("cons"), Number::int(1), Value::make_null()
        ]).unwrap(), "1" },
        basic_list = { Value::to_list(vec![
            Value::make_symbol("cons"),
            Number::int(1),
            Value::to_list(vec![
                Value::make_symbol("cons"),
                Number::int(2),
                Value::to_list(vec![
                    Value::make_symbol("cons"),
                    Number::int(3),
                    Value::to_list(vec![
                        Value::make_symbol("cons"),
                        Number::int(4),
                        Value::make_null()
                    ]).unwrap()
                ]).unwrap()
            ]).unwrap()
        ]).unwrap(), "1" },
    )]
    fn test_car(vals: Rc<Value>, expected: &str) {
        let env = Value::make_environment(Value::make_null()).unwrap();
        let expr = list(vec![Value::make_symbol("car"), vals.clone()]);
        let res = eval(expr, env).expect("ok");
        assert_eq!(res.to_string(), expected);
    }

    #[parameterized(
        basic_pair = { list(vec![
            Value::make_symbol("cons"), Number::int(1), Value::make_null()
        ]), "()" },
        basic_list = { list(vec![
            Value::make_symbol("cons"),
            Number::int(1),
            list(vec![
                Value::make_symbol("cons"),
                Number::int(2),
                list(vec![
                    Value::make_symbol("cons"),
                    Number::int(3),
                    list(vec![
                        Value::make_symbol("cons"),
                        Number::int(4),
                        Value::make_null()
                    ])
                ])
            ])
        ]), "(2 3 4)" },
    )]
    fn test_cdr(vals: Rc<Value>, expected: &str) {
        let env = Value::make_environment(Value::make_null()).unwrap();
        let expr = list(vec![Value::make_symbol("cdr"), vals.clone()]);
        let res = eval(expr, env).expect("ok");
        assert_eq!(res.to_string(), expected);
    }

    #[parameterized(
        single = { Number::int(1).as_list(), "(1)" },
        evaluated_returns = { list(vec![
            Number::int(1),
            list(vec![
                Value::make_symbol("+"), Number::int(1), Number::int(2),
            ]),
            Number::int(2),
        ]), "(1 3 2)" },
        symbols = { list(vec![
            Value::make_symbol("bla"),
            Value::make_symbol("bla"),
        ]),
                    "(\"bla\" \"bla\")" },
        symbols_list = { Value::cons(
            list(vec![
                Value::make_symbol("cons"),
                Value::make_symbol("bla"),
                list(vec![
                    Value::make_symbol("cons"),
                    Value::make_symbol("bla"),
                    Value::make_null(),
                ])
            ]),
            Value::make_null(),
        ).unwrap(), "((\"bla\" \"bla\"))" },
        nested_list = { Value::cons(
            list(vec![
                Value::make_symbol("list"),
                Value::make_symbol("bla"),
                Value::make_symbol("bla"),
            ]),
            Value::make_null(),
        ).unwrap(), "((\"bla\" \"bla\"))" },
    )]
    fn test_list(vals: Rc<Value>, expected: &str) {
        let env = Value::make_environment(Value::make_null()).unwrap();
        env.define(Value::make_symbol("bla"), Value::make_string("bla")).unwrap();
        let expr = Value::cons(Value::make_symbol("list"), vals.clone()).unwrap();

        let res = eval(expr, env).expect("ok");
        assert_eq!(res.to_string(), expected);
    }

    #[parameterized(
        null = { Value::make_null(), Value::make_null(), vec![] },
        ignore = { Value::make_ignore(), Value::make_ignore(), vec![] },
        symbol = { Value::make_symbol("bla"), Number::int(1), vec![(Value::make_symbol("bla"), Number::int(1))] },
        pair = {
            Value::cons(
                Value::make_symbol("bla"),
                Value::make_symbol("ble")
            ).unwrap(),
            list(vec![
                Value::make_symbol("cons"),
                Number::int(1),
                Number::int(2),
            ]),
            vec![
                (Value::make_symbol("bla"), Number::int(1)),
                (Value::make_symbol("ble"), Number::int(2)),
            ]
        },
        nested = {
            Value::cons(
                Value::cons(
                    Value::make_symbol("sym1"),
                    Value::cons(
                        Value::make_symbol("sym2"),
                        Value::make_symbol("sym3")
                    ).unwrap()
                ).unwrap(),
                Value::cons(
                    Value::cons(
                        Value::make_symbol("sym4"),
                        Value::make_null()
                    ).unwrap(),
                    Value::make_symbol("sym5")
                ).unwrap()
            ).unwrap(),
            list(vec![
                Value::make_symbol("cons"),
                list(vec![
                    Value::make_symbol("cons"),
                    Number::int(1),
                    list(vec![
                        Value::make_symbol("cons"),
                        Number::int(2),
                        Number::int(3),
                    ]),
                ]),
                list(vec![
                    Value::make_symbol("cons"),
                    list(vec![
                        Value::make_symbol("cons"),
                        Number::int(4),
                        Value::make_null()
                    ]),
                    Number::int(5),
                ])
            ]),
            vec![
                (Value::make_symbol("sym1"), Number::int(1)),
                (Value::make_symbol("sym2"), Number::int(2)),
                (Value::make_symbol("sym3"), Number::int(3)),
                (Value::make_symbol("sym4"), Number::int(4)),
                (Value::make_symbol("sym5"), Number::int(5)),
            ]
        },
    )]
    fn test_define(defined: Rc<Value>, exprs: Rc<Value>, expected: Vec<(Rc<Value>, Rc<Value>)>) {
        let env = Value::ground_env();
        let expr = Value::to_list(vec![Value::make_symbol("$define!"), defined, exprs]).unwrap();

        let res = eval(expr, env.clone()).expect("ok");
        assert_eq!(res.to_string(), "#inert".to_string());

        let symbols = Value::to_list(expected.iter().map(|(s, _)| s.clone()).collect()).unwrap();
        let res = eval(Value::cons(Value::make_symbol("list"), symbols).unwrap(), env).expect("ok");
        let symbols = expected.iter().map(|(_, v)| v.to_string()).collect::<Vec<String>>().join(" ");
        assert_eq!(res.to_string(), format!("({symbols})"));
    }

    #[parameterized(
        basic_returns = { Value::make_null(), "()" },
        evaluated_returns = { Value::to_list(vec![
            Value::make_symbol("+"), Number::int(1), Number::int(2),
        ]).unwrap(), "3" },
        // TODO: add tests
        // symbols = { Value::make_symbol("bla"), "\"ble\"" },
    )]
    fn test_eval(vals: Rc<Value>, expected: &str) {
        let env = Value::make_environment(Value::make_null()).unwrap();
        env.define(Value::make_symbol("bla"), Value::make_string("ble")).unwrap();
        let expr = Value::to_list(vec![Value::make_symbol("eval"), vals.clone(), env.clone()]).unwrap();

        let res = eval(expr, Value::ground_env()).expect("ok");
        assert_eq!(res.to_string(), expected);
    }
}
