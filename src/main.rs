pub mod lexemes;
pub mod tokens;
pub mod values;
pub mod errors;

use std::env;
use std::rc::Rc;
use errors::RuntimeError;
use values::ValueResult;

use crate::values::{ CallResult, CallResultType, Value };
use crate::values::combiners::Combiner;
use crate::values::eval::eval;

fn rep(raw: impl Into<String>) -> CallResult {
    let raw = raw.into();
    let mut chars = raw.chars();

    let lexes = lexemes::get_lexemes(&mut chars)?;
    let values = tokens::parse(lexes)?;
    // dbg!(values);
    let base_env = Value::ground_env();

    let printer = Value::new_applicative("printer", &|params, _env| {println!("evaling! {params}"); Value::make_inert().as_val()}, Value::make_inert());
    Value::env_set(base_env.clone(), Value::make_symbol("bla"), Value::make_string("this is from bla"))?;
    Value::env_set(base_env.clone(), Value::make_symbol("ble"), Value::make_string("this is from ble"))?;
    Value::env_set(base_env.clone(), Value::make_symbol("-"), printer.clone())?;

    let results = values.iter()
        .map(|v| eval(v.clone(), base_env.clone()))
        .collect::<Result<Vec<Rc<Value>>, RuntimeError>>()?;

    results.iter().for_each(|r| println!("{r}"));

    Value::make_inert().as_val()
}

fn main() {
    let raw = format!("1231 bla ble (+ 1 2 3 4)");
    let _ = rep(raw);

    env::args()
        .skip(1)
        .for_each(|s| match rep(s) {
            Ok(_) => (),
            Err(e) => println!("{e}"),
        });
}
