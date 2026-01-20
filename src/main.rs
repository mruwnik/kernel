pub mod lexemes;
pub mod tokens;
pub mod values;
pub mod errors;

#[cfg(test)]
mod tests;

use std::env;
use std::rc::Rc;
use errors::RuntimeError;

use crate::values::{ CallResult, Value };
use crate::values::eval::eval;

fn rep(raw: impl Into<String>) -> CallResult {
    let raw = raw.into();
    let mut chars = raw.chars();

    let lexes = lexemes::get_lexemes(&mut chars)?;
    let values = tokens::parse(lexes)?;
    let base_env = Value::ground_env();

    let results = values.iter()
        .map(|v| eval(v.clone(), base_env.clone()))
        .collect::<Result<Vec<Rc<Value>>, RuntimeError>>()?;

    results.iter().for_each(|r| println!("{r}"));

    Value::make_inert().as_val()
}

fn main() {
    env::args()
        .skip(1)
        .for_each(|s| match rep(s) {
            Ok(_) => (),
            Err(e) => println!("{e}"),
        });
}
