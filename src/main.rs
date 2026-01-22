use std::env;
use std::rc::Rc;

use orxl::errors::RuntimeError;
use orxl::lexemes;
use orxl::tokens;
use orxl::values::{CallResult, Value};
use orxl::values::eval::eval;

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
    let args: Vec<String> = env::args().skip(1).collect();

    if args.iter().any(|a| a == "--version" || a == "-V") {
        println!("orxl/kernel {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    args.iter().for_each(|s| match rep(s) {
        Ok(_) => (),
        Err(e) => println!("{e}"),
    });
}
