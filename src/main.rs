use crate::lexemes::Lexeme;
use crate::tokens::{parse_token, Token};
use crate::values::{Value, Env, Symbol};

pub mod lexemes;
pub mod tokens;
pub mod values;


fn main() {
    let raw = format!("(+ (- 1 2) \"asd\\\"asd\" 3 4)");
    let mut chars = raw.chars();

    // println!("{raw}");

    // let lexes = lexemes::get_lexemes(&mut chars);
    // println!("{:?}", lexes);

    // let tokens: Vec<Token> = lexes.expect("dqwed").iter().map(|l| parse_token(l).expect("Invalid token")).collect();
    // dbg!(tokens);

    // let lexeme = Lexeme::String(format!("+"));
    // dbg!(parse_token(&lexeme).expect("ad"));

    dbg!(Value::Symbol(Symbol("asdad".to_string())));

    let ground_env = Env::new(vec![]);

    let env = Env::new(vec![ground_env.clone()]);

    ground_env.borrow_mut().bind(
        Symbol("asd".to_string()),
        Value::Symbol(Symbol("val 1".to_string()))
    );
    ground_env.borrow_mut().bind(
        Symbol("bla".to_string()),
        Value::Symbol(Symbol("val 2".to_string()))
    );
    ground_env.borrow_mut().bind(
        Symbol("ble".to_string()),
        Value::Symbol(Symbol("val 3".to_string()))
    );
    env.borrow_mut().bind(
        Symbol("fewwf".to_string()),
        Value::Symbol(Symbol("val 5".to_string()))
    );
    dbg!(env);
    // println!("{}", "123".parse().unwrap())
}
