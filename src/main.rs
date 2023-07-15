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

    crate::values::tester();
    // println!("{}", "123".parse().unwrap())
}
