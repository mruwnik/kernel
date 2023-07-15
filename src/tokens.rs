use std::io;
use crate::lexemes::{Lexeme, SpecialLexeme};

fn make_error<T>(err: String) -> Result<T, io::Error> {
    Result::Err(io::Error::new(io::ErrorKind::InvalidData, err))
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Constant {
    True,
    False,
    Inert,
    Ignore,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Symbol(String),
    Special(SpecialLexeme),
    Constant(Constant),
    Char(char),
    String(String),
    Number(Number),
}

fn parse_number(lexeme: &str) -> Result<Token, io::Error> {
    Ok(Token::Number({
        if lexeme.contains('.') {
            let num = lexeme.parse::<f64>().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            Number::Float(num)
        } else {
            let num = lexeme.parse::<i64>().map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
            Number::Int(num)
        }
    }))
}

pub fn parse_token(lexeme: &Lexeme) -> Result<Token, io::Error> {
    Ok(match lexeme {
        Lexeme::String(v) => Token::String(v.clone()),
        Lexeme::Special(v) => Token::Special(v.clone()),
        Lexeme::Symbol(val) => {
            match val.to_lowercase().as_str() {
                "+" | "-" => Token::Symbol(val.clone()),
                "." => Token::Special(SpecialLexeme::FullStop),

                // Constants
                "#t" => Token::Constant(Constant::True),
                "#f" => Token::Constant(Constant::False),
                "#inert" => Token::Constant(Constant::Inert),
                "#ignore" => Token::Constant(Constant::Ignore),
                v if v.len() == 3 && v.starts_with("#\\") => Token::Char(val.chars().nth(2).unwrap()),
                v if v.starts_with("#") => return make_error(format!("Unknown constant found: {v}")),

                v => match val.chars().next().unwrap() {
                    '+' | '-' => parse_number(v)?,
                    c if c.is_digit(10) => parse_number(v)?,
                    _ => Token::Symbol(v.to_string()),
                },
            }
        },
    })
}

#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::lexemes::{Lexeme, SpecialLexeme};
    use crate::tokens::{parse_token, Token, Number, Constant};

    #[parameterized(
        left_param = { SpecialLexeme::LeftParam },
        right_param = { SpecialLexeme::RightParam },
    )]
    fn test_get_params_tokens(lexeme: SpecialLexeme) {
        assert_eq!(parse_token(&Lexeme::Special(lexeme)).ok().unwrap(), Token::Special(lexeme));
    }

    #[test]
    fn test_get_special_tokens() {
        assert_eq!(
            parse_token(&Lexeme::Symbol(".".to_string())).ok().unwrap(),
            Token::Special(SpecialLexeme::FullStop)
        );
    }

    #[parameterized(
        constant_true = { "#t", Constant::True },
        constant_false = { "#F", Constant::False },
        constant_inert = { "#INerT", Constant::Inert },
        constant_ignore = { "#iGnOre", Constant::Ignore },
    )]
    fn test_get_contant_tokens(val: &str, constant: Constant) {
        assert_eq!(
            parse_token(&Lexeme::Symbol(format!("{}", val))).ok().unwrap(),
            Token::Constant(constant)
        );
    }

    #[parameterized(
        char_lowercase = { "#\\a", 'a' },
        char_uppercase = { "#\\A", 'A' },
    )]
    fn test_get_char_tokens(val: &str, c: char) {
        assert_eq!(
            parse_token(&Lexeme::Symbol(format!("{}", val))).unwrap(),
            Token::Char(c)
        );
    }

    #[parameterized(
        unknown_single_char = { "#a" },
        unknown = { "#blablabla" },
    )]
    fn test_get_unknonwn_constants(val: &str) {
        let err = parse_token(&Lexeme::Symbol(format!("{val}"))).unwrap_err();
        assert_eq!(err.to_string(), format!("Unknown constant found: {val}"));
    }

    #[parameterized(
        basic_string = { "\"blaBlabla\"" },
        funky_string = { "\"bla Blablan \n \n  \t asdasdad\"" },
    )]
    fn test_get_string_tokens(chars: &str) {
        let string = chars.to_string();
        assert_eq!(
            parse_token(&Lexeme::String(string.clone())).ok().unwrap(),
            Token::String(string.clone())
        );
    }

    #[parameterized(
        basic_int = { "123", Number::Int(123) },
        signed_int = { "+31", Number::Int(31) },
        negative_int = { "-35", Number::Int(-35) },
        basic_float = { "0.123", Number::Float(0.123) },
        postitive_float = { "+5.123", Number::Float(5.123) },
        negative_float = { "-3.123", Number::Float(-3.123) },
    )]
    fn test_get_number_tokens(chars: &str, num: Number) {
        assert_eq!(
            parse_token(&Lexeme::Symbol(chars.to_string())).ok().unwrap(),
            Token::Number(num)
        );
    }

    #[parameterized(
        bad_number_1 = { "+da123" },
        bad_number_2 = { "123dasasd31132" },
        bad_number_4 = { "-123dsa12" },
        bad_number_5 = { "+123fsd" },
    )]
    fn test_get_bad_numbers(val: &str) {
        let err = parse_token(&&Lexeme::Symbol(val.to_string())).unwrap_err();
        assert_eq!(err.to_string(), format!("invalid digit found in string"));
    }

    #[parameterized(
        bad_number_1 = { "13.31.432" },
    )]
    fn test_get_bad_floats(val: &str) {
        let err = parse_token(&&Lexeme::Symbol(val.to_string())).unwrap_err();
        assert_eq!(err.to_string(), format!("invalid float literal"));
    }

    #[parameterized(
        basic_symbol = { "bla" },
        cased_symbol = { "blaBLAbLa" },
        plus = { "+" },
        minus = { "-" },
    )]
    fn test_get_symbol_tokens(chars: &str) {
        let string = chars.to_string();
        assert_eq!(
            parse_token(&Lexeme::Symbol(string.clone())).ok().unwrap(),
            Token::Symbol(string.to_lowercase().clone())
        );
    }

}
