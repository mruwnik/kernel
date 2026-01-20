use std::slice::Iter;
use std::rc::Rc;
use crate::lexemes::{Lexeme, SpecialLexeme};
use crate::errors::RuntimeError;
use crate::values::{Value, self, ValueResult};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Constant {
    True,
    False,
    Inert,
    Ignore,
    Null,
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

fn parse_number(lexeme: &str) -> Result<Token, RuntimeError> {
    Ok(Token::Number({
        if lexeme.contains('.') {
            let num = lexeme.parse::<f64>().map_err(RuntimeError::from)?;
            Number::Float(num)
        } else {
            let num = lexeme.parse::<i64>().map_err(RuntimeError::from)?;
            Number::Int(num)
        }
    }))
}

pub fn parse_token(lexeme: &Lexeme) -> Result<Token, RuntimeError> {
    Ok(match lexeme {
        Lexeme::String(v) => Token::String(v.clone()),
        Lexeme::Special(v) => Token::Special(v.clone()),
        Lexeme::Symbol(val) => {
            let lower = val.to_lowercase();
            match lower.as_str() {
                "+" | "-" => Token::Symbol(val.clone()),
                "." => Token::Special(SpecialLexeme::FullStop),

                // Constants (case-insensitive matching)
                "#t" => Token::Constant(Constant::True),
                "#f" => Token::Constant(Constant::False),
                "#inert" => Token::Constant(Constant::Inert),
                "#ignore" => Token::Constant(Constant::Ignore),
                v if v.len() == 3 && v.starts_with("#\\") => Token::Char(val.chars().nth(2).unwrap()),
                v if v.starts_with("#") => return RuntimeError::parse_error(format!("Unknown constant found: {v}")),

                v => match val.chars().next().unwrap() {
                    '+' | '-' => parse_number(v)?,
                    c if c.is_digit(10) => parse_number(v)?,
                    // Preserve original case for symbols
                    _ => Token::Symbol(val.clone()),
                },
            }
        },
    })
}

fn extract_list(tokens: &mut Iter<Token>) -> ValueResult {
    let root = Value::make_null().as_pair();
    let mut last = root.clone();
    loop {
        match tokens.next() {
            Some(token) => {
                let val = match token {
                    Token::Special(SpecialLexeme::LeftParam) => extract_list(tokens)?,
                    Token::Special(SpecialLexeme::RightParam) => break,
                    Token::Special(SpecialLexeme::Quote) => {
                        // 'foo inside list expands to (quote foo)
                        let next_token = tokens.next()
                            .ok_or_else(|| RuntimeError::new(crate::errors::ErrorTypes::ParseError, "quote requires an expression".to_string()))?;
                        let quoted_value = to_value(next_token, tokens)?;
                        Value::cons(
                            Value::make_symbol("quote"),
                            Value::cons(quoted_value, Value::make_null())?
                        )?
                    },
                    Token::Special(SpecialLexeme::FullStop) => {
                        let val = match tokens.next() {
                            None => return RuntimeError::parse_error("expression ended with a dot"),
                            Some(Token::Special(SpecialLexeme::LeftParam)) => return RuntimeError::parse_error("dot expressions must end with a value, not a list"),
                            Some(Token::Special(SpecialLexeme::RightParam)) => return RuntimeError::parse_error("expression ended with a dot"),
                            Some(Token::Special(SpecialLexeme::FullStop)) => return RuntimeError::parse_error("expression ended with a dot"),
                            Some(t) => to_value(t, tokens)?,
                        };
                        Value::set_cdr(last.clone(), val.clone())?;

                        match tokens.next() {
                            None => return RuntimeError::parse_error("unclosed expression"),
                            Some(Token::Special(SpecialLexeme::RightParam)) => break,
                            _ => return RuntimeError::parse_error("multiple values provided after a dot"),
                        }
                    },
                    _ => to_value(token, tokens)?,
                }.as_pair();
                Value::set_cdr(last.clone(), val.clone())?;
                last = val;
            }
            None => return RuntimeError::parse_error("expression ended without a closing parenthesis"),
        }
    }
    root.cdr()
}

fn make_number(number: &Number) -> Result<Rc<Value>, RuntimeError> {
    Ok(Rc::new(Value::Number(
        match number {
            Number::Float(n) => values::numbers::Number::Float(*n),
            Number::Int(n) => values::numbers::Number::Int(*n),
        }
    )))
}

fn to_value(token: &Token, tokens: &mut Iter<Token>) -> ValueResult {
    Ok(match token {
        Token::Special(SpecialLexeme::LeftParam) => extract_list(tokens)?,
        Token::Special(SpecialLexeme::RightParam) => return RuntimeError::parse_error("Dangling closing param found"),
        Token::Special(SpecialLexeme::FullStop) => return RuntimeError::parse_error("Dangling dot found - this can only be provided to mark the ending of a list"),
        Token::Special(SpecialLexeme::Quote) => {
            // 'foo expands to (quote foo)
            let next_token = tokens.next()
                .ok_or_else(|| RuntimeError::new(crate::errors::ErrorTypes::ParseError, "quote requires an expression".to_string()))?;
            let quoted_value = to_value(next_token, tokens)?;
            // Build (quote <value>)
            Value::cons(
                Value::make_symbol("quote"),
                Value::cons(quoted_value, Value::make_null())?
            )?
        },
        Token::Constant(Constant::True) => Value::boolean(true),
        Token::Constant(Constant::False) => Value::boolean(false),

        Token::Constant(Constant::Ignore) => Value::make_ignore(),
        Token::Constant(Constant::Inert) => Value::make_inert(),
        Token::Constant(Constant::Null) => Value::make_null(),

        Token::Number(n) => make_number(&n)?,
        Token::String(s) => Value::make_string(s),
        Token::Symbol(s) => Value::make_symbol(s),

        // TODO: handle chars
        _ => return RuntimeError::parse_error(format!("Unhandled token found")),
    })
}

pub fn parse(lexemes: Vec<Lexeme>) -> Result<Vec<Rc<Value>>, RuntimeError> {
    let tokens = lexemes.iter().map(parse_token).collect::<Result<Vec<Token>, RuntimeError>>()?;

    let mut values: Vec<Rc<Value>> = vec![];
    let mut tokens = tokens.iter();
    while let Some(token) = tokens.next() {
        values.push(to_value(token, &mut tokens)?);
    }
    Ok(values)
}

#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::lexemes::{Lexeme, SpecialLexeme, get_lexemes};
    use crate::tokens::{parse_token, to_value, extract_list, parse, Token, Number, Constant};

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
        assert_eq!(err.to_string(), format!("Parse error: Unknown constant found: {val}"));
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
        assert_eq!(err.to_string(), format!("Parse error: invalid digit found in string"));
    }

    #[parameterized(
        bad_number_1 = { "13.31.432" },
    )]
    fn test_get_bad_floats(val: &str) {
        let err = parse_token(&&Lexeme::Symbol(val.to_string())).unwrap_err();
        assert_eq!(err.to_string(), format!("Parse error: invalid float literal"));
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
            Token::Symbol(string.clone())  // Preserve original case
        );
    }

    #[parameterized(
        left = { Token::Special(SpecialLexeme::LeftParam), "(1)" },
        true_ = { Token::Constant(Constant::True), "#t" },
        false_ = { Token::Constant(Constant::False), "#f" },
        ignore = { Token::Constant(Constant::Ignore), "#ignore" },
        inert = { Token::Constant(Constant::Inert), "#inert" },
        null = { Token::Constant(Constant::Null), "()" },
        float = { Token::Number(Number::Float(0.123)), "0.123" },
        int = { Token::Number(Number::Int(32)), "32" },
        string = { Token::String("bla".to_string()), "\"bla\"" },
        symbol = { Token::Symbol("ble".to_string()), "ble" },
        // char = { Token::Char('a'), "#a" },
    )]
    fn test_get_to_value(token: Token, expected: &str) {
        let tokens = vec![Token::Number(Number::Int(1)), Token::Special(SpecialLexeme::RightParam)];
        let value = to_value(&token, &mut tokens.iter()).expect("this should work");
        assert_eq!(format!("{value}"), expected);
    }

    #[parameterized(
        right_param = { Token::Special(SpecialLexeme::RightParam), "Dangling closing param found" },
        full_stop = { Token::Special(SpecialLexeme::FullStop), "Dangling dot found - this can only be provided to mark the ending of a list" },
        chars = { Token::Char('a'), "Unhandled token found" },
    )]
    fn test_get_to_value_errors(token: Token, expected: &str) {
        let tokens = vec![Token::Number(Number::Int(1)), Token::Special(SpecialLexeme::RightParam)];
        let value = to_value(&token, &mut tokens.iter()).expect_err("this should fail");
        assert_eq!(value.to_string(), format!("Parse error: {expected}"));
    }


    #[parameterized(
        empty = { vec![
            Token::Special(SpecialLexeme::RightParam),
        ], "()" },
        single = { vec![
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::RightParam),
        ], "(bla)" },
        proper = { vec![
            Token::Symbol("bla".to_string()),
            Token::Symbol("bla".to_string()),
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::RightParam),
        ], "(bla bla bla)" },
        pair = { vec![
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::FullStop),
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::RightParam),
        ], "(bla . bla)" },
        list_dotted = { vec![
            Token::Symbol("bla1".to_string()),
            Token::Symbol("bla2".to_string()),
            Token::Special(SpecialLexeme::FullStop),
            Token::Symbol("bla3".to_string()),
            Token::Special(SpecialLexeme::RightParam),
        ], "(bla1 bla2 . bla3)" },
        nested = { vec![
            Token::Special(SpecialLexeme::LeftParam),
            Token::Symbol("1".to_string()),
            Token::Symbol("2".to_string()),
            Token::Symbol("3".to_string()),
            Token::Special(SpecialLexeme::RightParam),
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::LeftParam),
            Token::Symbol("4".to_string()),
            Token::Symbol("5".to_string()),
            Token::Symbol("6".to_string()),
            Token::Special(SpecialLexeme::RightParam),
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::LeftParam),
            Token::Symbol("7".to_string()),
            Token::Symbol("8".to_string()),
            Token::Symbol("9".to_string()),
            Token::Special(SpecialLexeme::RightParam),
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::LeftParam),
            Token::Symbol("10".to_string()),
            Token::Symbol("11".to_string()),
            Token::Symbol("12".to_string()),
            Token::Special(SpecialLexeme::RightParam),
            Token::Special(SpecialLexeme::RightParam),
        ], "((1 2 3) bla (4 5 6) bla (7 8 9) bla (10 11 12))" },

    )]
    fn test_extract_list(tokens: Vec<Token>, expected: &str) {
        let value = extract_list(&mut tokens.iter()).expect("this should work");
        assert_eq!(value.to_string(), expected);
    }

    #[parameterized(
        non_terminal_dot = { vec![
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::FullStop),
            Token::Symbol("bla".to_string()),
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::RightParam),
        ], "Parse error: multiple values provided after a dot" },
        last_dot = { vec![
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::FullStop),
            Token::Special(SpecialLexeme::RightParam),
        ], "Parse error: expression ended with a dot" },
        terminal_dot = { vec![
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::FullStop),
        ], "Parse error: expression ended with a dot" },
        nested_dotted = { vec![
            Token::Symbol("bla".to_string()),
            Token::Special(SpecialLexeme::FullStop),
            Token::Special(SpecialLexeme::LeftParam),
            Token::Symbol("10".to_string()),
            Token::Symbol("11".to_string()),
            Token::Symbol("12".to_string()),
            Token::Special(SpecialLexeme::RightParam),
            Token::Special(SpecialLexeme::RightParam),
        ], "Parse error: dot expressions must end with a value, not a list" },
    )]
    fn test_extract_list_fails(tokens: Vec<Token>, expected: &str) {
        let value = extract_list(&mut tokens.iter()).expect_err("this should fail");
        assert_eq!(value.to_string(), expected);
    }

    #[parameterized(
        true_ = { "#t" },
        false_ = { "#f" },

        empty = { "()"},
        ignore = { "#ignore" },
        inert = { "#inert" },

        zero = { "0" },
        int = { "123" },
        neg_int = { "-134" },

        float = { "1.234" },
        neg_float = { "-1.234" },

        pair = { "(1 . 2)" },
        list = { "(1 2 3 4)" },
        dotted_list = { "(1 2 3 . 4)" },
        nested_list = { "(1 (2 5 6) (3 7 8) 4)" },
        nested_dotted_list = { "(1 (2 5 6) (3 7 8) . 4)" },

        string = { "\"bla bla bla\"" },
        empty_string = { "\"\"" },
        symbol = { "bla" },
        if_ = { "($if #t 1 2)"},

        cons = { "(cons 1 (cons 2 (cons 3 (cons 4 ()))))"},
    )]
    fn test_tokenization_flow(expr: &str) {
        let mut chars = expr.chars();
        let lexes = get_lexemes(&mut chars).expect("ok");
        let value = parse(lexes).expect("ok")[0].to_string();
        assert_eq!(expr.to_string(), value);
    }
}
