use std::io;

#[derive(Debug, Copy, Clone, PartialEq)]
enum SpecialLexeme {
    LeftParam,
    RightParam,
    HashParam,
    DoubleQuote,
    Quote,
    BackQoute,
    Comma,
    CommaAt,
}

#[derive(Debug, PartialEq)]
enum Token {
    Special(SpecialLexeme),
    WhiteSpace,
    Char(char),
    None,
}

#[derive(Debug, PartialEq)]
enum Lexeme {
    Special(SpecialLexeme),
    Token(String),
}

fn parse_str(chars: &mut std::str::Chars) -> Result<Lexeme, io::Error> {
    let mut string = String::from("\"");
    let mut prev = ' ';
    for c in chars {
        string.push(c);
        if prev != '\\' && c == '"' {
            return Result::Ok(Lexeme::Token(string));
        }
        prev = c;
    }
    Result::Err(io::Error::new(io::ErrorKind::InvalidData, "Could not find end of string!"))
}

fn tokenize(chars: &mut std::str::Chars) -> Result<Vec<Lexeme>, io::Error> {
    let mut tokens: Vec<Lexeme> = Vec::new();

    let mut prev = Token::None;
    let mut current = String::new();
    while let Some(c) = chars.next() {
        let token: Token = match c {
            '(' => Token::Special(SpecialLexeme::LeftParam),
            ')' => Token::Special(SpecialLexeme::RightParam),
            ',' => Token::Special(SpecialLexeme::Comma),
            '"' => Token::Special(SpecialLexeme::DoubleQuote),
            '`' => Token::Special(SpecialLexeme::BackQoute),
            '\'' => Token::Special(SpecialLexeme::Quote),
            v => if v.is_whitespace() {
                Token::WhiteSpace
            } else {
                Token::Char(v)
            }
        };
        match &token {
            Token::Char(ch) => current.push(*ch),
            Token::None => (),
            Token::WhiteSpace => if let Token::Char(_) = prev {
                tokens.push(Lexeme::Token(current.clone()));
                current.clear();
            },
            Token::Special(l) => {
                if let Token::Char(_) = prev {
                    tokens.push(Lexeme::Token(current.clone()));
                    current.clear();
                }
                tokens.push({
                    match l {
                        SpecialLexeme::DoubleQuote => parse_str(chars)?,
                        _ => Lexeme::Special(*l)
                    }
                });
            },
        }
        prev = token;
    }
    if !current.is_empty() {
        tokens.push(Lexeme::Token(current.clone()));
    }
    Result::Ok(tokens)
}



fn main() {
    let raw = String::from("(+ (- 1 2) \"asd\\\"asd\" 3 4)");
    let mut chars = raw.chars();

    println!("{raw}");
    // println!("{:?}", tokens);
    println!("{:?}", tokenize(&mut chars));
    // println!("{:?}", parse(&mut chars));

}

#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::{Lexeme, SpecialLexeme, tokenize};

    #[parameterized(
        empty = { String::from(""), vec![] },
        empty_w_whitespace = { String::from("   \n \n   \t  "), vec![] },
        symbol = { String::from("bla"), vec![Lexeme::Token(String::from("bla"))]  },
        symbol_w_whitespace = { String::from("    bla\n   "), vec![Lexeme::Token(String::from("bla"))]  },

        single_string = { String::from("\"asd\""), vec![Lexeme::Token(String::from("\"asd\""))]  },
        string_w_whitespace = { String::from(" \"a  sdn \n\"  "), vec![Lexeme::Token(String::from("\"a  sdn \n\""))]  },
        escape_quote_string = { String::from("\"asd\\\"\""), vec![Lexeme::Token(String::from("\"asd\\\"\""))]  },

        multi_string = {
            String::from("  \"bla\" \"asd\""),
            vec![
                Lexeme::Token(String::from("\"bla\"")),
                Lexeme::Token(String::from("\"asd\"")),
            ]
        },
        multi_string_no_space = {
            String::from("  \"bla\"\"asd\""),
            vec![
                Lexeme::Token(String::from("\"bla\"")),
                Lexeme::Token(String::from("\"asd\"")),
            ]
        },

        empty_list = {
            String::from("  ()"),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },
        single_element_list = {
            String::from(" (bla) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(String::from("bla")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },
        multi_element_list = {
            String::from(" (bla ble wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(String::from("bla")),
                Lexeme::Token(String::from("ble")),
                Lexeme::Token(String::from("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        nestedmulti_element_list = {
            String::from(" (bla ble (+ 1 2) wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(String::from("bla")),
                Lexeme::Token(String::from("ble")),
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(String::from("+")),
                Lexeme::Token(String::from("1")),
                Lexeme::Token(String::from("2")),
                Lexeme::Special(SpecialLexeme::RightParam),
                Lexeme::Token(String::from("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        special_token_list = {
            String::from(" (, ' `) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Special(SpecialLexeme::Comma),
                Lexeme::Special(SpecialLexeme::Quote),
                Lexeme::Special(SpecialLexeme::BackQoute),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

    )]
    fn test_tokenize(input: String, expected: Vec<Lexeme>) {
        let mut chars = input.chars();
        assert_eq!(tokenize(&mut chars).ok().unwrap(), expected);
    }

    #[parameterized(
        non_terminated_string = { String::from("\"asdadsad") },
        non_terminated_string2 = { String::from("bla bla \"asdadsad") },
    )]
    fn test_tokenize_error(input: String) {
        let mut chars = input.chars();
        assert!(tokenize(&mut chars).is_err());
    }
}
