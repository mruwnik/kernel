use std::io;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
enum SpecialLexeme {
    LeftParam,
    RightParam,
    HashParam,
    DoubleQuote,
    Quote,
    BackQuote,
    Comma,
    Semicolon,
}

impl fmt::Display for SpecialLexeme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            SpecialLexeme::LeftParam => "(",
            SpecialLexeme::RightParam => ")",
            SpecialLexeme::HashParam => "#(",
            SpecialLexeme::DoubleQuote => "\"",
            SpecialLexeme::Quote => "'",
            SpecialLexeme::BackQuote => "`",
            SpecialLexeme::Comma => ",",
            SpecialLexeme::Semicolon => ";",
        })
    }
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

fn make_error<T>(err: String) -> Result<T, io::Error> {
    Result::Err(io::Error::new(io::ErrorKind::InvalidData, err))
}

fn parse_str(chars: &mut std::str::Chars) -> Result<Lexeme, io::Error> {
    let mut string = format!("\"");
    let mut prev = ' ';
    for c in chars {
        string.push(c);
        if prev != '\\' && c == '"' {
            return Result::Ok(Lexeme::Token(string));
        }
        prev = c;
    }
    make_error(format!("Could not find end of string!"))
}

fn skip_to_line_end(chars: &mut std::str::Chars) -> () {
    for c in chars {
        if c == '\n' {
            break;
        }
    }
}

fn tokenize(chars: &mut std::str::Chars) -> Result<Vec<Lexeme>, io::Error> {
    let mut tokens: Vec<Lexeme> = Vec::new();

    let mut prev = Token::None;
    let mut current = String::new();
    while let Some(c) = chars.next() {
        let token: Token = match c {
            '(' if prev == Token::Char('#') => Token::Special(SpecialLexeme::HashParam),
            '(' => Token::Special(SpecialLexeme::LeftParam),
            ')' => Token::Special(SpecialLexeme::RightParam),
            ',' => Token::Special(SpecialLexeme::Comma),
            '"' => Token::Special(SpecialLexeme::DoubleQuote),
            '`' => Token::Special(SpecialLexeme::BackQuote),
            '\'' => Token::Special(SpecialLexeme::Quote),
            ';' => Token::Special(SpecialLexeme::Semicolon),
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
            Token::Special(
                l @ (SpecialLexeme::Quote |
                     SpecialLexeme::BackQuote |
                     SpecialLexeme::Comma |
                     SpecialLexeme::HashParam)
            ) => return make_error(format!("Invalid token found: '{l}'")),
            Token::Special(l) => {
                if let Token::Char(_) = prev {
                    tokens.push(Lexeme::Token(current.clone()));
                    current.clear();
                }
                if SpecialLexeme::Semicolon == *l {
                    skip_to_line_end(chars);
                } else {
                    tokens.push({
                        match l {
                            SpecialLexeme::DoubleQuote => parse_str(chars)?,
                            _ => Lexeme::Special(*l)
                        }
                    });
                }
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
    let raw = format!("(+ (- 1 2) \"asd\\\"asd\" 3 4)");
    let mut chars = raw.chars();

    println!("{raw}");
    // println!("{:?}", tokens);
    println!("{:?}", tokenize(&mut chars));
    // println!("{:?}", parse(&mut chars));

}

#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::{Lexeme, SpecialLexeme, tokenize, make_error};

    #[parameterized(
        empty = { format!(""), vec![] },
        empty_w_whitespace = { format!("   \n \n   \t  "), vec![] },
        symbol = { format!("bla"), vec![Lexeme::Token(format!("bla"))]  },
        symbol_w_whitespace = { format!("    bla\n   "), vec![Lexeme::Token(format!("bla"))]  },

        single_string = { format!("\"asd\""), vec![Lexeme::Token(format!("\"asd\""))]  },
        string_w_whitespace = { format!(" \"a  sdn \n\"  "), vec![Lexeme::Token(format!("\"a  sdn \n\""))]  },
        escape_quote_string = { format!("\"asd\\\"\""), vec![Lexeme::Token(format!("\"asd\\\"\""))]  },

        multi_string = {
            format!("  \"bla\" \"asd\""),
            vec![
                Lexeme::Token(format!("\"bla\"")),
                Lexeme::Token(format!("\"asd\"")),
            ]
        },
        multi_string_no_space = {
            format!("  \"bla\"\"asd\""),
            vec![
                Lexeme::Token(format!("\"bla\"")),
                Lexeme::Token(format!("\"asd\"")),
            ]
        },

        empty_list = {
            format!("  ()"),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },
        single_element_list = {
            format!(" (bla) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(format!("bla")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },
        multi_element_list = {
            format!(" (bla ble wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(format!("bla")),
                Lexeme::Token(format!("ble")),
                Lexeme::Token(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        nestedmulti_element_list = {
            format!(" (bla ble (+ 1 2) wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(format!("bla")),
                Lexeme::Token(format!("ble")),
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(format!("+")),
                Lexeme::Token(format!("1")),
                Lexeme::Token(format!("2")),
                Lexeme::Special(SpecialLexeme::RightParam),
                Lexeme::Token(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        multiline_expr = {
            format!(" (bla\n   ble\n   wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(format!("bla")),
                Lexeme::Token(format!("ble")),
                Lexeme::Token(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        comments = {
            format!(" (bla  ;; a nice comment to be ignored\n   ble; another comment\n   wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Token(format!("bla")),
                Lexeme::Token(format!("ble")),
                Lexeme::Token(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

    )]
    fn test_tokenize(input: String, expected: Vec<Lexeme>) {
        let mut chars = input.chars();
        assert_eq!(tokenize(&mut chars).ok().unwrap(), expected);
    }

    #[parameterized(
        non_terminated_string = { format!("\"asdadsad"), format!("Could not find end of string!") },
        non_terminated_string2 = { format!("bla bla \"asdadsad"), format!("Could not find end of string!") },
        invalid_char_quote = { format!("bla bla 'wrar ble ble"), format!("Invalid token found: '''") },
        invalid_char_comma = { format!("bla bla , ble ble"), format!("Invalid token found: ','") },
        invalid_char_backquote = { format!("bla bla `wrar ble ble"), format!("Invalid token found: '`'") },
        invalid_char_hash_param = { format!("bla bla #(wrar) ble ble"), format!("Invalid token found: '#('") },
    )]
    fn test_tokenize_error(input: String, error: String) {
        let mut chars = input.chars();
        let err = tokenize(&mut chars).unwrap_err();
        assert_eq!(err.to_string(), error)
    }
}
