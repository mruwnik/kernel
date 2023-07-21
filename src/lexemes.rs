use std::fmt;

use crate::errors::{ RuntimeError, ErrorTypes };

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SpecialLexeme {
    LeftParam,
    RightParam,
    DoubleQuote,
    Semicolon,
    FullStop,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum InvalidLexeme {
    HashParam,
    Quote,
    BackQuote,
    Comma,
    CommaAt,
}

impl fmt::Display for InvalidLexeme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            InvalidLexeme::HashParam => "#(",
            InvalidLexeme::Quote => "'",
            InvalidLexeme::BackQuote => "`",
            InvalidLexeme::Comma => ",",
            InvalidLexeme::CommaAt => ",@"
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ReservedLexeme {
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    Vertical,
}

impl fmt::Display for ReservedLexeme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            ReservedLexeme::LeftCurly => "{",
            ReservedLexeme::LeftSquare => "[",
            ReservedLexeme::RightCurly => "}",
            ReservedLexeme::RightSquare => "]",
            ReservedLexeme::Vertical => "|"
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum Char {
    Special(SpecialLexeme),
    Invalid(InvalidLexeme),
    Reserved(ReservedLexeme),
    WhiteSpace,
    Char(char),
    None,
}

#[derive(Debug, PartialEq)]
pub enum Lexeme {
    Special(SpecialLexeme),
    Symbol(String),
    String(String),
}

fn make_error<T>(err: String) -> Result<T, RuntimeError> {
    Result::Err(RuntimeError::new(ErrorTypes::ParseError, err))
}

/// Process a string literal, reading chars until it get to the end of the string
fn parse_str(chars: &mut std::str::Chars) -> Result<Lexeme, RuntimeError> {
    let mut string = String::new();
    let mut prev = ' ';
    for c in chars {
        if prev != '\\' && c == '"' {
            return Result::Ok(Lexeme::String(string));
        }
        string.push(c);
        prev = c;
    }
    make_error(format!("Could not find end of string!"))
}

/// Fast forward the char stream to the end of the current line
fn skip_to_line_end(chars: &mut std::str::Chars) -> () {
    for c in chars {
        if c == '\n' {
            break;
        }
    }
}

/// Parse the given chars iterator and return a vector of resulting lexemes
///
/// # Arguments
///
/// * `chars` - an iterator of chars. This will be used up in the process of parsing,
///             so after this function has executed, it will have either gone through
///             it all, or in the case of errors will point at the character after the
///             error.
///
/// # Returns
///
/// A vector of the parsed lexemes, if the string was successfully parsed
pub fn get_lexemes(chars: &mut std::str::Chars) -> Result<Vec<Lexeme>, RuntimeError> {
    let mut lexemes: Vec<Lexeme> = Vec::new();

    let mut prev = Char::None;
    let mut current_lexeme = String::new();
    while let Some(c) = chars.next() {
        let char: Char = match c {
            '(' if prev == Char::Char('#') => Char::Invalid(InvalidLexeme::HashParam),
            '@' if prev == Char::Char(',') => Char::Invalid(InvalidLexeme::CommaAt),
            '(' => Char::Special(SpecialLexeme::LeftParam),
            ')' => Char::Special(SpecialLexeme::RightParam),
            ';' => Char::Special(SpecialLexeme::Semicolon),
            '"' => Char::Special(SpecialLexeme::DoubleQuote),

            ',' => Char::Invalid(InvalidLexeme::Comma),
            '`' => Char::Invalid(InvalidLexeme::BackQuote),
            '\'' => Char::Invalid(InvalidLexeme::Quote),

            '[' => Char::Reserved(ReservedLexeme::LeftSquare),
            ']' => Char::Reserved(ReservedLexeme::RightSquare),
            '{' => Char::Reserved(ReservedLexeme::LeftCurly),
            '}' => Char::Reserved(ReservedLexeme::RightCurly),
            '|' => Char::Reserved(ReservedLexeme::Vertical),
            v if v.is_whitespace() => Char::WhiteSpace,
            v => Char::Char(v),
        };
        // If this char is a special one or is whitespace then it marks the end of the previous
        // token, assuming that the previous char was a basic character
        if let Char::WhiteSpace | Char::Special(_) = &char {
            if let Char::Char(_) = prev {
                lexemes.push(Lexeme::Symbol(current_lexeme.clone().to_lowercase()));
                current_lexeme.clear();
            }
        }
        match &char {
            Char::Char(ch) => current_lexeme.push(*ch),
            Char::None => (),
            Char::WhiteSpace => (),
            Char::Invalid(l) => return make_error(format!("Invalid token found: '{l}'")),
            Char::Reserved(l) => return make_error(format!("Reserved token found: '{l}'")),
            Char::Special(SpecialLexeme::Semicolon) => skip_to_line_end(chars),
            Char::Special(SpecialLexeme::DoubleQuote) => lexemes.push(parse_str(chars)?),
            Char::Special(l) => lexemes.push(Lexeme::Special(*l)),
        }
        prev = char;
    }
    if !current_lexeme.is_empty() {
        lexemes.push(Lexeme::Symbol(current_lexeme.clone().to_lowercase()));
    }
    Result::Ok(lexemes)
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use crate::lexemes::{Lexeme, SpecialLexeme, get_lexemes};

    #[parameterized(
        empty = { format!(""), vec![] },
        empty_w_whitespace = { format!("   \n \n   \t  "), vec![] },
        symbol = { format!("bla"), vec![Lexeme::Symbol(format!("bla"))]  },
        symbol_w_whitespace = { format!("    bla\n   "), vec![Lexeme::Symbol(format!("bla"))]  },

        single_string = { format!("\"asd\""), vec![Lexeme::String(format!("asd"))]  },
        string_w_whitespace = { format!(" \"a  sdn \n\"  "), vec![Lexeme::String(format!("a  sdn \n"))]  },
        escape_quote_string = { format!("\"asd\\\"\""), vec![Lexeme::String(format!("asd\\\""))]  },

        multi_string = {
            format!("  \"bla\" \"asd\""),
            vec![
                Lexeme::String(format!("bla")),
                Lexeme::String(format!("asd")),
            ]
        },
        multi_string_no_space = {
            format!("  \"bla\"\"asd\""),
            vec![
                Lexeme::String(format!("bla")),
                Lexeme::String(format!("asd")),
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
                Lexeme::Symbol(format!("bla")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },
        multi_element_list = {
            format!(" (bla ble wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Symbol(format!("bla")),
                Lexeme::Symbol(format!("ble")),
                Lexeme::Symbol(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        nestedmulti_element_list = {
            format!(" (bla ble (+ 1 2) wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Symbol(format!("bla")),
                Lexeme::Symbol(format!("ble")),
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Symbol(format!("+")),
                Lexeme::Symbol(format!("1")),
                Lexeme::Symbol(format!("2")),
                Lexeme::Special(SpecialLexeme::RightParam),
                Lexeme::Symbol(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        multiline_expr = {
            format!(" (bla\n   ble\n   wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Symbol(format!("bla")),
                Lexeme::Symbol(format!("ble")),
                Lexeme::Symbol(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        comments = {
            format!(" (bla  ;; a nice comment to be ignored\n   ble; another comment\n   wrar) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Symbol(format!("bla")),
                Lexeme::Symbol(format!("ble")),
                Lexeme::Symbol(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },

        symbols_case = {
            format!(" (BLA  ble WrAr) "),
            vec![
                Lexeme::Special(SpecialLexeme::LeftParam),
                Lexeme::Symbol(format!("bla")),
                Lexeme::Symbol(format!("ble")),
                Lexeme::Symbol(format!("wrar")),
                Lexeme::Special(SpecialLexeme::RightParam),
            ]
        },
    )]
    fn test_get_lexemes(input: String, expected: Vec<Lexeme>) {
        let mut chars = input.chars();
        assert_eq!(get_lexemes(&mut chars).ok().unwrap(), expected);
    }

    #[parameterized(
        string = { format!("\"asdadsad") },
        string2 = { format!("bla bla \"asdadsad") },
    )]
    fn test_get_lexemes_non_terminated_string(input: String) {
        let mut chars = input.chars();
        let err = get_lexemes(&mut chars).unwrap_err();
        assert_eq!(err.to_string(), "Parse error: Could not find end of string!")
    }

    #[parameterized(
        quote = { format!("'") },
        comma = { format!(",") },
        backquote = { format!("`") },
        hash_param = { format!("#(") },
    )]
    fn test_get_lexemes_invalid_lexeme(c: String) {
        let input = format!("bla bla {c}wrar ble ble");
        let mut chars = input.chars();
        let err = get_lexemes(&mut chars).unwrap_err();
        assert_eq!(err.to_string(), format!("Parse error: Invalid token found: '{c}'"))
    }

    #[parameterized(
        left_square = { '[' },
        right_square = { ']' },
        left_curly = { '{' },
        right_curly = { '}' },
        vertical = { '|' },
    )]
    fn test_get_lexemes_reserved_lexeme(c: char) {
        let input = format!("bla bla {c}wrar ble ble");
        let mut chars = input.chars();
        let err = get_lexemes(&mut chars).unwrap_err();
        assert_eq!(err.to_string(), format!("Parse error: Reserved token found: '{c}'"))
    }
}
