use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorTypes {
    ImmutableError,
    LookupError,
    ParseError,
    TypeError,
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    error_type: ErrorTypes,
    cause: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", match self.error_type {
            ErrorTypes::ImmutableError => "Mutate error",
            ErrorTypes::LookupError => "Lookup error",
            ErrorTypes::ParseError => "Parse error",
            ErrorTypes::TypeError => "Type error",
        }, self.cause)
    }
}

impl<T> From<T> for RuntimeError
where
    T: std::error::Error + std::fmt::Debug,
{
    fn from(err: T) -> Self {
        RuntimeError {
            error_type: ErrorTypes::ParseError,
            cause: err.to_string(),
        }
    }
}

impl RuntimeError {
    pub fn new(error_type: ErrorTypes, cause: impl Into<String>) -> Self {
        Self { error_type, cause: cause.into() }
    }
}
