use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorTypes {
    TypeError,
    ImmutableError,
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    error_type: ErrorTypes,
    cause: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", match self.error_type {
            ErrorTypes::TypeError => "Type error",
            ErrorTypes::ImmutableError => "Mutate error",
        }, self.cause)
    }
}

impl RuntimeError {
    pub fn new(error_type: ErrorTypes, cause: impl Into<String>) -> Self {
        Self { error_type, cause: cause.into() }
    }
}
