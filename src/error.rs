use colored::Colorize;
use std::fmt;

use crate::lib::position::Span;
pub(crate) type LoxResult<T> = Result<T, LoxError>;

/// Error that wraps the inner errors
/// in order to be able to print the line
/// at which the error was generated
pub(crate) struct InterpreterError<'a> {
    err: LoxError,
    src_file: Vec<&'a str>,
}

impl<'a> InterpreterError<'a> {
    pub fn from(err: LoxError, src_file: &'a str) -> Self {
        Self {
            err,
            src_file: src_file.lines().collect(),
        }
    }
}

impl<'a> fmt::Display for InterpreterError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LoxError::*;
        match &self.err {
            Inner(e) => {
                let line = fmt_line_error(
                    e.pos,
                    self.src_file
                        .get(e.pos.start().line_number() as usize - 1)
                        // Unwrapping here is safe because we know this line exists
                        .unwrap(),
                );
                write!(f, "{} {}\n{}", ErrorLevel::Error, e, line)
            }
            e => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorLevel {
    Error,
}

impl fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ErrorLevel::Error => write!(f, "{}:", "error".red().bold()),
        }
    }
}

#[derive(Debug)]
pub(crate) struct InnerError {
    pos: Span,
    message: String,
}

impl InnerError {
    pub fn new(pos: Span, msg: &str) -> Self {
        Self {
            pos,
            message: msg.to_string(),
        }
    }
}

impl std::fmt::Display for InnerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

/// Error wrapper for irrecoverable errors
#[derive(Debug)]
pub(crate) enum LoxError {
    /// Inner interpreter errors (lexing, parsing and evaluating stages)
    Inner(InnerError),
    /// Errors thrown by any I/O function
    Io(std::io::Error),
    /// Errors that don't have any special way of handling and are just intended
    /// to be passed back to it's caller as an error message.
    Generic(String),
    /// Errors thrown when parsing a string to an integer
    ParseInt(std::num::ParseIntError),
    /// Errors thrown when parsing a string to a float
    ParseFloat(std::num::ParseFloatError),
}

impl std::error::Error for LoxError {}

impl std::fmt::Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxError::Inner(e) => write!(f, "{}", e),
            LoxError::Generic(e) => write!(f, "{}", e),
            LoxError::Io(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::ParseInt(e) => write!(f, "{}", e),
            LoxError::ParseFloat(e) => write!(f, "{}", e),
        }
    }
}

impl From<InnerError> for LoxError {
    fn from(err: InnerError) -> Self {
        Self::Inner(err)
    }
}

impl From<std::io::Error> for LoxError {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<std::num::ParseIntError> for LoxError {
    fn from(err: std::num::ParseIntError) -> Self {
        Self::ParseInt(err)
    }
}

impl From<std::num::ParseFloatError> for LoxError {
    fn from(err: std::num::ParseFloatError) -> Self {
        Self::ParseFloat(err)
    }
}

/// Repeats a char `n` times.
fn pad(n: usize, ch: char) -> String {
    std::iter::repeat(ch).take(n).collect()
}

/// Helper for pretty-printing errors in a line
/// ```md
///   |
/// 1 |   ok$(err);
///   |     ^
/// ```
fn fmt_line_error(span: Span, text: &str) -> String {
    let sep = "|".blue();
    let (start, end) = (span.start(), span.end());
    let here = format!(
        "{}{}",
        pad(start.column_number().saturating_sub(1) as usize, ' '),
        pad(
            end.column_number()
                .saturating_add(1)
                .saturating_sub(start.column_number()) as usize,
            '^'
        )
        .red()
    );
    format!(
        " {} {}:{}\n{space} {sep}\n{} {sep}\t{}\n{space} {sep}\t{here}",
        "-->".blue(),
        // Unwrapping is safe here bc we always set this variable when the program runs
        std::env::var("LOX_SRC_FILE").unwrap(),
        start,
        start.line_number().to_string().blue(),
        text.trim(),
        sep = sep,
        here = here.blue(),
        space = pad(start.line_number().to_string().len(), ' '),
    )
}
