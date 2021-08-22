use crate::Token;
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
        let mut display = |pos: Span, e: String| {
            let line = fmt_line_error(
                pos,
                &self
                    .src_file
                    .get(pos.start().line_number() as usize - 1)
                    .unwrap(),
            );
            write!(f, "{} {}\n{}", ErrorLevel::Error, e, line)
        };
        match &self.err {
            Scan(e) => display(e.pos, e.to_string()),
            Parse(e) => display(e.pos, e.to_string()),
            Runtime(e) => display(e.pos, e.to_string()),
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

/// Error wrapper for irrecoverable errors
#[derive(Debug)]
pub(crate) enum LoxError {
    /// Errors thrown during the lexical analysis
    Scan(ScanError),
    /// Errors thrown during the parsing
    Parse(ParseError),
    /// Errors thrown during the evaluation of expressions
    Runtime(RuntimeError),
    /// Errors thrown by any I/O function
    Io(std::io::Error),
    /// Errors that still don't have any special handling
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
            LoxError::Scan(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::Parse(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::Runtime(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::Io(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::ParseInt(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::ParseFloat(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::Generic(e) => write!(f, "{}", e),
        }
    }
}

impl From<std::io::Error> for LoxError {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<RuntimeError> for LoxError {
    fn from(err: RuntimeError) -> Self {
        Self::Runtime(err)
    }
}
impl From<ParseError> for LoxError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl From<ScanError> for LoxError {
    fn from(err: ScanError) -> Self {
        Self::Scan(err)
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

#[derive(Debug)]
pub(crate) struct RuntimeError {
    pos: Span,
    message: String,
}

impl RuntimeError {
    pub fn new(pos: Span, msg: &str) -> Self {
        Self {
            pos,
            message: msg.to_string(),
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug)]
pub(crate) struct ParseError {
    pos: Span,
    message: String,
}

impl ParseError {
    pub fn new(tk: Token, msg: &str) -> Self {
        Self {
            pos: *tk.span(),
            message: msg.to_string(),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ScanError {
    pos: Span,
    message: String,
}

impl ScanError {
    pub fn new<T: Into<String>>(message: T, src_line: String, pos: Span) -> Self {
        Self {
            pos,
            message: message.into(),
        }
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
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
        std::env::var("LOX_SRC_FILE").unwrap(),
        start,
        start.line_number().to_string().blue(),
        text.trim(),
        sep = sep,
        here = here.blue(),
        space = pad(start.line_number().to_string().len(), ' '),
    )
}
