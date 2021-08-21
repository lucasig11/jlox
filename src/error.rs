use crate::Token;
use colored::Colorize;
use std::fmt;

use crate::lib::position::Span;
pub(crate) type LoxResult<T> = Result<T, LoxError>;

#[derive(Debug, Clone)]
pub enum ErrorLevel {
    Error,
}

impl fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ErrorLevel::Error => write!(f, "{}:", "error".red().bold()),
            // ErrorLevel::Warning => write!(f, "{}:", "warning".yellow().bold()),
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
            LoxError::Io(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::ParseInt(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::ParseFloat(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::Generic(e) => write!(f, "{} {}", ErrorLevel::Error, e),
        }
    }
}

impl From<std::io::Error> for LoxError {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
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
pub(crate) struct ParseError {
    tk: Token,
    message: String,
}

impl ParseError {
    pub fn new(tk: Token, msg: &str) -> Self {
        Self {
            tk,
            message: msg.to_string(),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line = fmt_line_error(*self.tk.span(), &self.tk.kind().to_string());
        write!(f, "{}\n{}", self.message, line)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ScanError {
    pos: Span,
    src_line: String,
    message: String,
}

impl ScanError {
    pub fn error<T: Into<String>>(message: T, src_line: String, pos: Span) -> Self {
        Self {
            pos,
            src_line,
            message: message.into(),
        }
    }
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = fmt_line_error(self.pos, &self.src_line);
        write!(f, "{}\n{}", self.message, line)
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
