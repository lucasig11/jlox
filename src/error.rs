use colored::Colorize;
use std::fmt;

use crate::lox::position::Span;
pub(crate) type LoxResult<T> = Result<T, LoxError>;

#[derive(Debug, Clone)]
pub enum ErrorLevel {
    Error,
    Warning,
}

impl fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ErrorLevel::Error => write!(f, "{}:", "error".red().bold()),
            ErrorLevel::Warning => write!(f, "{}:", "warning".yellow().bold()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum LoxError {
    Scan(ScanError),
    Io(std::io::Error),
    Parse(String),
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
}

impl std::error::Error for LoxError {}

impl std::fmt::Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxError::Scan(e) => write!(f, "{}", e),
            LoxError::Io(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::ParseInt(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::ParseFloat(e) => write!(f, "{} {}", ErrorLevel::Error, e),
            LoxError::Parse(e) => write!(f, "{}", e),
        }
    }
}

impl From<std::io::Error> for LoxError {
    fn from(err: std::io::Error) -> Self {
        Self::Io(err)
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

#[derive(Debug, Clone)]
pub(crate) struct ScanError {
    pos: Span,
    level: ErrorLevel,
    src_line: String,
    file: String,
    message: String,
}

impl ScanError {
    pub fn error<T: Into<String>>(message: T, file: T, src_line: String, pos: Span) -> Self {
        Self {
            pos,
            level: ErrorLevel::Error,
            src_line,
            file: file.into(),
            message: message.into(),
        }
    }
}

fn pad(n: usize, ch: char) -> String {
    std::iter::repeat(ch).take(n).collect()
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sep = "|".blue();
        let here = format!(
            "{}{}",
            pad(
                self.pos.start().column_number().saturating_sub(1) as usize,
                ' '
            ),
            pad(
                self.pos
                    .end()
                    .column_number()
                    .saturating_add(1)
                    .saturating_sub(self.pos.start().column_number()) as usize,
                '^'
            )
            .red()
        );
        let line = format!(
            "{space} {sep}\n{} {sep}\t{}\n{space} {sep}\t{here}",
            self.pos.start().line_number().to_string().blue(),
            self.src_line.trim(),
            sep = sep,
            here = here.blue(),
            space = pad(self.pos.start().line_number().to_string().len(), ' '),
        );

        write!(
            f,
            "{} {}\n  {} {}:{}\n{}",
            self.level,
            self.message,
            "-->".blue(),
            self.file,
            self.pos.start(),
            line
        )
    }
}
