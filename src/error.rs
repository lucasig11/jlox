use colored::Colorize;
use std::fmt;

pub(crate) type LoxResult<T> = Result<T, Box<dyn std::error::Error>>;

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

#[derive(Debug, Clone)]
pub(crate) struct LoxError {
    line: u32,
    col: u32,
    level: ErrorLevel,
    src_line: String,
    file: String,
    message: String,
}

impl LoxError {
    pub fn error<T: AsRef<str>>(
        message: T,
        file: T,
        src_line: String,
        position: (u32, u32),
    ) -> Box<Self> {
        Box::new(Self {
            line: position.0,
            col: position.1,
            level: ErrorLevel::Error,
            src_line,
            file: file.as_ref().to_string(),
            message: message.as_ref().to_string(),
        })
    }
}

fn spaces(n: usize) -> String {
    let mut s = String::new();
    for _ in 0..n {
        s.push(' ');
    }
    s
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sep = "|".blue();
        let here = format!("{}^-- here", spaces(self.col as usize - 1));
        let line = format!(
            "{space} {sep}\n{} {sep}\t{}\n{space} {sep}\t{here}",
            self.line.to_string().blue(),
            self.src_line,
            sep = sep,
            here = here.blue(),
            space = spaces(self.line.to_string().len()),
        );

        write!(
            f,
            "{} {}\n  {} {}:{}:{}\n{}",
            self.level,
            self.message,
            "-->".blue(),
            self.file,
            self.line,
            self.col,
            line
        )
    }
}

impl std::error::Error for LoxError {}
