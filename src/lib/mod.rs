use crate::error::{InterpreterError, LoxResult};
use std::{io::Write, path::PathBuf};

mod interpreter;
mod lexer;
pub(crate) mod parser;
pub(crate) mod position;

pub(crate) use lexer::token;
use lexer::Lexer;

use interpreter::Interpreter;
use parser::Parser;

pub struct Lox;

impl Lox {
    pub(crate) fn do_file(path: PathBuf) -> LoxResult<()> {
        let src = std::fs::read_to_string(&path)?;
        Self::run(
            &src,
            path.to_str().ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "filename contains illegal UTF-8 characters",
                )
            })?,
        );
        Ok(())
    }

    pub(crate) fn do_repl() -> LoxResult<()> {
        loop {
            print!("> ");
            let mut buf = String::new();
            std::io::stdout().flush()?;
            std::io::stdin().read_line(&mut buf)?;
            Self::run(&buf, "stdin");
        }
    }

    fn run<T: AsRef<str>>(src: &str, src_filename: T) {
        if src.trim().len().eq(&0) {
            return;
        }
        std::env::set_var("LOX_SRC_FILE", src_filename.as_ref());
        let run = |src| {
            let tokens = Lexer::new(src).scan_tokens().map_err(|e| vec![e])?;
            let expr = Parser::new(&tokens).parse()?;
            Interpreter::interpret(expr)
        };

        if let Err(errors) = run(src) {
            for e in errors {
                let e = InterpreterError::from(e, src);
                println!("{}", e);
            }
        }
    }
}
