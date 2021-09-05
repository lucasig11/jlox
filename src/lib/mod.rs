use error::{InterpreterError, LoxResult};
use interpreter::Resolver;
use std::{io::Write, path::PathBuf};

mod error;
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
        if src.trim().is_empty() {
            return;
        }

        std::env::set_var("LOX_SRC_FILE", src_filename.as_ref());

        let run = |src| {
            let tokens = Lexer::new(src).scan_tokens().map_err(|e| vec![e])?;
            let statements = Parser::new(&tokens).parse()?;
            let interpreter = Interpreter::new(&statements);
            let resolver = Resolver::new(&interpreter);
            resolver.resolve(&statements).map_err(|e| vec![e])?;
            interpreter.interpret()
        };

        if let Err(errors) = run(src) {
            for e in errors {
                let e = InterpreterError::from(e, src);
                eprintln!("{}\n", e);
            }
        }
    }
}
