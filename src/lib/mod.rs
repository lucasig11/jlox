use crate::error::LoxResult;
use std::{io::Write, path::PathBuf};

mod lexer;
pub(crate) mod parser;
pub(crate) mod position;

pub(crate) use lexer::token;
use lexer::Lexer;

use self::parser::Parser;

pub struct Lox;

impl Lox {
    pub(crate) fn do_file(path: PathBuf) -> LoxResult<()> {
        let src = std::fs::read_to_string(&path)?;
        Self::run(
            src,
            path.to_str().ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Source-file name contains illegal UTF-8 characters",
                )
            })?,
        )
    }

    pub(crate) fn do_repl() -> LoxResult<()> {
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf)?;
            if let Err(e) = Self::run(buf, "STDIN") {
                println!("{}", e);
            }
        }
    }

    fn run<T: AsRef<str>>(src: String, src_filename: T) -> LoxResult<()> {
        std::env::set_var("LOX_SRC_FILE", src_filename.as_ref());
        let lexer = Lexer::new(&src);
        let tokens = lexer.scan_tokens()?;

        let parser = Parser::new(tokens);
        let expr = parser.parse()?;
        println!("{}", expr);

        Ok(())
    }
}
