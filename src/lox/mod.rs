use crate::error::LoxResult;
use std::{io::Write, path::PathBuf};

pub struct Lexer {
    src: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token;

impl Lexer {
    pub fn new(src: String) -> Self {
        Self { src }
    }
    pub fn scan(&self) -> Vec<Token> {
        return Vec::new();
    }
}
pub struct Lox;

impl Lox {
    pub fn do_file(path: PathBuf) -> LoxResult<()> {
        let src = std::fs::read_to_string(path)?;

        Self::run(src)
    }

    pub(crate) fn do_repl() -> LoxResult<()> {
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf)?;
            if let Err(e) = Self::run(buf) {
                println!("{}", e);
            }
        }
    }

    fn run(src: String) -> LoxResult<()> {
        let lexer = Lexer::new(src);
        let tokens = lexer.scan();

        for token in tokens {
            println!("{:?}", token);
        }
        Ok(())
    }
}
