use error::{InterpreterError, LoxError, LoxResult};
use interpreter::Resolver;
use std::{io::Write, path::PathBuf, rc::Rc};

mod error;
mod interpreter;
mod lexer;
pub(crate) mod parser;
pub(crate) mod position;

pub(crate) use lexer::token;
use lexer::Lexer;

use interpreter::Interpreter;
use parser::Parser;

use self::{interpreter::Environment, parser::Stmt};

pub struct Lox;

impl Lox {
    pub(crate) fn do_file(path: PathBuf) -> LoxResult<()> {
        let src = std::fs::read_to_string(&path)?;

        if src.trim().is_empty() {
            return Ok(());
        }

        std::env::set_var("LOX_SRC_FILE", &*path.to_string_lossy());

        let run = |src| {
            let statements = Self::parse(src)?;
            Self::execute(statements)
        };

        if let Err(errors) = run(src.to_string()) {
            for e in errors {
                let e = InterpreterError::from(e, &src);
                eprintln!("{}\n", e);
            }
        }
        Ok(())
    }

    pub(crate) fn do_repl() -> LoxResult<()> {
        println!("\u{001b}c");
        let mut buf = String::with_capacity(4096);
        std::env::set_var("LOX_SRC_FILE", "REPL");
        let env = Rc::new(Environment::new());
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            std::io::stdin().read_line(&mut buf)?;

            let run = |src| {
                let statements = Self::parse(src)?;
                if statements.len().eq(&1) {
                    if let Some(Stmt::Expression(expr)) = statements.first() {
                        println!(
                            "{}",
                            expr.evaluate(Rc::clone(&env), &Default::default())
                                .map_err(|e| vec![e])?
                                .to_string()
                        );
                        return Ok(());
                    }
                }
                if statements.is_empty() {
                    return Ok(());
                }

                let interpreter = Interpreter::with_env(&statements, Rc::clone(&env));
                let resolver = Resolver::new(&interpreter);
                resolver.resolve(&statements).map_err(|e| vec![e])?;
                interpreter.interpret()
            };

            if let Err(errors) = run(buf.to_string()) {
                for e in errors {
                    let e = InterpreterError::from(e, &buf);
                    eprintln!("{}\n", e);
                }
            }
            buf.clear();
        }
    }

    fn parse(src: String) -> Result<Vec<Stmt>, Vec<LoxError>> {
        let tokens = Lexer::new(&src).scan_tokens().map_err(|e| vec![e])?;
        Parser::new(&tokens).parse()
    }

    fn execute(statements: Vec<Stmt>) -> Result<(), Vec<LoxError>> {
        if statements.is_empty() {
            return Ok(());
        }

        let interpreter = Interpreter::new(&statements);
        let resolver = Resolver::new(&interpreter);
        resolver.resolve(&statements).map_err(|e| vec![e])?;
        interpreter.interpret()
    }
}
