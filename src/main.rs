use std::path::PathBuf;

mod lox {
    use std::{io::Write, path::PathBuf};

    pub(crate) use self::error::LoxResult;

    mod error {
        pub(crate) type LoxResult<T> = Result<T, Box<dyn std::error::Error>>;

        use colored::Colorize;

        #[derive(Debug, Clone)]
        pub(crate) struct LoxError {
            line: u32,
            col: u32,
            r#where: String,
            message: String,
        }

        impl std::fmt::Display for LoxError {
            fn fmt(
                &self,
                f: &mut std::fmt::Formatter<'_>,
            ) -> std::result::Result<(), std::fmt::Error> {
                write!(
                    f,
                    "{} {} at line {}:{}",
                    "Error:".red().bold(),
                    self.message,
                    self.line,
                    self.col
                )
            }
        }

        impl std::error::Error for LoxError {
            fn description(&self) -> &str {
                &self.message
            }
        }
    }

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
}

use structopt::StructOpt;

#[derive(StructOpt)]
struct Options {
    #[structopt(parse(from_os_str), help = "Script file to be interpreted (*.lox)")]
    file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Options::from_args();
    match opt.file {
        Some(path) => lox::Lox::do_file(path)?,
        None => lox::Lox::do_repl()?,
    };
    Ok(())
}
