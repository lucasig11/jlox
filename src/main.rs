use std::path::PathBuf;
use structopt::StructOpt;

pub(crate) mod error;
mod lib;

use lib::{
    parser::expression::Expr::*,
    position::Position,
    token::{Numeric, Punctuator, Token, TokenKind},
};

#[derive(StructOpt)]
struct Options {
    #[structopt(parse(from_os_str), help = "Script file to be interpreted (*.lox)")]
    file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let expr = Binary(
        Unary(
            Token::new(Punctuator::Sub, Position::new(1, 1).into()),
            Literal(Token::new(
                Numeric::Integer(123),
                Position::new(1, 1).into(),
            ))
            .into(),
        )
        .into(),
        Token::new(Punctuator::Mul, Position::new(1, 1).into()),
        Grouping(
            Literal(Token::new(
                Numeric::Decimal(45.67),
                Position::new(1, 1).into(),
            ))
            .into(),
        )
        .into(),
    );
    println!("{}", expr);

    let opt = Options::from_args();
    match opt.file {
        Some(path) => {
            if let Err(e) = lib::Lox::do_file(path) {
                println!("{}", e);
            }
        }
        None => lib::Lox::do_repl()?,
    };

    Ok(())
}
