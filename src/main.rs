#![feature(result_cloned)]
mod lib;
use lib::Lox;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Options {
    #[structopt(parse(from_os_str), help = "Script file to be interpreted (*.lox)")]
    file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Options::from_args();
    if let Err(e) = match opt.file {
        Some(path) => Lox::do_file(path),
        None => Lox::do_repl(),
    } {
        eprintln!("{}", e);
    }

    Ok(())
}
