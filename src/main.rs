use std::path::PathBuf;
use structopt::StructOpt;

pub(crate) mod error;
mod lox;

#[derive(StructOpt)]
struct Options {
    #[structopt(parse(from_os_str), help = "Script file to be interpreted (*.lox)")]
    file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Options::from_args();
    match opt.file {
        Some(path) => {
            if let Err(e) = lox::Lox::do_file(path) {
                println!("{}", e);
            }
        }
        None => lox::Lox::do_repl()?,
    };
    Ok(())
}
