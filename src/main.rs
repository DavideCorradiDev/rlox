use clap::Parser;
use rlox::Interpreter;

#[derive(Debug, Parser)]
struct Args {
    #[arg(value_hint = clap::ValueHint::DirPath)]
    file_path: Option<std::path::PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    match args.file_path {
        Some(file_path) => Interpreter::new().run_file(&file_path)?,
        None => Interpreter::new().run_prompt()?,
    }
    Ok(())
}
