use clap::Parser;
use rlox::RLox;

#[derive(Debug, Parser)]
struct Args {
    #[arg(value_hint = clap::ValueHint::DirPath)]
    file_path: Option<std::path::PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    match args.file_path {
        Some(file_path) => RLox::new().run_file(&file_path)?,
        None => RLox::new().run_prompt()?,
    }
    Ok(())
}
