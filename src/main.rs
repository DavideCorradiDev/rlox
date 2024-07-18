use clap::Parser;
use rlox::RLox;

#[derive(Debug, Parser)]
struct Args {
    #[arg(value_hint = clap::ValueHint::DirPath)]
    file_path: Option<std::path::PathBuf>,
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let mut rlox = RLox::new(args.verbose);
    match args.file_path {
        Some(file_path) => rlox.run_file(&file_path)?,
        None => rlox.run_prompt()?,
    }
    Ok(())
}
