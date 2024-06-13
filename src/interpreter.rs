use thiserror::Error;

use std::{fs, io};

use crate::{AstPrint, Lexer, LexerErrors, Parser, ParserError};

#[derive(Debug, Clone)]
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_prompt(&self) -> Result<(), InterpreterError> {
        use std::io::Write;

        let mut line = String::new();
        loop {
            // Show prompt cursor.
            print!("> ");
            io::stdout().flush()?;

            // Read a line, stop if EOF was reached.
            line.clear();
            if io::stdin().read_line(&mut line)? == 0 {
                break;
            }
            let line = line.trim();
            if line == "quit" {
                break;
            }

            // Run the line, eventually print error but don't quit the prompt.
            if let Err(error) = self.run(&line) {
                println!("{error}");
            }
        }

        Ok(())
    }

    pub fn run_file(&self, file_path: &std::path::Path) -> Result<(), InterpreterError> {
        let file_content = fs::read_to_string(file_path)?;
        self.run(&file_content)
    }

    pub fn run(&self, source: &str) -> Result<(), InterpreterError> {
        let tokens = Lexer::new(source).scan_tokens()?;

        println!("=== LEXING ===");
        println!("Lexer output:");
        for token in tokens.iter() {
            println!("{token:?}");
        }

        let ast = Parser::new(tokens).parse()?;

        println!("=== PARSING ===");
        println!("Parser output:");
        println!("{}", ast.ast_print());

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("I/O error ({0})")]
    IoError(#[from] io::Error),
    #[error("Lexer error:\n{0}")]
    LexerError(#[from] LexerErrors),
    #[error("Parser error:\n{0}")]
    ParserError(#[from] ParserError),
}
