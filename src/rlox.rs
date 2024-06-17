use thiserror::Error;

use std::{fs, io};

use crate::{AstPrint, Interpreter, InterpreterError, Lexer, LexerErrors, Parser, ParserError};

#[derive(Debug, Clone)]
pub struct RLox {}

impl RLox {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run_prompt(&self) -> Result<(), RLoxError> {
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

    pub fn run_file(&self, file_path: &std::path::Path) -> Result<(), RLoxError> {
        let file_content = fs::read_to_string(file_path)?;
        self.run(&file_content)
    }

    pub fn run(&self, source: &str) -> Result<(), RLoxError> {
        let tokens = Lexer::new(source).run()?;

        println!("=== LEXING ===");
        println!("Lexer output:");
        for token in tokens.iter() {
            println!("{token:?}");
        }

        let ast = Parser::new(tokens).run()?;

        println!("=== PARSING ===");
        println!("Parser output:");
        println!("{}", ast.ast_print());

        let res = Interpreter::new(ast).run()?;
        println!("=== INTERPRETING ===");
        println!("{}", res.ast_print());

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum RLoxError {
    #[error("I/O error ({0})")]
    IoError(#[from] io::Error),
    #[error("Lexer error:\n{0}")]
    LexerError(#[from] LexerErrors),
    #[error("Parser error:\n{0}")]
    ParserError(#[from] ParserError),
    #[error("Interpreter error:\n{0}")]
    InterpreterError(#[from] InterpreterError),
}
