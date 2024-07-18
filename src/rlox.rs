use thiserror::Error;

use std::{fs, io};

use crate::{AstPrint, Interpreter, InterpreterError, Lexer, LexerErrors, Parser, ParserErrors};

#[derive(Debug, Clone)]
pub struct RLox {
    interpreter: Interpreter,
    verbose: bool,
}

impl RLox {
    pub fn new(verbose: bool) -> Self {
        Self {
            interpreter: Interpreter::new(),
            verbose,
        }
    }

    pub fn run_prompt(&mut self) -> Result<(), RLoxError> {
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

    pub fn run_file(&mut self, file_path: &std::path::Path) -> Result<(), RLoxError> {
        let file_content = fs::read_to_string(file_path)?;
        if let Err(error) = self.run(&file_content) {
            println!("{error}");
        }
        Ok(())
    }

    pub fn run(&mut self, source: &str) -> Result<(), RLoxError> {
        if self.verbose {
            println!("=== LEXER ===");
        }
        let tokens = Lexer::run(source)?;
        if self.verbose {
            for token in tokens.iter() {
                println!("{token:?}");
            }
        }

        if self.verbose {
            println!("=== PARSER ===");
        }
        let statements = Parser::run(tokens)?;
        if self.verbose {
            for statement in statements.iter() {
                print!("{}", statement.ast_print());
            }
            println!("");
        }

        if self.verbose {
            println!("=== INTERPRETER ===");
        }
        self.interpreter.evaluate_stmts(&statements)?;

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum RLoxError {
    #[error("I/O error ({0})")]
    IoError(#[from] io::Error),
    #[error("Lexer encountered errors:\n{0}")]
    LexerError(#[from] LexerErrors),
    #[error("Parser encountered errors:\n{0}")]
    ParserError(#[from] ParserErrors),
    #[error("Interpreter encountered error:\n{0}")]
    InterpreterError(#[from] InterpreterError),
}
