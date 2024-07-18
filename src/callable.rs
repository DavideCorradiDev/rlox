use crate::{Interpreter, InterpreterError, Stmt, Token, Value};

use dyn_clone::DynClone;

use std::{
    fmt::{Debug, Display},
    time::{SystemTime, UNIX_EPOCH},
};

pub trait Callable: Debug + Display + DynClone {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError>;
}

#[derive(Debug, Clone)]
pub struct ClockFn {}

impl ClockFn {
    pub fn new() -> Self {
        Self {}
    }
}

impl Display for ClockFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

impl Callable for ClockFn {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut crate::Interpreter,
        _arguments: Vec<crate::Value>,
    ) -> Result<crate::Value, crate::InterpreterError> {
        Ok(Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("couldn't retrieve current time")
                .as_secs_f64(),
        ))
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Stmt,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}> ", self.name.lexeme)
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        for i in 0..self.params.len() {
            interpreter
                .environment
                .define(self.params[i].lexeme.clone(), arguments[i].clone());
        }
        interpreter
            .evaluate_stmt(&self.body)
            .map(|x| x.unwrap_or(Value::Nil))
    }
}
