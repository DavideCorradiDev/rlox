use crate::{Environment, Interpreter, InterpreterError, Scope, Stmt, Token, Value};

use dyn_clone::DynClone;

use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

pub trait Callable: Debug + Display + DynClone {
    fn arity(&self) -> usize;
    fn call(&self, arguments: Vec<Value>) -> Result<Value, InterpreterError>;
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

    fn call(&self, _arguments: Vec<crate::Value>) -> Result<crate::Value, crate::InterpreterError> {
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
    pub body: Vec<Stmt>,
    pub closure: Rc<RefCell<Scope>>,
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

    fn call(&self, arguments: Vec<Value>) -> Result<Value, InterpreterError> {
        let mut interpreter =
            Interpreter::with_environment(Environment::with_enclosing(self.closure.clone()));
        for i in 0..self.params.len() {
            interpreter.environment.define(arguments[i].clone());
        }
        interpreter.run(&self.body).map(|x| x.unwrap_or(Value::Nil))
    }
}
