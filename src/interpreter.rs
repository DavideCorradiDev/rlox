use crate::{ClockFn, Environment, Expr, Function, Stmt, Token, TokenKind, Value};

use thiserror::Error;

pub enum ValuePair {
    Numbers(f64, f64),
    Strings(String, String),
}

fn expect_number(token: &Token, right: Value) -> Result<f64, InterpreterError> {
    right.into_number().ok_or(InterpreterError::new(
        token,
        InterpreterErrorKind::ExpectedNumber,
    ))
}

fn expect_numbers(
    token: &Token,
    left: Value,
    right: Value,
) -> Result<(f64, f64), InterpreterError> {
    let left = left.into_number().ok_or(InterpreterError::new(
        token,
        InterpreterErrorKind::ExpectedNumbers,
    ))?;
    let right = right.into_number().ok_or(InterpreterError::new(
        token,
        InterpreterErrorKind::ExpectedNumbers,
    ))?;
    Ok((left, right))
}

fn expect_strings_or_numbers(
    token: &Token,
    left: Value,
    right: Value,
) -> Result<ValuePair, InterpreterError> {
    if let Value::Number(left) = left {
        if let Value::Number(right) = right {
            return Ok(ValuePair::Numbers(left, right));
        }
    }
    if let Value::String(left) = left {
        if let Value::String(right) = right {
            return Ok(ValuePair::Strings(left, right));
        }
    }
    Err(InterpreterError::new(
        token,
        InterpreterErrorKind::ExpectedStringsOrNumbers,
    ))
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut environment = Environment::new();
        environment.define(Value::from(ClockFn::new()));
        Self { environment }
    }

    pub fn with_environment(environment: Environment) -> Self {
        Self { environment }
    }

    pub fn run(&mut self, statements: &[Stmt]) -> Result<Option<Value>, InterpreterError> {
        self.evaluate_stmts(statements)
    }

    fn evaluate_stmts(&mut self, statements: &[Stmt]) -> Result<Option<Value>, InterpreterError> {
        for statement in statements {
            match self.evaluate_stmt(statement)? {
                Some(v) => {
                    return Ok(Some(v));
                }
                None => (),
            }
        }
        Ok(None)
    }

    fn evaluate_stmt(&mut self, statement: &Stmt) -> Result<Option<Value>, InterpreterError> {
        match statement {
            Stmt::Expression { expr } => {
                self.evaluate_expr(&expr)?;
                Ok(None)
            }
            Stmt::Print { expr } => {
                let value = self.evaluate_expr(&expr)?;
                println!("{}", value.to_string());
                Ok(None)
            }
            Stmt::Var { initializer, .. } => {
                let value = self.evaluate_expr(&initializer)?;
                self.environment.define(value);
                Ok(None)
            }
            Stmt::Block { statements } => {
                self.environment.push_scope();
                let result = self.evaluate_stmts(&statements);
                self.environment.pop_scope();
                result
            }
            Stmt::Function { name, params, body } => {
                self.environment.define(Value::from(Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.clone(),
                    closure: self.environment.get_scope(),
                }));
                Ok(None)
            }
            Stmt::Return { expr, .. } => {
                let value = self.evaluate_expr(expr)?;
                Ok(Some(value))
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if self.evaluate_expr(&condition)?.is_truthy() {
                    self.evaluate_stmt(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.evaluate_stmt(else_branch)
                } else {
                    Ok(None)
                }
            }
            Stmt::While { condition, body } => {
                while self.evaluate_expr(&condition)?.is_truthy() {
                    match self.evaluate_stmt(body)? {
                        Some(v) => return Ok(Some(v)),
                        None => (),
                    }
                }
                Ok(None)
            }
        }
    }

    fn evaluate_expr(&mut self, expression: &Expr) -> Result<Value, InterpreterError> {
        match expression {
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Variable {
                scope_distance,
                var_index,
                ..
            } => Ok(self
                .environment
                .get(*scope_distance, *var_index)
                .borrow()
                .clone()),
            Expr::Assign {
                value,
                scope_distance,
                var_index,
                ..
            } => {
                let value = self.evaluate_expr(value)?;
                let var = self.environment.get(*scope_distance, *var_index).clone();
                *var.borrow_mut() = value.clone();
                Ok(value)
            }
            Expr::Grouping { expr } => self.evaluate_expr(expr),
            Expr::Unary { operator, right } => {
                let right = self.evaluate_expr(right)?;
                match operator.kind {
                    TokenKind::Minus => {
                        let right = expect_number(&operator, right)?;
                        Ok(Value::from(-right))
                    }
                    TokenKind::Bang => Ok(Value::from(!right.is_truthy())),
                    _ => panic!("unimplemented unary operator"),
                }
            }
            Expr::Binary {
                operator,
                left,
                right,
            } => {
                let left = self.evaluate_expr(left)?;
                let right = self.evaluate_expr(right)?;
                match operator.kind {
                    TokenKind::Greater => {
                        match expect_strings_or_numbers(&operator, left, right)? {
                            ValuePair::Numbers(left, right) => Ok(Value::from(left > right)),
                            ValuePair::Strings(left, right) => Ok(Value::from(left > right)),
                        }
                    }
                    TokenKind::GreaterEqual => {
                        match expect_strings_or_numbers(&operator, left, right)? {
                            ValuePair::Numbers(left, right) => Ok(Value::from(left >= right)),
                            ValuePair::Strings(left, right) => Ok(Value::from(left >= right)),
                        }
                    }
                    TokenKind::Less => match expect_strings_or_numbers(&operator, left, right)? {
                        ValuePair::Numbers(left, right) => Ok(Value::from(left < right)),
                        ValuePair::Strings(left, right) => Ok(Value::from(left < right)),
                    },
                    TokenKind::LessEqual => {
                        match expect_strings_or_numbers(&operator, left, right)? {
                            ValuePair::Numbers(left, right) => Ok(Value::from(left <= right)),
                            ValuePair::Strings(left, right) => Ok(Value::from(left <= right)),
                        }
                    }
                    TokenKind::BangEqual => Ok(Value::from(left != right)),
                    TokenKind::EqualEqual => Ok(Value::from(left == right)),
                    TokenKind::Minus => {
                        let (left, right) = expect_numbers(&operator, left, right)?;
                        Ok(Value::from(left - right))
                    }
                    TokenKind::Slash => {
                        let (left, right) = expect_numbers(&operator, left, right)?;
                        if right == 0. {
                            Err(InterpreterError::new(
                                &operator,
                                InterpreterErrorKind::DivisionByZero,
                            ))
                        } else {
                            Ok(Value::from(left / right))
                        }
                    }
                    TokenKind::Star => {
                        let (left, right) = expect_numbers(&operator, left, right)?;
                        Ok(Value::from(left * right))
                    }
                    TokenKind::Plus => {
                        if let Value::String(left) = left {
                            return Ok(Value::from(format!("{left}{right}")));
                        }
                        if let Value::String(right) = right {
                            return Ok(Value::from(format!("{left}{right}")));
                        }
                        if let Value::Number(left) = left {
                            if let Value::Number(right) = right {
                                return Ok(Value::from(left + right));
                            }
                        }
                        Err(InterpreterError::new(
                            &operator,
                            InterpreterErrorKind::ExpectedNumbersOrOneString,
                        ))
                    }
                    TokenKind::Comma => Ok(right),
                    _ => panic!("unimplemented binary operator"),
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate_expr(callee)?;
                let arguments = arguments
                    .iter()
                    .map(|x| self.evaluate_expr(x))
                    .collect::<Result<Vec<_>, _>>()?;
                match callee {
                    Value::Callable(c) => {
                        if arguments.len() != c.arity() {
                            Err(InterpreterError::new(
                                &paren,
                                InterpreterErrorKind::MismatchedArity(c.arity(), arguments.len()),
                            ))
                        } else {
                            c.call(arguments)
                        }
                    }
                    _ => Err(InterpreterError::new(
                        &paren,
                        InterpreterErrorKind::InvalidCall,
                    )),
                }
            }
            Expr::Logical {
                operator,
                left,
                right,
            } => {
                let left = self.evaluate_expr(left)?;
                match operator.kind {
                    TokenKind::Or => {
                        if left.is_truthy() {
                            return Ok(left);
                        }
                    }
                    TokenKind::And => {
                        if !left.is_truthy() {
                            return Ok(left);
                        }
                    }
                    _ => panic!("unimplemented logical operator"),
                }
                Ok(self.evaluate_expr(right)?)
            }
        }
    }
}

#[derive(Debug, Clone, Error)]
pub struct InterpreterError {
    pub line: usize,
    pub location: String,
    pub kind: InterpreterErrorKind,
}

impl InterpreterError {
    pub fn new(token: &Token, kind: InterpreterErrorKind) -> Self {
        let line = token.line;
        let location = match token.kind {
            TokenKind::Eof => String::from("end"),
            _ => format!("'{}'", token.lexeme),
        };
        Self {
            line,
            location,
            kind,
        }
    }
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {} at {}] {}", self.line, self.location, self.kind)
    }
}

#[derive(Debug, Clone, Error)]
pub enum InterpreterErrorKind {
    #[error("Operand must be a bool")]
    ExpectedBool,
    #[error("Operand must be a number")]
    ExpectedNumber,
    #[error("Operands must be numbers")]
    ExpectedNumbers,
    #[error("Operands must be strings or numbers")]
    ExpectedStringsOrNumbers,
    #[error("Operands must be numbers or at least one of them must be a string")]
    ExpectedNumbersOrOneString,
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Undefined variable")]
    UndefinedVariable,
    #[error("Can only call functions and classes")]
    InvalidCall,
    #[error("Expected {0} arguments but got {1}")]
    MismatchedArity(usize, usize),
}
