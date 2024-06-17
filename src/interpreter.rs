use thiserror::Error;

use crate::{Expr, Token, TokenKind, Value};

pub enum ValuePair {
    Numbers(f64, f64),
    Strings(String, String),
}

fn expect_bool(token: &Token, right: Value) -> Result<bool, InterpreterError> {
    right.into_bool().ok_or(InterpreterError::new(
        token,
        InterpreterErrorKind::ExpectedBool,
    ))
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

#[derive(Debug)]
pub struct Interpreter {
    expr: Expr,
}

impl Interpreter {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }

    pub fn run(self) -> Result<Value, InterpreterError> {
        self.expr.evaluate()
    }
}

pub trait Evaluatable {
    fn evaluate(self) -> Result<Value, InterpreterError>;
}

impl Evaluatable for Expr {
    fn evaluate(self) -> Result<Value, InterpreterError> {
        match self {
            Self::Literal { value } => Ok(value),
            Self::Grouping { expr } => expr.evaluate(),
            Self::Unary { operator, right } => {
                let right = right.evaluate()?;
                match operator.kind {
                    TokenKind::Minus => {
                        let right = expect_number(&operator, right)?;
                        Ok(Value::from(-right))
                    }
                    TokenKind::Bang => Ok(Value::from(!right.is_truthy())),
                    _ => panic!("unimplemented unary operator"),
                }
            }
            Self::Binary {
                operator,
                left,
                right,
            } => {
                let left = left.evaluate()?;
                let right = right.evaluate()?;
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
            Self::Ternary {
                operator,
                expr,
                left,
                right,
            } => {
                let expr = expr.evaluate()?;
                match operator.kind {
                    TokenKind::QuestionMark => {
                        let expr = expect_bool(&operator, expr)?;
                        if expr {
                            Ok(left.evaluate()?)
                        } else {
                            Ok(right.evaluate()?)
                        }
                    }
                    _ => panic!("unimplemented ternary operator"),
                }
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
        write!(
            f,
            "[line {} at {}] Error: {}",
            self.line, self.location, self.kind
        )
    }
}

#[derive(Debug, Clone, Error)]
pub enum InterpreterErrorKind {
    #[error("operand must be a bool")]
    ExpectedBool,
    #[error("operand must be a number")]
    ExpectedNumber,
    #[error("operands must be numbers")]
    ExpectedNumbers,
    #[error("operands must be strings or numbers")]
    ExpectedStringsOrNumbers,
    #[error("operands must be numbers or at least one of them must be a string")]
    ExpectedNumbersOrOneString,
    #[error("division by zero")]
    DivisionByZero,
}
