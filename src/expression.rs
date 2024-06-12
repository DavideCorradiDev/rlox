use crate::Token;

use std::fmt::Debug;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        value: Value,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Binary {
        operator: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
}

pub trait AstPrint {
    fn ast_print(&self) -> String;
}

impl AstPrint for Value {
    fn ast_print(&self) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => s.clone(),
        }
    }
}

impl AstPrint for Expr {
    fn ast_print(&self) -> String {
        match self {
            Self::Literal { value } => value.ast_print(),
            Self::Unary { operator, right } => {
                format!("({} {})", operator.lexeme, right.ast_print())
            }
            Self::Binary {
                operator,
                left,
                right,
            } => {
                format!(
                    "({} {} {})",
                    operator.lexeme,
                    left.ast_print(),
                    right.ast_print()
                )
            }
            Self::Grouping { expression } => {
                format!("(group {})", expression.ast_print())
            }
        }
    }
}
