use crate::Token;

use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

impl Value {
    pub fn into_bool(self) -> Option<bool> {
        match self {
            Self::Bool(v) => Some(v),
            _ => None,
        }
    }

    pub fn into_number(self) -> Option<f64> {
        match self {
            Self::Number(v) => Some(v),
            _ => None,
        }
    }

    pub fn into_string(self) -> Option<String> {
        match self {
            Self::String(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Bool(v) => *v,
            _ => true,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b:?}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        value: Value,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
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
    Logical {
        operator: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

impl Expr {
    pub fn literal<T>(value: T) -> Self
    where
        T: Into<Value>,
    {
        Self::Literal {
            value: value.into(),
        }
    }

    pub fn variable(name: Token) -> Self {
        Self::Variable { name }
    }

    pub fn assign(name: Token, value: Expr) -> Self {
        Self::Assign {
            name,
            value: Box::new(value),
        }
    }

    pub fn unary(operator: Token, right: Expr) -> Self {
        Self::Unary {
            operator,
            right: Box::new(right),
        }
    }

    pub fn binary(operator: Token, left: Expr, right: Expr) -> Self {
        Self::Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn logical(operator: Token, left: Expr, right: Expr) -> Self {
        Self::Logical {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn grouping(expr: Expr) -> Self {
        Self::Grouping {
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression {
        expr: Expr,
    },
    Print {
        expr: Expr,
    },
    Var {
        name: Token,
        initializer: Expr,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    }
}

impl Stmt {
    pub fn expression(expr: Expr) -> Self {
        Self::Expression { expr }
    }

    pub fn print(expr: Expr) -> Self {
        Self::Print { expr }
    }

    pub fn var(name: Token, initializer: Expr) -> Self {
        Self::Var { name, initializer }
    }

    pub fn block(statements: Vec<Stmt>) -> Self {
        Self::Block { statements }
    }

    pub fn if_statement(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Self::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(|x| Box::new(x)),
        }
    }

    pub fn while_statement(condition: Expr, body: Stmt) -> Self {
        Self::While {
            condition,
            body: Box::new(body),
        }
    }
}

pub trait AstPrint {
    fn ast_print(&self) -> String;
}

impl AstPrint for Value {
    fn ast_print(&self) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => format!("\"{s}\""),
        }
    }
}

impl AstPrint for Expr {
    fn ast_print(&self) -> String {
        match self {
            Self::Literal { value } => value.ast_print(),
            Self::Variable { name } => format!("{}", name.lexeme),
            Self::Assign { name, value } => format!("assign {} {}", name.lexeme, value.ast_print()),
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
            Self::Logical {
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
            Self::Grouping { expr } => {
                format!("(group {})", expr.ast_print())
            }
        }
    }
}

impl AstPrint for Stmt {
    fn ast_print(&self) -> String {
        match self {
            Self::Expression { expr } => format!("{};", expr.ast_print()),
            Self::Print { expr } => format!("print {};", expr.ast_print()),
            Self::Var { name, initializer } => {
                format!("var {} = {};", name.lexeme, initializer.ast_print())
            }
            Self::Block { statements } => {
                let block_str = statements
                    .iter()
                    .map(|x| format!("{}", x.ast_print()))
                    .collect::<Vec<String>>()
                    .join("\n");
                format!("{{\n{block_str}\n}}")
            }
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                format!(
                    "if {} {}{}",
                    condition.ast_print(),
                    then_branch.ast_print(),
                    match else_branch {
                        Some(stmt) => format!(" else {}", stmt.ast_print()),
                        None => String::new(),
                    }
                )
            }
            Self::While {
                condition,
                body,
            } => {
                format!(
                    "while {} {}",
                    condition.ast_print(),
                    body.ast_print(),
                )
            }
        }
    }
}
