use crate::{Callable, Instance, Token};

use std::{fmt::Debug, rc::Rc};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Callable(Rc<dyn Callable>),
    Instance(Instance),
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
            Value::Callable(c) => write!(f, "{c}"),
            Value::Instance(i) => write!(f, "{}", i),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Nil => match other {
                Value::Nil => true,
                _ => false,
            },
            Value::Bool(l) => match other {
                Value::Bool(r) => l == r,
                _ => false,
            },
            Value::Number(l) => match other {
                Value::Number(r) => l == r,
                _ => false,
            },
            Value::String(l) => match other {
                Value::String(r) => l == r,
                _ => false,
            },
            Value::Callable(_) => false,
            Value::Instance(_) => unimplemented!(),
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

impl<T> From<T> for Value
where
    T: Callable + 'static,
{
    fn from(value: T) -> Self {
        Self::Callable(Rc::new(value))
    }
}

impl<T> From<Rc<T>> for Value
where
    T: Callable + 'static,
{
    fn from(value: Rc<T>) -> Self {
        Self::Callable(value)
    }
}

impl From<Instance> for Value {
    fn from(value: Instance) -> Self {
        Self::Instance(value)
    }
}

dyn_clone::clone_trait_object!(Callable);

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        value: Value,
    },
    Variable {
        name: Token,
        scope_distance: usize,
        var_index: usize,
    },
    This {
        keyword: Token,
        scope_distance: usize,
        var_index: usize,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
        scope_distance: usize,
        var_index: usize,
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
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
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
        Self::Variable {
            name,
            scope_distance: 0,
            var_index: 0,
        }
    }

    pub fn assign(name: Token, value: Expr) -> Self {
        Self::Assign {
            name,
            value: Box::new(value),
            scope_distance: 0,
            var_index: 0,
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

    pub fn call(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Self {
        Self::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        }
    }

    pub fn get(object: Expr, name: Token) -> Self {
        Self::Get {
            object: Box::new(object),
            name,
        }
    }

    pub fn set(object: Expr, name: Token, value: Expr) -> Self {
        Self::Set {
            object: Box::new(object),
            name,
            value: Box::new(value),
        }
    }

    pub fn this(keyword: Token) -> Self {
        Self::This {
            keyword,
            scope_distance: 0,
            var_index: 0,
        }
    }

    pub fn grouping(expr: Expr) -> Self {
        Self::Grouping {
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionStmt {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
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
    Function {
        function: FunctionStmt,
    },
    Return {
        keyword: Token,
        expr: Expr,
    },
    Class {
        name: Token,
        methods: Vec<Stmt>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
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

    pub fn function(function: FunctionStmt) -> Self {
        Self::Function { function }
    }

    pub fn return_stmt(keyword: Token, expr: Expr) -> Self {
        Self::Return { keyword, expr }
    }

    pub fn if_stmt(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Self::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(|x| Box::new(x)),
        }
    }

    pub fn while_stmt(condition: Expr, body: Stmt) -> Self {
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
        format!("{self}")
    }
}

impl<T> AstPrint for Vec<T>
where
    T: AstPrint,
{
    fn ast_print(&self) -> String {
        self.iter()
            .map(|x| x.ast_print())
            .collect::<Vec<String>>()
            .join(", ")
    }
}

impl AstPrint for Expr {
    fn ast_print(&self) -> String {
        match self {
            Self::Literal { value } => value.ast_print(),
            Self::Variable {
                name,
                scope_distance,
                var_index,
            } => format!("{}[{scope_distance},{var_index}]", name.lexeme),
            Self::This {
                keyword,
                scope_distance,
                var_index,
            } => {
                format!("({}[{scope_distance},{var_index}])", keyword.lexeme)
            }
            Self::Assign {
                name,
                value,
                scope_distance,
                var_index,
            } => {
                format!(
                    "assign {}[{scope_distance}{var_index}] {}",
                    name.lexeme,
                    value.ast_print()
                )
            }
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
            Self::Call {
                callee, arguments, ..
            } => {
                format!("(call {}({}))", callee.ast_print(), arguments.ast_print())
            }
            Self::Get { object, name } => {
                format!("(get {}.{})", object.ast_print(), name.lexeme)
            }
            Self::Set {
                object,
                name,
                value,
            } => {
                format!(
                    "(set {}.{} = {})",
                    object.ast_print(),
                    name.lexeme,
                    value.ast_print()
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

impl AstPrint for FunctionStmt {
    fn ast_print(&self) -> String {
        let params_str = self
            .params
            .iter()
            .map(|x| x.lexeme.clone())
            .collect::<Vec<String>>()
            .join(", ");
        let body_str = self
            .body
            .iter()
            .map(|x| format!("{{{}}}", x.ast_print()))
            .collect::<Vec<String>>()
            .join("");
        format!("{{fun {}({params_str}){body_str}}}", self.name.lexeme)
    }
}

impl AstPrint for Stmt {
    fn ast_print(&self) -> String {
        match self {
            Self::Expression { expr } => format!("{{expr {}}}", expr.ast_print()),
            Self::Print { expr } => format!("{{print {}}}", expr.ast_print()),
            Self::Var { name, initializer } => {
                format!("{{var {} = {}}}", name.lexeme, initializer.ast_print())
            }
            Self::Block { statements } => {
                let block_str = statements
                    .iter()
                    .map(|x| format!("{{{}}}", x.ast_print()))
                    .collect::<Vec<String>>()
                    .join("");
                format!("{{block {block_str}}}")
            }
            Self::Function { function } => function.ast_print(),
            Self::Return { keyword, expr } => {
                format!("{{{} {}}}", keyword.lexeme, expr.ast_print())
            }
            Self::Class { name, methods } => {
                let method_str = methods
                    .iter()
                    .map(|x| format!("{{{}}}", x.ast_print()))
                    .collect::<Vec<String>>()
                    .join("");
                format!("{{class {}{method_str}", name.lexeme)
            }
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                format!(
                    "{{if {} {}{}}}",
                    condition.ast_print(),
                    then_branch.ast_print(),
                    match else_branch {
                        Some(stmt) => format!(" else {}", stmt.ast_print()),
                        None => String::new(),
                    }
                )
            }
            Self::While { condition, body } => {
                format!("{{while {} {}}}", condition.ast_print(), body.ast_print(),)
            }
        }
    }
}
