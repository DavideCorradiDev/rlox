use crate::{Expr, FunctionStmt, Stmt, Token, TokenKind, Value};

use thiserror::Error;

use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionKind {
    None,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, Clone, PartialEq)]
struct VariableInfo {
    state: VariableState,
    token: Token,
    var_index: usize,
}

impl Display for VariableInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "state: {:?}, idx: {}", self.state, self.var_index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariableState {
    Declared,
    Defined,
    Used,
}

#[derive(Debug, Clone)]
struct Scopes(Vec<HashMap<String, VariableInfo>>);

impl Display for Scopes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Env Start ===")?;
        for (i, scope) in self.0.iter().enumerate() {
            writeln!(f, "--- Scope {i} ---")?;
            for (name, info) in scope.iter() {
                writeln!(f, "  {name} - {info}")?;
            }
        }
        write!(f, "=== Env End ===")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Resolver {
    scopes: Scopes,
    current_function: FunctionKind,
}

impl Resolver {
    pub fn new() -> Self {
        let mut global_scope = HashMap::new();
        global_scope.insert(
            String::from("clock"),
            VariableInfo {
                state: VariableState::Used,
                token: Token {
                    line: 0,
                    lexeme: String::from("clock"),
                    kind: TokenKind::Fun,
                },
                var_index: 0,
            },
        );
        Self {
            scopes: Scopes(vec![global_scope]),
            current_function: FunctionKind::None,
        }
    }

    pub fn run(&mut self, statements: &mut [Stmt]) -> Result<(), ResolverError> {
        self.resolve_stmts(statements)?;
        while !self.scopes.0.is_empty() {
            self.pop_scope()?;
        }
        Ok(())
    }

    fn resolve_stmts(&mut self, statements: &mut [Stmt]) -> Result<(), ResolverError> {
        for statement in statements {
            self.resolve_stmt(statement)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, statement: &mut Stmt) -> Result<(), ResolverError> {
        match statement {
            Stmt::Block { statements } => {
                self.push_scope();
                self.resolve_stmts(statements)?;
                self.pop_scope()?;
            }
            Stmt::Var { name, initializer } => {
                self.declare(name)?;
                self.resolve_expr(initializer)?;
                self.define(name);
            }
            Stmt::Function { function } => {
                self.resolve_function(function, FunctionKind::Function)?;
            }
            Stmt::Expression { expr } => {
                self.resolve_expr(expr)?;
            }
            Stmt::Class { name, methods } => {
                self.declare(name)?;
                self.define(name);
                for method in methods {
                    if let Stmt::Function { function } = method {
                        let function_kind = if function.name.lexeme == String::from("init") {
                            FunctionKind::Initializer
                        } else {
                            FunctionKind::Method
                        };
                        self.resolve_function(function, function_kind)?;
                    } else {
                        panic!("Only methods are allowed inside a class")
                    }
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }
            }
            Stmt::Print { expr } => {
                self.resolve_expr(expr)?;
            }
            Stmt::Return { keyword, expr } => {
                match self.current_function {
                    FunctionKind::None => {
                        return Err(ResolverError::new(
                            keyword,
                            ResolverErrorKind::TopLevelReturn,
                        ));
                    }
                    FunctionKind::Initializer => {
                        if let Expr::Literal { value } = expr {
                            if *value == Value::Nil {
                                return self.resolve_expr(expr);
                            }
                        }
                        return Err(ResolverError::new(
                            keyword,
                            ResolverErrorKind::InitializerReturn,
                        ));
                    }
                    FunctionKind::Method | FunctionKind::Function => (),
                }
                self.resolve_expr(expr)?;
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expression: &mut Expr) -> Result<(), ResolverError> {
        match expression {
            Expr::Variable {
                name,
                scope_distance,
                var_index,
            } => {
                if let Some(scope) = self.scopes.0.last() {
                    if let Some(info) = scope.get(&name.lexeme) {
                        if info.state == VariableState::Declared {
                            return Err(ResolverError::new(
                                &name,
                                ResolverErrorKind::VariableReadInInitializer,
                            ));
                        }
                    }
                }
                (*scope_distance, *var_index) = self.resolve_variable(name)?;
            }
            Expr::Assign {
                name,
                value,
                scope_distance,
                var_index,
            } => {
                self.resolve_expr(value)?;
                (*scope_distance, *var_index) = self.resolve_variable(name)?;
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expr(callee)?;
                self.push_scope();
                for argument in arguments {
                    self.resolve_expr(argument)?;
                }
                self.pop_scope()?;
            }
            Expr::Get { object, .. } => {
                self.resolve_expr(object)?;
            }
            Expr::Set { object, value, .. } => {
                self.resolve_expr(object)?;
                self.resolve_expr(value)?;
            }
            Expr::This {
                keyword,
                scope_distance,
                var_index,
            } => {
                (*scope_distance, *var_index) = self.resolve_variable(keyword)?;
            }
            Expr::Grouping { expr } => {
                self.resolve_expr(expr)?;
            }
            Expr::Literal { .. } => (),
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Unary { right, .. } => {
                self.resolve_expr(right)?;
            }
        }
        Ok(())
    }

    fn push_scope(&mut self) {
        self.scopes.0.push(HashMap::new());
    }

    fn pop_scope(&mut self) -> Result<(), ResolverError> {
        let scope = self.scopes.0.pop().expect("No scope to pop");
        Self::check_for_unused_variables(&scope)
    }

    fn check_for_unused_variables(
        scope: &HashMap<String, VariableInfo>,
    ) -> Result<(), ResolverError> {
        for (_, info) in scope.iter() {
            if info.state != VariableState::Used {
                return Err(ResolverError::new(
                    &info.token,
                    ResolverErrorKind::UnusedVariable,
                ));
            }
        }
        Ok(())
    }

    fn declare(&mut self, name: &Token) -> Result<(), ResolverError> {
        let scope = self.scopes.0.last_mut().expect("No scopes left");
        match scope.get(&name.lexeme) {
            Some(_) => {
                return Err(ResolverError::new(
                    name,
                    ResolverErrorKind::ShadowedVariableInScope,
                ))
            }
            None => {
                let var_index = scope.len();
                scope.insert(
                    name.lexeme.clone(),
                    VariableInfo {
                        state: VariableState::Declared,
                        token: name.clone(),
                        var_index,
                    },
                );
                return Ok(());
            }
        }
    }

    fn define(&mut self, name: &Token) {
        let scope = self.scopes.0.last_mut().expect("No scopes left");
        scope
            .get_mut(&name.lexeme)
            .expect("variable was not declared")
            .state = VariableState::Defined;
    }

    fn declare_this(&mut self, mut name: Token) {
        name.lexeme = String::from("this");
        self.declare_used(name);
    }

    fn declare_used(&mut self, name: Token) {
        let scope = self.scopes.0.last_mut().expect("No scopes left");
        let var_index = scope.len();
        scope.insert(
            name.lexeme.clone(),
            VariableInfo {
                state: VariableState::Used,
                token: name,
                var_index,
            },
        );
    }

    fn resolve_variable(&mut self, name: &Token) -> Result<(usize, usize), ResolverError> {
        for (scope_idx, scope) in self.scopes.0.iter_mut().rev().enumerate() {
            if let Some(info) = scope.get_mut(&name.lexeme) {
                match info.state {
                    VariableState::Declared => {
                        return Err(ResolverError::new(
                            name,
                            ResolverErrorKind::UndefinedVariable,
                        ));
                    }
                    VariableState::Defined => {
                        info.state = VariableState::Used;
                        return Ok((scope_idx, info.var_index));
                    }
                    VariableState::Used => {
                        return Ok((scope_idx, info.var_index));
                    }
                }
            }
        }
        Err(ResolverError::new(
            name,
            ResolverErrorKind::UndefinedVariable,
        ))
    }

    fn resolve_function(
        &mut self,
        function: &mut FunctionStmt,
        function_kind: FunctionKind,
    ) -> Result<(), ResolverError> {
        match function_kind {
            FunctionKind::Function => {
                self.declare_used(function.name.clone());
            }
            FunctionKind::Method | FunctionKind::Initializer => {
                self.push_scope();
                self.declare_this(function.name.clone());
            }
            FunctionKind::None => (),
        }

        let enclosing_function = self.current_function;
        self.current_function = function_kind;

        self.push_scope();
        for param in function.params.iter() {
            self.declare(&param)?;
            self.define(&param);
        }
        self.resolve_stmts(&mut function.body)?;
        self.pop_scope()?;

        self.current_function = enclosing_function;

        match function_kind {
            FunctionKind::Method | FunctionKind::Initializer => self.pop_scope()?,
            FunctionKind::Function | FunctionKind::None => (),
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Error)]
pub struct ResolverError {
    pub line: usize,
    pub location: String,
    pub kind: ResolverErrorKind,
}

impl ResolverError {
    pub fn new(token: &Token, kind: ResolverErrorKind) -> Self {
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

impl std::fmt::Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {} at {}] {}", self.line, self.location, self.kind)
    }
}

#[derive(Debug, Clone, Error)]
pub enum ResolverErrorKind {
    #[error("Can't read local variable in its own initializer")]
    VariableReadInInitializer,
    #[error("Undefined variable")]
    UndefinedVariable,
    #[error("Defined but not used")]
    UnusedVariable,
    #[error("This scope already contains a variable with this name")]
    ShadowedVariableInScope,
    #[error("Can't return from top-level code")]
    TopLevelReturn,
    #[error("Can't return from initializer")]
    InitializerReturn,
}
