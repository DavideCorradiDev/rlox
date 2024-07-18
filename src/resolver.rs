use crate::{Expr, Stmt, Token, TokenKind};

use thiserror::Error;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionKind {
    None,
    Function,
}

#[derive(Debug, Clone, PartialEq)]
struct VariableInfo {
    state: VariableState,
    token: Token,
    var_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariableState {
    Declared,
    Defined,
    Used,
}

#[derive(Debug, Clone)]
pub struct Resolver {
    scopes: Vec<HashMap<String, VariableInfo>>,
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
            scopes: vec![global_scope],
            current_function: FunctionKind::None,
        }
    }

    pub fn run(&mut self, statements: &mut [Stmt]) -> Result<(), ResolverError> {
        self.resolve_stmts(statements)?;
        while !self.scopes.is_empty() {
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
            Stmt::Function { name, params, body } => {
                self.resolve_function(name, params, body, FunctionKind::Function)?;
            }
            Stmt::Expression { expr } => {
                self.resolve_expr(expr)?;
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
                if self.current_function == FunctionKind::None {
                    return Err(ResolverError::new(
                        keyword,
                        ResolverErrorKind::TopLevelReturn,
                    ));
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
                if let Some(scope) = self.scopes.last() {
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
                for argument in arguments {
                    self.resolve_expr(argument)?;
                }
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
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) -> Result<(), ResolverError> {
        let scope = self.scopes.pop().expect("No scope to pop");
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
        if let Some(scope) = self.scopes.last_mut() {
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
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope
                .get_mut(&name.lexeme)
                .expect("variable was not declared")
                .state = VariableState::Defined;
        }
    }

    fn resolve_variable(&mut self, name: &Token) -> Result<(usize, usize), ResolverError> {
        for (scope_idx, scope) in self.scopes.iter_mut().rev().enumerate() {
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
        name: &Token,
        params: &[Token],
        body: &mut [Stmt],
        function_kind: FunctionKind,
    ) -> Result<(), ResolverError> {
        let enclosing_function = self.current_function;
        self.current_function = function_kind;

        self.declare(name)?;
        self.define(name);

        self.push_scope();
        for param in params {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve_stmts(body)?;
        self.pop_scope()?;

        self.current_function = enclosing_function;

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
}
