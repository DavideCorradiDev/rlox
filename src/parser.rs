use crate::{Expr, Stmt, Token, TokenKind, Value};

use thiserror::Error;

const MAX_PARAMETER_COUNT: usize = 255;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn run(tokens: Vec<Token>) -> Result<Vec<Stmt>, ParserErrors> {
        let parser = Self { tokens, current: 0 };
        parser.parse()
    }

    fn parse(mut self) -> Result<Vec<Stmt>, ParserErrors> {
        let mut statements = Vec::new();
        let mut errors = ParserErrors(Vec::new());
        while !self.is_at_end() {
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    errors.0.push(err);
                    self.synchronize();
                }
            }
        }
        if errors.0.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self
            .match_optional(|x| matches!(x.kind, TokenKind::Fun))
            .is_some()
        {
            self.function("function")
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::Var))
            .is_some()
        {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParserError> {
        let name = self.consume(
            |x| matches!(x.kind, TokenKind::Identifier(_)),
            ParserErrorKind::MissingFunName(kind.to_string()),
        )?;
        self.consume(
            |x| matches!(x.kind, TokenKind::LeftParen),
            ParserErrorKind::MissingLeftFunParen(kind.to_string()),
        )?;
        let mut params = Vec::new();
        if !matches!(self.peek().kind, TokenKind::RightParen) {
            loop {
                if params.len() >= MAX_PARAMETER_COUNT {
                    return Err(ParserError::new(
                        &self.peek(),
                        ParserErrorKind::TooManyParameters,
                    ));
                }
                params.push(self.consume(
                    |x| matches!(x.kind, TokenKind::Identifier(_)),
                    ParserErrorKind::MissingParameterName,
                )?);
                if self
                    .match_optional(|x| matches!(x.kind, TokenKind::Comma))
                    .is_none()
                {
                    break;
                }
            }
        }
        self.consume(
            |x| matches!(x.kind, TokenKind::RightParen),
            ParserErrorKind::MissingRightFunParen(kind.to_string()),
        )?;
        self.consume(
            |x| matches!(x.kind, TokenKind::LeftBrace),
            ParserErrorKind::MissingLeftFunBrace(kind.to_string()),
        )?;
        let body = self.block()?;
        if let Stmt::Block { statements } = body {
            Ok(Stmt::function(name, params, statements))
        }
        else {
            panic!("expected a block statement after function")
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.consume(
            |x| matches!(x.kind, TokenKind::Identifier(_)),
            ParserErrorKind::MissingVarName,
        )?;

        let initializer = if self
            .match_optional(|x| matches!(x.kind, TokenKind::Equal))
            .is_some()
        {
            self.expression()?
        } else {
            Expr::literal(Value::Nil)
        };

        self.consume(
            |x| matches!(x.kind, TokenKind::Semicolon),
            ParserErrorKind::MissingSemicolon,
        )?;

        Ok(Stmt::var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self
            .match_optional(|x| matches!(x.kind, TokenKind::Print))
            .is_some()
        {
            self.print_stmt()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::Return))
            .is_some()
        {
            self.return_stmt()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::LeftBrace))
            .is_some()
        {
            self.block()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::If))
            .is_some()
        {
            self.if_stmt()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::While))
            .is_some()
        {
            self.while_stmt()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::For))
            .is_some()
        {
            self.for_stmt()
        } else {
            self.expr_stmt()
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.consume(
            |x| matches!(x.kind, TokenKind::Semicolon),
            ParserErrorKind::MissingSemicolon,
        )?;
        Ok(Stmt::print(expr))
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParserError> {
        let keyword = self.prev();
        let expr = if matches!(self.peek().kind, TokenKind::Semicolon) {
            Expr::Literal { value: Value::Nil }
        } else {
            self.expression()?
        };
        self.consume(
            |x| matches!(x.kind, TokenKind::Semicolon),
            ParserErrorKind::MissingSemicolonAfterReturn,
        )?;
        Ok(Stmt::return_stmt(keyword, expr))
    }

    fn block(&mut self) -> Result<Stmt, ParserError> {
        let mut statements = Vec::new();
        while !self.is_at_end() && !matches!(self.peek().kind, TokenKind::RightBrace) {
            statements.push(self.declaration()?);
        }
        self.consume(
            |x| matches!(x.kind, TokenKind::RightBrace),
            ParserErrorKind::UnmatchedBrace,
        )?;
        Ok(Stmt::block(statements))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.consume(
            |x| matches!(x.kind, TokenKind::LeftParen),
            ParserErrorKind::MissingLeftIfParen,
        )?;
        let condition = self.expression()?;
        self.consume(
            |x| matches!(x.kind, TokenKind::RightParen),
            ParserErrorKind::MissingRightIfParen,
        )?;
        let then_branch = self.statement()?;
        let else_branch = if self
            .match_optional(|x| matches!(x.kind, TokenKind::Else))
            .is_some()
        {
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Stmt::if_stmt(condition, then_branch, else_branch))
    }

    fn while_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.consume(
            |x| matches!(x.kind, TokenKind::LeftParen),
            ParserErrorKind::MissingLeftWhileParen,
        )?;
        let condition = self.expression()?;
        self.consume(
            |x| matches!(x.kind, TokenKind::RightParen),
            ParserErrorKind::MissingRightWhileParen,
        )?;
        let body = self.statement()?;
        Ok(Stmt::while_stmt(condition, body))
    }

    fn for_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.consume(
            |x| matches!(x.kind, TokenKind::LeftParen),
            ParserErrorKind::MissingLeftForParen,
        )?;

        let initializer = if self
            .match_optional(|x| matches!(x.kind, TokenKind::Semicolon))
            .is_some()
        {
            None
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::Var))
            .is_some()
        {
            Some(self.var_declaration()?)
        } else {
            Some(self.expr_stmt()?)
        };

        let condition = if matches!(self.peek().kind, TokenKind::Semicolon) {
            Expr::literal(Value::from(true))
        } else {
            self.expression()?
        };
        self.consume(
            |x| matches!(x.kind, TokenKind::Semicolon),
            ParserErrorKind::MissingForSemicolon,
        )?;

        let increment = if matches!(self.peek().kind, TokenKind::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(
            |x| matches!(x.kind, TokenKind::RightParen),
            ParserErrorKind::MissingRightForParen,
        )?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::block(vec![body, Stmt::expression(increment)]);
        }

        body = Stmt::while_stmt(condition, body);

        if let Some(initializer) = initializer {
            body = Stmt::block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.consume(
            |x| matches!(x.kind, TokenKind::Semicolon),
            ParserErrorKind::MissingSemicolon,
        )?;
        Ok(Stmt::expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.or()?;
        if let Some(token) = self.match_optional(|x| matches!(x.kind, TokenKind::Equal)) {
            let value = self.assignment()?;
            match expr {
                Expr::Variable { name, .. } => Ok(Expr::assign(name, value)),
                _ => Err(ParserError::new(&token, ParserErrorKind::InvalidAssignment)),
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        self.match_many_optional_logical(|x| matches!(x.kind, TokenKind::Or), Self::and)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        self.match_many_optional_logical(|x| matches!(x.kind, TokenKind::And), Self::equality)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        self.match_many_optional(
            |x| matches!(x.kind, TokenKind::BangEqual | TokenKind::EqualEqual),
            Self::comparison,
        )
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        self.match_many_optional(
            |x| {
                matches!(
                    x.kind,
                    TokenKind::Greater
                        | TokenKind::GreaterEqual
                        | TokenKind::Less
                        | TokenKind::LessEqual
                )
            },
            Self::term,
        )
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        self.match_many_optional(
            |x| matches!(x.kind, TokenKind::Minus | TokenKind::Plus),
            Self::factor,
        )
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        self.match_many_optional(
            |x| matches!(x.kind, TokenKind::Slash | TokenKind::Star),
            Self::unary,
        )
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) =
            self.match_optional(|x| matches!(x.kind, TokenKind::Minus | TokenKind::Bang))
        {
            let right = self.unary()?;
            Ok(Expr::unary(token, right))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        loop {
            if let Some(_) = self.match_optional(|x| matches!(x.kind, TokenKind::LeftParen)) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments = Vec::new();
        if !matches!(self.peek().kind, TokenKind::RightParen) {
            arguments.push(Box::new(self.expression()?));
            while let Some(_) = self.match_optional(|x| matches!(x.kind, TokenKind::Comma)) {
                if arguments.len() >= MAX_PARAMETER_COUNT {
                    return Err(ParserError::new(
                        &self.peek(),
                        ParserErrorKind::TooManyArguments,
                    ));
                }
                arguments.push(Box::new(self.expression()?));
            }
        }
        let paren = self.consume(
            |x| matches!(x.kind, TokenKind::RightParen),
            ParserErrorKind::MissingRightFunctionParen,
        )?;
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let token = self.next();
        match token.kind {
            TokenKind::Nil => Ok(Expr::literal(Value::Nil)),
            TokenKind::False => Ok(Expr::literal(false)),
            TokenKind::True => Ok(Expr::literal(true)),
            TokenKind::Number(n) => Ok(Expr::literal(n)),
            TokenKind::String(s) => Ok(Expr::literal(s)),
            TokenKind::Identifier(_) => Ok(Expr::variable(token)),
            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.consume(
                    |x| matches!(x.kind, TokenKind::RightParen),
                    ParserErrorKind::UnmatchedParen,
                )?;
                Ok(Expr::grouping(expr))
            }
            TokenKind::BangEqual | TokenKind::EqualEqual => {
                self.equality()?;
                Err(ParserError::new(
                    &token,
                    ParserErrorKind::MissingLeftHandOperand,
                ))
            }
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual => {
                self.comparison()?;
                Err(ParserError::new(
                    &token,
                    ParserErrorKind::MissingLeftHandOperand,
                ))
            }
            TokenKind::Plus => {
                self.term()?;
                Err(ParserError::new(
                    &token,
                    ParserErrorKind::MissingLeftHandOperand,
                ))
            }
            TokenKind::Slash | TokenKind::Star => {
                self.factor()?;
                Err(ParserError::new(
                    &token,
                    ParserErrorKind::MissingLeftHandOperand,
                ))
            }
            _ => Err(ParserError::new(&token, ParserErrorKind::InvalidExpression)),
        }
    }

    fn match_optional(&mut self, match_fn: fn(&Token) -> bool) -> Option<Token> {
        let token = self.peek();
        if match_fn(&token) {
            self.advance();
            Some(token)
        } else {
            None
        }
    }

    fn match_many_optional(
        &mut self,
        match_fn: fn(&Token) -> bool,
        sub_fn: fn(&mut Self) -> Result<Expr, ParserError>,
    ) -> Result<Expr, ParserError> {
        let mut expr = sub_fn(self)?;
        while let Some(token) = self.match_optional(match_fn) {
            let right = sub_fn(self)?;
            expr = Expr::binary(token, expr, right);
        }
        Ok(expr)
    }

    fn match_many_optional_logical(
        &mut self,
        match_fn: fn(&Token) -> bool,
        sub_fn: fn(&mut Self) -> Result<Expr, ParserError>,
    ) -> Result<Expr, ParserError> {
        let mut expr = sub_fn(self)?;
        while let Some(token) = self.match_optional(match_fn) {
            let right = sub_fn(self)?;
            expr = Expr::logical(token, expr, right);
        }
        Ok(expr)
    }

    fn consume(
        &mut self,
        match_fn: fn(&Token) -> bool,
        error_kind: ParserErrorKind,
    ) -> Result<Token, ParserError> {
        let token = self.peek();
        if match_fn(&token) {
            self.advance();
            Ok(token)
        } else {
            Err(ParserError::new(&token, error_kind))
        }
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            match self.next().kind {
                TokenKind::Semicolon => break,
                _ => match self.peek().kind {
                    TokenKind::Class
                    | TokenKind::Fun
                    | TokenKind::Var
                    | TokenKind::If
                    | TokenKind::For
                    | TokenKind::While
                    | TokenKind::Print
                    | TokenKind::Return => {
                        break;
                    }
                    _ => (),
                },
            }
        }
    }

    fn advance(&mut self) {
        assert!(self.current + 1 < self.tokens.len());
        self.current += 1
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn next(&mut self) -> Token {
        let token = self.peek();
        match token.kind {
            TokenKind::Eof => (),
            _ => self.advance(),
        }
        token
    }

    fn prev(&self) -> Token {
        assert!(self.current > 0, "Attempting to read previous token at the start");
        self.tokens[self.current - 1].clone()
    }

    fn is_at_end(&self) -> bool {
        if let TokenKind::Eof = self.peek().kind {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Error)]
pub struct ParserErrors(Vec<ParserError>);

impl std::fmt::Display for ParserErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error_msg = self
            .0
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        write!(f, "{error_msg}")
    }
}

#[derive(Debug, Clone, Error)]
pub struct ParserError {
    pub line: usize,
    pub location: String,
    pub kind: ParserErrorKind,
}

impl ParserError {
    pub fn new(token: &Token, kind: ParserErrorKind) -> Self {
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

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {} at {}] {}",
            self.line, self.location, self.kind
        )
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParserErrorKind {
    #[error("Expected expression")]
    InvalidExpression,
    #[error("Expected ')' after expression")]
    UnmatchedParen,
    #[error("Expected '}}' after block")]
    UnmatchedBrace,
    #[error("Expected ':' after '?' and expression")]
    UnmatchedQuestionMark,
    #[error("Missing left-hand operand")]
    MissingLeftHandOperand,
    #[error("Expected ';' after expression")]
    MissingSemicolon,
    #[error("Expected variable name")]
    MissingVarName,
    #[error("Expected '(' after 'if'")]
    MissingLeftIfParen,
    #[error("Expected ')' after 'if' condition")]
    MissingRightIfParen,
    #[error("Expected '(' after 'while'")]
    MissingLeftWhileParen,
    #[error("Expected ')' after 'while' condition")]
    MissingRightWhileParen,
    #[error("Expected '(' after 'for'")]
    MissingLeftForParen,
    #[error("Expected ')' after 'for' clauses")]
    MissingRightForParen,
    #[error("Expected ';' after 'for' condition")]
    MissingForSemicolon,
    #[error("Expected ')' after arguments")]
    MissingRightFunctionParen,
    #[error("Can't have more than {} arguments", MAX_PARAMETER_COUNT)]
    TooManyArguments,
    #[error("Invalid assignment target")]
    InvalidAssignment,
    #[error("Expected {0} name")]
    MissingFunName(String),
    #[error("Expected '(' after {0} name")]
    MissingLeftFunParen(String),
    #[error("Expected ')' after {0} parameters")]
    MissingRightFunParen(String),
    #[error("Expected '{{' before {0} body")]
    MissingLeftFunBrace(String),
    #[error("Expected parameter name")]
    MissingParameterName,
    #[error("Can't have more than {} parameters", MAX_PARAMETER_COUNT)]
    TooManyParameters,
    #[error("Expected ';' after return value")]
    MissingSemicolonAfterReturn,
}
