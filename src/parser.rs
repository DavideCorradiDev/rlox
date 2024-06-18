use crate::{Expr, Stmt, Token, TokenKind, Value};

use thiserror::Error;

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
            .match_optional(|x| matches!(x.kind, TokenKind::Var))
            .is_some()
        {
            self.var_declaration()
        } else {
            self.statement()
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
            self.print_statement()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::LeftBrace))
            .is_some()
        {
            self.block_statement()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::If))
            .is_some()
        {
            self.if_statement()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::While))
            .is_some()
        {
            self.while_statement()
        } else if self
            .match_optional(|x| matches!(x.kind, TokenKind::For))
            .is_some()
        {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.consume(
            |x| matches!(x.kind, TokenKind::Semicolon),
            ParserErrorKind::MissingSemicolon,
        )?;
        Ok(Stmt::print(expr))
    }

    fn block_statement(&mut self) -> Result<Stmt, ParserError> {
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

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
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
        Ok(Stmt::if_statement(condition, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
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
        Ok(Stmt::while_statement(condition, body))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
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
            Some(self.expression_statement()?)
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

        body = Stmt::while_statement(condition, body);

        if let Some(initializer) = initializer {
            body = Stmt::block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
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
                Expr::Variable { name } => Ok(Expr::assign(name, value)),
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
            self.primary()
        }
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
            "[line {} at {}] Error: {}",
            self.line, self.location, self.kind
        )
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParserErrorKind {
    #[error("expected expression")]
    InvalidExpression,
    #[error("expected ')' after expression")]
    UnmatchedParen,
    #[error("expected '}}' after block")]
    UnmatchedBrace,
    #[error("expected ':' after '?' and expression")]
    UnmatchedQuestionMark,
    #[error("missing left-hand operand")]
    MissingLeftHandOperand,
    #[error("expected ';' after expression")]
    MissingSemicolon,
    #[error("expected variable name")]
    MissingVarName,
    #[error("expected '(' after 'if'")]
    MissingLeftIfParen,
    #[error("expected ')' after 'if' condition")]
    MissingRightIfParen,
    #[error("expected '(' after 'while'")]
    MissingLeftWhileParen,
    #[error("expected ')' after 'while' condition")]
    MissingRightWhileParen,
    #[error("expected '(' after 'for'")]
    MissingLeftForParen,
    #[error("expected ')' after 'for' clauses")]
    MissingRightForParen,
    #[error("expected ';' after 'for' condition")]
    MissingForSemicolon,
    #[error("invalid assignment target")]
    InvalidAssignment,
}
