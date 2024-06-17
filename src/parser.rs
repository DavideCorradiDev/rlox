use crate::{Expr, Token, TokenKind, Value};

use thiserror::Error;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn run(mut self) -> Result<Expr, ParserError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.condition()
    }

    fn condition(&mut self) -> Result<Expr, ParserError> {
        let expr = self.comma()?;
        let token = self.peek();
        match token.kind {
            TokenKind::QuestionMark => {
                self.advance();
                let left = self.comma()?;
                let middle_token = self.peek();
                match middle_token.kind {
                    TokenKind::Colon => {
                        self.advance();
                        let right = self.comma()?;
                        Ok(Expr::ternary(token, expr, left, right))
                    }
                    _ => Err(ParserError::new(
                        &middle_token,
                        ParserErrorKind::UnmatchedQuestionMark,
                    )),
                }
            }
            _ => Ok(expr),
        }
    }

    fn comma(&mut self) -> Result<Expr, ParserError> {
        self.match_many_optional(|x| matches!(x.kind, TokenKind::Comma), Self::equality)
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
        let token = self.peek();
        match token.kind {
            TokenKind::Minus | TokenKind::Bang => {
                self.advance();
                let right = self.unary()?;
                Ok(Expr::unary(token, right))
            }
            _ => self.primary(),
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
            TokenKind::LeftParen => {
                let expr = self.expression()?;
                let token = self.peek();
                match token.kind {
                    TokenKind::RightParen => {
                        self.advance();
                        Ok(Expr::grouping(expr))
                    }
                    _ => Err(ParserError::new(&token, ParserErrorKind::UnmatchedParen)),
                }
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

    fn match_many_optional(
        &mut self,
        match_fn: fn(&Token) -> bool,
        sub_fn: fn(&mut Self) -> Result<Expr, ParserError>,
    ) -> Result<Expr, ParserError> {
        let mut expr = sub_fn(self)?;
        loop {
            let token = self.peek();
            if match_fn(&token) {
                self.advance();
                let right = sub_fn(self)?;
                expr = Expr::binary(token, expr, right);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // fn synchronize(&mut self) {
    //     while let Some(token) = self.next() {
    //         match token.kind {
    //             TokenKind::Semicolon => { break },
    //             _ => if let Some(token) = self.peek() {
    //                 match token.kind {
    //                     TokenKind::Class | TokenKind::Fun | TokenKind::Var | TokenKind::If |
    //                     TokenKind::For | TokenKind::While | TokenKind::Print | TokenKind::Return
    // => {                         break;
    //                     }
    //                     _ => { self.advance() }
    //                 }
    //             }
    //         }
    //     }
    // }

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
    #[error("expected ':' after '?' and expression")]
    UnmatchedQuestionMark,
    #[error("missing left-hand operand")]
    MissingLeftHandOperand,
}
