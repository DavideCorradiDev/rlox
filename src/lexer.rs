use thiserror::Error;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    tokens: Vec<Token>,
    errors: LexerErrors,
    source: &'a str,
    iter: std::iter::Peekable<std::str::CharIndices<'a>>,
    start_idx: usize,
    current_idx: usize,
    current_line: usize,
}

impl<'a> Lexer<'a> {
    pub fn run(source: &'a str) -> Result<Vec<Token>, LexerErrors> {
        let lexer = Self {
            tokens: Vec::new(),
            errors: LexerErrors(Vec::new()),
            source: source,
            iter: source.char_indices().peekable(),
            start_idx: 0,
            current_idx: 0,
            current_line: 1,
        };
        lexer.scan()
    }

    fn scan(mut self) -> Result<Vec<Token>, LexerErrors> {
        while self.scan_token() {}

        self.tokens.push(Token {
            line: self.current_line,
            lexeme: String::new(),
            kind: TokenKind::Eof,
        });

        if self.errors.0.is_empty() {
            Ok(self.tokens)
        } else {
            Err(self.errors)
        }
    }

    fn scan_token(&mut self) -> bool {
        self.start_idx = self.current_idx;
        match self.next() {
            Some(c) => {
                match c {
                    '(' => self.add_token(TokenKind::LeftParen),
                    ')' => self.add_token(TokenKind::RightParen),
                    '{' => self.add_token(TokenKind::LeftBrace),
                    '}' => self.add_token(TokenKind::RightBrace),
                    ',' => self.add_token(TokenKind::Comma),
                    '.' => self.add_token(TokenKind::Dot),
                    '-' => self.add_token(TokenKind::Minus),
                    '+' => self.add_token(TokenKind::Plus),
                    ';' => self.add_token(TokenKind::Semicolon),
                    ':' => self.add_token(TokenKind::Colon),
                    '*' => self.add_token(TokenKind::Star),
                    '?' => self.add_token(TokenKind::QuestionMark),

                    '!' => self.add_token_switch('=', TokenKind::BangEqual, TokenKind::Bang),
                    '=' => self.add_token_switch('=', TokenKind::EqualEqual, TokenKind::Equal),
                    '<' => self.add_token_switch('=', TokenKind::LessEqual, TokenKind::Less),
                    '>' => self.add_token_switch('=', TokenKind::GreaterEqual, TokenKind::Greater),

                    '/' => {
                        if self.next_if_eq('/') {
                            self.skip_comment_line();
                        } else if self.next_if_eq('*') {
                            self.skip_comment_block();
                        } else {
                            self.add_token(TokenKind::Slash);
                        }
                    }

                    '"' => self.add_string_token(),
                    '0'..='9' => self.add_number_token(),
                    'a'..='z' | 'A'..='Z' | '_' => self.add_keyword_or_identifier_token(),

                    ' ' | '\r' | '\t' => (),
                    '\n' => self.current_line += 1,

                    _ => self.add_error(LexerErrorKind::UnexpectedCharacter(c)),
                }
                true
            }
            None => false,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|(_, c)| *c)
    }

    fn next_peek(&mut self) -> Option<char> {
        let mut iter_copy = self.iter.clone();
        iter_copy.next();
        iter_copy.peek().map(|(_, c)| *c)
    }

    fn next(&mut self) -> Option<char> {
        let (i, c) = self.iter.next()?;
        self.current_idx = i + 1;
        Some(c)
    }

    fn next_if_eq(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(c) => {
                if c == expected {
                    self.next();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            lexeme: String::from(&self.source[self.start_idx..self.current_idx]),
            line: self.current_line,
        });
    }

    fn add_token_switch(
        &mut self,
        expected_next_char: char,
        double_kind: TokenKind,
        single_kind: TokenKind,
    ) {
        let token_kind = if self.next_if_eq(expected_next_char) {
            double_kind
        } else {
            single_kind
        };
        self.add_token(token_kind);
    }

    fn add_string_token(&mut self) {
        while let Some(c) = self.next() {
            if c == '"' {
                self.add_token(TokenKind::String(String::from(
                    &self.source[(self.start_idx + 1)..(self.current_idx - 1)],
                )));
                return;
            } else if c == '\n' {
                self.current_line += 1;
            }
        }
        self.add_error(LexerErrorKind::UnterminatedString);
    }

    fn add_number_token(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => {
                    self.next();
                }
                '.' => match self.next_peek() {
                    Some(c) => match c {
                        '0'..='9' => {
                            self.next();
                        }
                        _ => break,
                    },
                    None => break,
                },
                _ => break,
            }
        }
        self.add_token(TokenKind::Number(
            self.source[self.start_idx..self.current_idx]
                .parse::<f64>()
                .expect("failed to parse string to number"),
        ));
    }

    fn add_keyword_or_identifier_token(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                    self.next();
                }
                _ => break,
            }
        }

        let keyword_or_identifier = String::from(&self.source[self.start_idx..self.current_idx]);
        let token_kind = match keyword_or_identifier.as_str() {
            "and" => TokenKind::And,
            "class" => TokenKind::Class,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "fun" => TokenKind::Fun,
            "if" => TokenKind::If,
            "nil" => TokenKind::Nil,
            "or" => TokenKind::Or,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            "super" => TokenKind::Super,
            "this" => TokenKind::This,
            "true" => TokenKind::True,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            _ => TokenKind::Identifier(keyword_or_identifier),
        };
        self.add_token(token_kind);
    }

    fn skip_comment_line(&mut self) {
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.next();
        }
    }

    fn skip_comment_block(&mut self) {
        while let Some(c) = self.next() {
            if c == '\n' {
                self.current_line += 1;
            } else if c == '*' && self.next_if_eq('/') {
                return;
            } else if c == '/' && self.next_if_eq('*') {
                self.skip_comment_block();
            }
        }
        self.add_error(LexerErrorKind::UnterminatedComment);
    }

    fn add_error(&mut self, kind: LexerErrorKind) {
        self.errors.0.push(LexerError {
            kind,
            line: self.current_line,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub line: usize,
    pub lexeme: String,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Colon,
    Slash,
    Star,
    QuestionMark,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier(String),
    String(String),
    Number(f64),
    // Keywords.
    True,
    False,
    And,
    Or,
    Var,
    Fun,
    Class,
    Super,
    This,
    If,
    Else,
    For,
    While,
    Return,
    Nil,
    Eof,
    Print,
}

#[derive(Debug, Clone, Error)]
pub struct LexerErrors(Vec<LexerError>);

impl std::fmt::Display for LexerErrors {
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
pub struct LexerError {
    pub line: usize,
    pub kind: LexerErrorKind,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.kind)
    }
}

#[derive(Debug, Clone, Error)]
pub enum LexerErrorKind {
    #[error("unexpected character '{0}'")]
    UnexpectedCharacter(char),
    #[error("unterminated string")]
    UnterminatedString,
    #[error("unterminated comment")]
    UnterminatedComment,
}
