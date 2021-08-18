use crate::error::ScanError;
use crate::lox::{
    position::{Cursor, Position, Span},
    token::{Keyword, Numeric, Punctuator, Token, TokenKind},
    LoxResult,
};

pub struct Scanner {
    src: String,
    filename: String,
    tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(filename: String, src: String) -> Self {
        Self {
            src,
            filename,
            tokens: Default::default(),
        }
    }

    pub(crate) fn scan_tokens(mut self) -> LoxResult<Vec<Token>> {
        let mut cursor = Cursor::new();
        let mut start = Position::new(1, 1);
        let mut iter = self.src.chars().into_iter().peekable();

        while let Some(ch) = iter.next() {
            match ch {
                '\n' | '\r' => {
                    cursor.next_line();
                    start = cursor.pos();
                    continue;
                }
                '(' => {
                    self.tokens
                        .push(Token::new(Punctuator::OpenParen, "(", cursor.pos().into()));
                }
                ')' => {
                    self.tokens
                        .push(Token::new(Punctuator::CloseParen, ")", cursor.pos().into()));
                }
                '[' => {
                    self.tokens
                        .push(Token::new(Punctuator::OpenBlock, "[", cursor.pos().into()));
                }
                ']' => {
                    self.tokens
                        .push(Token::new(Punctuator::CloseBlock, "]", cursor.pos().into()));
                }
                ',' => {
                    self.tokens
                        .push(Token::new(Punctuator::Comma, ",", cursor.pos().into()));
                }
                '.' => {
                    self.tokens
                        .push(Token::new(Punctuator::Dot, ".", cursor.pos().into()));
                }
                ';' => {
                    self.tokens
                        .push(Token::new(Punctuator::Semicolon, ";", cursor.pos().into()));
                }
                '+' => {
                    self.tokens
                        .push(Token::new(Punctuator::Add, "+", cursor.pos().into()));
                }
                '-' => {
                    self.tokens
                        .push(Token::new(Punctuator::Sub, "-", cursor.pos().into()));
                }
                '!' => {
                    let token = match iter.next_if_eq(&'=') {
                        Some(_) => {
                            cursor.next_column();
                            Token::new(
                                Punctuator::NotEq,
                                format!("{}", Punctuator::NotEq),
                                Span::new(start, cursor.pos()),
                            )
                        }
                        _ => Token::new(Punctuator::Not, ch.to_string(), cursor.pos().into()),
                    };
                    self.tokens.push(token);
                }
                e @ _ => {
                    return Err(ScanError::error(
                        format!("unexpected character `{}`", e),
                        self.filename,
                        self.src
                            .lines()
                            .collect::<Vec<&str>>()
                            .get(start.line_number() as usize - 1)
                            .unwrap()
                            .to_string(),
                        cursor.pos().into(),
                    )
                    .into())
                }
            }
            start = cursor.pos();
            cursor.next_column();
        }
        Ok(self.tokens)
    }
}
