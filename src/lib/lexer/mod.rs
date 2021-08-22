pub(crate) mod token;

use crate::error::ScanError;
use crate::lib::{
    position::{Cursor, Position, Span},
    LoxResult,
};
use token::{Numeric, Punctuator, Token, TokenKind};

use self::token::Keyword;

pub(crate) trait Tokenizer {
    fn lex<'a>(s: &str, lexer: &mut Lexer<'a>) -> LoxResult<()>;
}

pub struct Lexer<'a> {
    src: String,
    buffer: Cursor<'a>,
    start: Position,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            src: src.to_string(),
            start: Position::new(1, 1),
            buffer: Cursor::new(src.chars().peekable()),
            tokens: Default::default(),
        }
    }

    fn add_token(&mut self, tk: impl Into<TokenKind>) {
        let token = Token::new(tk.into(), Span::new(self.start, self.buffer.pos()));
        self.tokens.push(token);
        self.buffer.next_column();
    }

    fn add_if_next<I, F: FnOnce(&mut Self, I)>(&mut self, cmp: char, eq: I, neq: I, strategy: F) {
        match self.buffer.peek_next() {
            Some(t) if t == cmp => {
                self.buffer.next_column();
                self.buffer.next();
                strategy(self, eq);
            }
            _ => strategy(self, neq),
        }
    }

    fn get_line_content(&self, line: u32) -> String {
        self.src
            .lines()
            .collect::<Vec<&str>>()
            .get(line as usize - 1)
            .unwrap()
            .to_string()
    }

    fn lex_string(&mut self) -> LoxResult<()> {
        let mut buf = String::new();
        loop {
            match self.buffer.next() {
                Some(c) if c != '\n' => {
                    self.buffer.next_column();
                    if c == '"' {
                        break;
                    }
                    buf.push(c);
                }
                _ => {
                    return Err(ScanError::new(
                        "unterminated string",
                        self.get_line_content(self.start.line_number()),
                        Span::new(self.start, self.buffer.pos()),
                    )
                    .into())
                }
            }
        }
        self.add_token(TokenKind::string_literal(buf));
        Ok(())
    }

    // TODO: create Tokenizer trait
    fn lex_numeric(&mut self, start: char) -> LoxResult<()> {
        let buf = self
            .buffer
            .take_char_while(start, |c| c.is_ascii_digit() || c == '.')?
            .parse::<Numeric>()
            .map_err(|e| {
                ScanError::new(
                    e.to_string(),
                    self.get_line_content(self.buffer.pos().line_number()),
                    Span::new(self.start, self.buffer.pos()),
                )
            })?;
        self.add_token(TokenKind::numeric_literal(buf));
        Ok(())
    }

    fn lex_identifier(&mut self, start: char) -> LoxResult<()> {
        let ident = match self
            .buffer
            .take_char_while(start, |c| c.is_ascii_alphanumeric())?
            .parse::<Keyword>()
        {
            Ok(kw) => TokenKind::keyword(kw),
            Err(ident) => match ident.as_str() {
                "true" => true.into(),
                "false" => false.into(),
                _ => TokenKind::identifier(ident),
            },
        };
        self.add_token(ident);
        Ok(())
    }

    fn lex_slash(&mut self) -> LoxResult<()> {
        match self.buffer.peek_next() {
            Some('/') => {
                self.buffer.consume_until('\n');
                self.buffer.next_line();
            }
            _ => self.add_if_next('=', Punctuator::AssignDiv, Punctuator::Div, Self::add_token),
        }
        Ok(())
    }

    pub(crate) fn scan_tokens(mut self) -> LoxResult<Vec<Token>> {
        use Punctuator::*;
        while let Some(ch) = self.buffer.next() {
            match ch {
                '\n' | '\r' => self.buffer.next_line(),
                ' ' | '\t' => self.buffer.next_column(),
                '(' => self.add_token(OpenParen),
                ')' => self.add_token(CloseParen),
                '{' => self.add_token(OpenBlock),
                '}' => self.add_token(CloseBlock),
                ',' => self.add_token(Comma),
                '.' => self.add_token(Dot),
                ';' => self.add_token(Semicolon),
                '+' => self.add_if_next('=', AssignAdd, Add, Self::add_token),
                '*' => self.add_if_next('=', AssignMul, Mul, Self::add_token),
                '-' => self.add_if_next('=', AssignSub, Sub, Self::add_token),
                '!' => self.add_if_next('=', NotEq, Not, Self::add_token),
                '=' => self.add_if_next('=', Eq, Assign, Self::add_token),
                '>' => self.add_if_next('=', GreaterThanOrEq, GreaterThan, Self::add_token),
                '<' => self.add_if_next('=', LessThanOrEq, LessThan, Self::add_token),
                '/' => self.lex_slash()?,
                '"' => self.lex_string()?,
                _ if ch.is_digit(10) => self.lex_numeric(ch)?,
                _ if ch.is_ascii_alphabetic() || ch.eq(&'_') => self.lex_identifier(ch)?,
                err => {
                    return Err(ScanError::new(
                        format!("unexpected character `{}`", err),
                        self.get_line_content(self.start.line_number()),
                        self.buffer.pos().into(),
                    )
                    .into())
                }
            }
            self.start = self.buffer.pos();
        }
        Ok(self.tokens)
    }
}
