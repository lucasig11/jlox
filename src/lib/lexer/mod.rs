pub(crate) mod token;

use crate::lib::{
    error::InnerError,
    position::{Cursor, Position, Span},
    LoxResult,
};
use token::{Numeric, Punctuator, Token, TokenKind};

use self::token::Keyword;

pub struct Lexer<'a> {
    buffer: Cursor<'a>,
    start: Position,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
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
                    return Err(InnerError::new(
                        Span::new(self.start, self.buffer.pos()),
                        "unterminated string",
                    )
                    .into())
                }
            }
        }
        self.add_token(TokenKind::string_literal(buf));
        Ok(())
    }

    fn lex_numeric(&mut self, start: char) -> LoxResult<()> {
        let buf = self
            .buffer
            .take_char_while(start, |c| c.is_ascii_digit() || c == '.')?
            .parse::<Numeric>()
            .map_err(|e| {
                InnerError::new(Span::new(self.start, self.buffer.pos()), &e.to_string())
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

    fn lex_pipe(&mut self) -> LoxResult<()> {
        if let Some('>') = self.buffer.peek_next() {
            self.buffer.next_column();
            self.buffer.next();
            self.add_token(Punctuator::Pipe);
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
                '[' => self.add_token(OpenBracket),
                ']' => self.add_token(CloseBracket),
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
                '|' => self.lex_pipe()?,
                _ if ch.is_digit(10) => self.lex_numeric(ch)?,
                _ if ch.is_ascii_alphabetic() || ch.eq(&'_') => self.lex_identifier(ch)?,
                err => {
                    return Err(InnerError::new(
                        self.buffer.pos().into(),
                        &format!("unexpected character `{}`", err),
                    )
                    .into())
                }
            }
            self.start = self.buffer.pos();
        }
        Ok(self.tokens)
    }
}
