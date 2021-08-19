use crate::lox::LoxResult;
use std::{iter::Peekable, num::NonZeroU32, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    line: NonZeroU32,
    col: NonZeroU32,
}

impl Position {
    #[inline]
    #[track_caller]
    pub fn new(line: u32, col: u32) -> Self {
        Self {
            line: NonZeroU32::new(line).expect("line number cannot be 0"),
            col: NonZeroU32::new(col).expect("line number cannot be 0"),
        }
    }

    #[inline]
    pub fn line_number(self) -> u32 {
        self.line.get()
    }

    #[inline]
    pub fn column_number(self) -> u32 {
        self.col.get()
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    /// Creates a new Span from two positions.
    #[inline]
    pub fn new(start: Position, end: Position) -> Self {
        assert!(start <= end, "a span cannot start after its end");
        Self { start, end }
    }

    /// Consumes `self` returning the start position.
    #[inline]
    pub fn start(self) -> Position {
        self.start
    }

    /// Consumes `self` returning the end position.
    #[inline]
    pub fn end(self) -> Position {
        self.end
    }
}

impl From<Position> for Span {
    fn from(pos: Position) -> Span {
        Span {
            start: pos,
            end: pos,
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}..{}]", self.start, self.end)
    }
}

type InnerIter<'a> = Peekable<Chars<'a>>;

#[derive(Debug)]
pub(crate) struct Cursor<'a> {
    iter: InnerIter<'a>,
    pos: Position,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(inner: InnerIter<'a>) -> Self {
        Self {
            iter: inner,
            pos: Position::new(1, 1),
        }
    }

    #[inline]
    pub fn pos(&self) -> Position {
        self.pos
    }

    #[inline]
    pub fn next_line(&mut self) {
        let line = self.pos.line_number() + 1;
        self.pos = Position::new(line, 1);
    }

    #[inline]
    pub fn next_column(&mut self) {
        let line = self.pos.line_number();
        let col = self.pos.column_number() + 1;
        self.pos = Position::new(line, col);
    }

    pub fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    pub fn consume_until(&mut self, ch: char) {
        for t in &mut self.iter {
            if t == ch {
                break;
            }
        }
    }

    pub fn peek_next(&mut self) -> Option<char> {
        self.iter.peek().copied()
    }

    pub fn take_char_while(
        &mut self,
        start: char,
        mut f: impl FnMut(char) -> bool,
    ) -> LoxResult<String> {
        let mut buf = String::from(start);
        while let Some(c) = self.peek_next() {
            if !f(c) {
                break;
            }
            self.next_column();
            self.next();
            buf.push(c);
        }
        Ok(buf)
    }
}
