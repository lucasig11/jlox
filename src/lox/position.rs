use std::num::NonZeroU32;

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

pub struct Cursor {
    pos: Position,
}

impl Cursor {
    pub fn new() -> Self {
        Self {
            pos: Position::new(1, 1),
        }
    }

    pub fn pos(&self) -> Position {
        self.pos
    }

    pub fn next_line(&mut self) {
        let line = self.pos.line_number() + 1;
        self.pos = Position::new(line, 1)
    }

    pub fn next_column(&mut self) {
        let line = self.pos.line_number();
        let col = self.pos.column_number() + 1;
        self.pos = Position::new(line, col)
    }
}
