#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token;

pub enum TokenKind {
    Keyword(Keyword),
    Punctuator(Punctuator),
    Identifier(Box<str>),
    StringLiteral(Box<str>),
    NumericLiteral(Numeric),
    BooleanLiteral(bool),
    Comment,
    EOF,
}

pub enum Punctuator {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Assign,
    AssignAdd,
    AssignSub,
    Eq,
    Comma,
    Dot,
    Minus,
    Add,
    Semicolon,
    Slash,
    Mul,
    Not,
    NotEq,
    GreaterThan,
    GreaterThanOrEq,
    LessThan,
    LessThanOrEq,
}

pub enum Keyword {
    And,
    Class,
    Else,
    Let,
    While,
    Fn,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    Extends,
}

pub enum Numeric {
    Integer(usize),
    Decimal(f64),
}
