use std::str::FromStr;

use crate::lib::{error::LoxError, position::Span};

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub(crate) fn new<K: Into<TokenKind>>(kind: K, span: Span) -> Self {
        Self {
            kind: kind.into(),
            span,
        }
    }

    pub(crate) fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub(crate) fn span(&self) -> &Span {
        &self.span
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub(crate) enum TokenKind {
    Keyword(Keyword),
    Punctuator(Punctuator),
    Identifier(Box<str>),
    StringLiteral(Box<str>),
    NumericLiteral(Numeric),
    BooleanLiteral(bool),
}

impl From<bool> for TokenKind {
    fn from(oth: bool) -> Self {
        Self::BooleanLiteral(oth)
    }
}

impl From<Keyword> for TokenKind {
    fn from(kw: Keyword) -> Self {
        Self::Keyword(kw)
    }
}

impl From<Punctuator> for TokenKind {
    fn from(punc: Punctuator) -> Self {
        Self::Punctuator(punc)
    }
}

impl From<Numeric> for TokenKind {
    fn from(num: Numeric) -> Self {
        Self::NumericLiteral(num)
    }
}

impl TokenKind {
    pub fn keyword(kw: Keyword) -> Self {
        Self::Keyword(kw)
    }

    pub fn identifier<I: Into<Box<str>>>(ident: I) -> Self {
        Self::Identifier(ident.into())
    }

    pub fn string_literal<S: Into<Box<str>>>(lit: S) -> Self {
        Self::StringLiteral(lit.into())
    }

    pub fn numeric_literal<N: Into<Numeric>>(lit: N) -> Self {
        Self::NumericLiteral(lit.into())
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TokenKind::Keyword(ref k) => write!(f, "{}", k),
            TokenKind::Identifier(ref ident) => write!(f, "{}", ident),
            TokenKind::Punctuator(ref punc) => write!(f, "{}", punc),
            TokenKind::StringLiteral(ref s) => write!(f, "{}", s),
            TokenKind::NumericLiteral(Numeric::Integer(n)) => write!(f, "{}", n),
            TokenKind::NumericLiteral(Numeric::Decimal(n)) => write!(f, "{}", n),
            TokenKind::BooleanLiteral(ref b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Punctuator {
    OpenParen,
    CloseParen,
    OpenBlock,
    CloseBlock,
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    Not,
    Eq,
    NotEq,
    Add,
    Sub,
    Mul,
    Div,
    Dot,
    Comma,
    Semicolon,
    GreaterThan,
    GreaterThanOrEq,
    LessThan,
    LessThanOrEq,
}

impl std::fmt::Display for Punctuator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Punctuator::OpenParen => "(",
                Punctuator::CloseParen => ")",
                Punctuator::OpenBlock => "{",
                Punctuator::CloseBlock => "}",
                Punctuator::Comma => ",",
                Punctuator::Dot => ".",
                Punctuator::Semicolon => ";",
                Punctuator::Assign => "=",
                Punctuator::AssignAdd => "+=",
                Punctuator::AssignSub => "-=",
                Punctuator::AssignDiv => "/=",
                Punctuator::AssignMul => "*=",
                Punctuator::Eq => "==",
                Punctuator::Sub => "-",
                Punctuator::Add => "+",
                Punctuator::Div => "/",
                Punctuator::Mul => "*",
                Punctuator::GreaterThan => ">",
                Punctuator::GreaterThanOrEq => ">=",
                Punctuator::LessThan => "<",
                Punctuator::LessThanOrEq => "<=",
                Punctuator::Not => "!",
                Punctuator::NotEq => "!=",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Keyword::And => "and",
                Keyword::Class => "class",
                Keyword::Else => "else",
                Keyword::Let => "let",
                Keyword::While => "while",
                Keyword::Fn => "fn",
                Keyword::For => "for",
                Keyword::If => "if",
                Keyword::Nil => "nil",
                Keyword::Or => "or",
                Keyword::Print => "print",
                Keyword::Return => "return",
                Keyword::Super => "super",
                Keyword::This => "this",
                Keyword::Extends => "extends",
            }
        )
    }
}

impl FromStr for Keyword {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "and" => Ok(Keyword::And),
            "class" => Ok(Keyword::Class),
            "else" => Ok(Keyword::Else),
            "let" => Ok(Keyword::Let),
            "while" => Ok(Keyword::While),
            "fn" => Ok(Keyword::Fn),
            "for" => Ok(Keyword::For),
            "if" => Ok(Keyword::If),
            "nil" => Ok(Keyword::Nil),
            "or" => Ok(Keyword::Or),
            "print" => Ok(Keyword::Print),
            "return" => Ok(Keyword::Return),
            "super" => Ok(Keyword::Super),
            "this" => Ok(Keyword::This),
            "extends" => Ok(Keyword::Extends),
            _ => Err(s.to_owned()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Numeric {
    Integer(isize),
    Decimal(f64),
}

impl Numeric {
    pub fn inner(&self) -> f64 {
        match &self {
            Self::Integer(i) => *i as f64,
            Self::Decimal(d) => *d,
        }
    }
}

impl std::cmp::Eq for Numeric {}

impl std::cmp::PartialEq for Numeric {
    fn eq(&self, oth: &Self) -> bool {
        self.inner().eq(&oth.inner())
    }
}

impl std::hash::Hash for Numeric {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self {
            Self::Integer(i) => i.hash(state),
            // please dont yell at me for this ok
            Self::Decimal(f) => (*f as u64).hash(state),
        }
    }
}
impl FromStr for Numeric {
    type Err = LoxError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains('.') {
            if s.ends_with('.') {
                return Err(LoxError::Generic(
                    "unterminated numeric literal".to_string(),
                ));
            }
            Ok(Self::Decimal(s.parse()?))
        } else {
            Ok(Self::Integer(s.parse()?))
        }
    }
}
