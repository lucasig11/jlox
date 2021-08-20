use self::expression::Expr;
use crate::error::{LoxResult, ParseError};

use super::token::{Keyword, Punctuator, Token, TokenKind};

pub(crate) mod expression {

    use crate::lib::token::Token;

    #[allow(dead_code)]
    /// Language expressions
    pub enum Expr {
        /// Binary expression (Expr, Operator, Expr)
        Binary(Box<Expr>, Token, Box<Expr>),
        /// Unary operator (op: Token, rhs: Expr)
        Unary(Token, Box<Expr>),
        /// Assign expression (name: Token, value: Expression)
        Assign(Token, Box<Expr>),
        /// Call expression (callee: Expr, Token: paren, args: Vec<Expr>)
        Call(Box<Expr>, Token, Vec<Expr>),
        /// Class `get` expression (object: Expr, name: Token)
        Get(Box<Expr>, Token),
        /// Class set expression (object: Expr, name: Token, value: Expr)
        Set(Box<Expr>, Token, Box<Expr>),
        /// Represents the parentheses groups
        Grouping(Box<Expr>),
        /// Literal values
        Literal(Token),
        /// Logical expression (lhs: Expr, op: Token, rhs: Expr)
        Logical(Box<Expr>, Token, Box<Expr>),
        /// Super expression (keyword: Token, method: Token)
        Super(Token, Token),
        /// Class `this` expression
        This(Token),
        /// Variable expression (name: Token)
        Variable(Token),
    }

    impl std::fmt::Display for Expr {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self {
                Expr::Binary(lhs, tk, rhs) => write!(f, "({} {} {})", tk, *lhs, *rhs),
                Expr::Unary(tk, rhs) => write!(f, "({} {})", tk, *rhs),
                Expr::Assign(tk, expr) => write!(f, "({} {})", tk, *expr),
                Expr::Grouping(expr) => write!(f, "(group {})", *expr),
                Expr::Literal(tk) => write!(f, "{}", tk),
                _ => unimplemented!(),
            }
        }
    }
}

use Keyword::*;

/// Converts a list of tokens into an _AST_.
/// ```text
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
/// ```
pub(crate) struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(mut self) -> LoxResult<Expr> {
        self.expression()
    }
    /// Helper function for recovering from errors.
    /// It walks the token buffer until it finds a statement boundary.
    #[allow(dead_code)]
    fn synchronize(&mut self) {
        self.next();
        while let Some(e) = self.peek() {
            if let Some(t) = self.previous() {
                if t.kind() == &TokenKind::Punctuator(Punctuator::Semicolon) {
                    break;
                }
            }

            if let TokenKind::Keyword(Class | Fn | Let | For | If | While | Print | Return) =
                e.kind()
            {
                break;
            }

            self.next();
        }
    }

    /// Compares a list of tokens to the next token in the buffer,
    /// if the comparison returns true, walks the list and return true.
    fn multi_check<T: Into<TokenKind> + Clone>(&mut self, tks: &[T]) -> bool {
        for t in tks {
            let t: TokenKind = t.to_owned().into();
            if self.next_if_check(&t) {
                return true;
            }
        }
        false
    }

    #[inline]
    fn next_if_check(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.next();
            return true;
        }
        false
    }

    #[inline]
    fn check(&self, kind: &TokenKind) -> bool {
        match self.peek() {
            Some(e) if e.kind() == kind => true,
            _ => false,
        }
    }

    fn next(&mut self) -> Option<&Token> {
        if self.current < self.tokens.len() - 1 {
            self.current += 1;
        }
        self.previous()
    }

    #[inline]
    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    #[inline]
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    #[inline]
    fn expression(&mut self) -> LoxResult<Expr> {
        self.equality()
    }

    /// Parse left associative tokens
    fn parse_left<T, F>(&mut self, token_kinds: &[T], mut op_func: F) -> LoxResult<Expr>
    where
        T: Into<TokenKind> + Clone,
        F: FnMut(&mut Self) -> LoxResult<Expr>,
    {
        let mut expr = op_func(self)?;

        while self.multi_check(token_kinds) {
            let op = match self.previous() {
                Some(op) => op.clone(),
                None => break,
            };
            let rhs = op_func(self)?;
            expr = Expr::Binary(expr.into(), op, rhs.into());
        }
        Ok(expr)
    }

    /// Parse (in)equality expressions
    #[inline]
    fn equality(&mut self) -> LoxResult<Expr> {
        self.parse_left(&[Punctuator::Eq, Punctuator::NotEq], Self::comparison)
    }

    /// Parse comparison expressions
    #[inline]
    fn comparison(&mut self) -> LoxResult<Expr> {
        self.parse_left(
            &[
                Punctuator::GreaterThan,
                Punctuator::GreaterThanOrEq,
                Punctuator::LessThan,
                Punctuator::LessThanOrEq,
            ],
            Self::term,
        )
    }

    /// Addition and subtraction
    #[inline]
    fn term(&mut self) -> LoxResult<Expr> {
        self.parse_left(&[Punctuator::Add, Punctuator::Sub], Self::factor)
    }

    /// Division and multiplication
    #[inline]
    fn factor(&mut self) -> LoxResult<Expr> {
        self.parse_left(&[Punctuator::Div, Punctuator::Mul], Self::unary)
    }

    /// Logic/Arithmetic negation
    fn unary(&mut self) -> LoxResult<Expr> {
        if self.multi_check(&[Punctuator::Not, Punctuator::Sub]) {
            let op = self.previous().unwrap().to_owned();
            let rhs = self.unary()?;
            return Ok(Expr::Unary(op, rhs.into()));
        }
        self.primary()
    }

    fn primary(&mut self) -> LoxResult<Expr> {
        if let Some(tk) = self.peek() {
            let exp = match tk.kind() {
                TokenKind::BooleanLiteral(_) => Ok(Expr::Literal(tk.to_owned())),
                TokenKind::StringLiteral(_) => Ok(Expr::Literal(tk.to_owned())),
                TokenKind::NumericLiteral(_) => Ok(Expr::Literal(tk.to_owned())),
                TokenKind::Keyword(Keyword::Nil) => Ok(Expr::Literal(tk.to_owned())),
                TokenKind::Punctuator(Punctuator::OpenParen) => {
                    self.next();
                    let expr = self.expression()?;
                    self.consume(Punctuator::CloseParen, "expected ')' after expression")?;
                    Ok(Expr::Grouping(expr.into()))
                }
                _ => Err(ParseError::new(tk.to_owned(), "expected expression").into()),
            };

            if exp.is_ok() {
                self.next();
            }
            return exp;
        }
        unimplemented!("Attempt to parse primary expression with no tokens left in the buffer.")
    }

    fn consume<T: Into<TokenKind>>(&mut self, kind: T, msg: &str) -> LoxResult<()> {
        let kind: TokenKind = kind.into();
        if self.next_if_check(&kind) {
            dbg!(self.peek());
            return Ok(());
        }
        Err(ParseError::new(self.previous().unwrap().to_owned(), msg).into())
    }
}
