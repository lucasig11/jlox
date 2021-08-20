use self::expression::Expr;
use crate::error::{LoxError, LoxResult};

use super::token::{Keyword, Punctuator, Token, TokenKind};

pub(crate) mod expression {

    use crate::lib::token::Token;

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

pub(crate) struct Parser {
    tokens: Vec<Token>,
    current: usize,
}
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    /// Compares a list of tokens to the next token in the buffer,
    /// if the comparison returns true, walks the list and return true.
    fn next_if_eq<T: Into<TokenKind> + Clone>(&mut self, tks: &[T]) -> bool {
        for t in tks {
            if self.check((*t).clone().into()) {
                self.next();
                return true;
            }
        }
        false
    }

    fn check(&self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(e) if e.kind() == &kind => true,
            _ => false,
        }
    }

    fn next(&mut self) -> Option<&Token> {
        if self.current < self.tokens.len() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    /// Parse left associative tokens
    fn parse_left<T, F>(&mut self, token_kinds: &[T], mut op_func: F) -> Expr
    where
        T: Into<TokenKind> + Clone,
        F: FnMut(&mut Self) -> Expr,
    {
        let mut expr = op_func(self);

        while self.next_if_eq(token_kinds) {
            let op = match self.previous() {
                Some(op) => op.clone(),
                None => break,
            };
            let rhs = op_func(self);
            expr = Expr::Binary(expr.into(), op, rhs.into());
        }
        expr
    }

    /// Parse (in)equality expressions
    fn equality(&mut self) -> Expr {
        self.parse_left(&[Punctuator::Eq, Punctuator::NotEq], Self::comparison)
    }

    /// Parse comparison expressions
    fn comparison(&mut self) -> Expr {
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
    fn term(&mut self) -> Expr {
        self.parse_left(&[Punctuator::Add, Punctuator::Sub], Self::factor)
    }

    /// Division and multiplication
    fn factor(&mut self) -> Expr {
        self.parse_left(&[Punctuator::Div, Punctuator::Mul], Self::unary)
    }

    /// Logic/Arithmetic negation
    fn unary(&mut self) -> LoxResult<Expr> {
        if self.next_if_eq(&[Punctuator::Not, Punctuator::Sub]) {
            let op = &self.previous().unwrap();
            let rhs = self.unary();
            return Ok(Expr::Unary(*op, rhs.into()));
        }
        self.primary()
    }

    fn primary(&mut self) -> LoxResult<Expr> {
        if let Some(tk) = self.peek() {
            match tk.kind() {
                TokenKind::BooleanLiteral(_) => Ok(Expr::Literal(*tk)),
                TokenKind::StringLiteral(_) => Ok(Expr::Literal(*tk)),
                TokenKind::NumericLiteral(_) => Ok(Expr::Literal(*tk)),
                TokenKind::Keyword(Keyword::Nil) => Ok(Expr::Literal(*tk)),
                TokenKind::Punctuator(Punctuator::OpenParen) => {
                    let expr = self.expression();
                    if self.check(Punctuator::CloseParen.into()) {
                        self.next();
                        Ok(Expr::Grouping(expr.into()))
                    } else {
                        Err(LoxError::Parse("Expected ')' after expression".to_string()))
                    }
                }
                _ => todo!(),
            }
        } else {
            todo!()
        }
    }
}
