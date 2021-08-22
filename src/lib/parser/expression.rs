use crate::lib::position::Span;
use crate::lib::token::Token;

#[allow(dead_code)]
#[derive(Debug)]
/// Language expressions
pub(crate) enum Expr {
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

use crate::error::*;
use crate::lib::interpreter::LoxValue;
use crate::lib::token::{Punctuator, TokenKind};
use std::convert::TryInto;

impl Expr {
    pub fn evaluate(self) -> LoxResult<LoxValue> {
        let pos = &self.position();
        match self {
            Expr::Literal(tk) => tk
                .kind()
                .try_into()
                .map_err(|e: &str| LoxError::Generic(e.to_string())),
            Expr::Grouping(expr) => (*expr).evaluate(),
            Expr::Unary(op, rhs) => {
                let rhs = (*rhs).evaluate()?;

                use Punctuator::*;

                match *op.kind() {
                    TokenKind::Punctuator(Sub) => Ok(-rhs),
                    TokenKind::Punctuator(Not) => Ok(LoxValue::Boolean(!rhs.is_truthy())),
                    _ => unreachable!(),
                }
            }

            Expr::Binary(lhs, op, rhs) => {
                let lhs = lhs.evaluate()?;
                let rhs = rhs.evaluate()?;
                use Punctuator::*;
                let result = match *op.kind() {
                    TokenKind::Punctuator(Sub) => lhs - rhs,
                    TokenKind::Punctuator(Mul) => lhs * rhs,
                    TokenKind::Punctuator(Div) => lhs / rhs,
                    TokenKind::Punctuator(Add) => lhs + rhs,
                    _ => unreachable!(),
                };
                result.map_err(|e: LoxError| InnerError::new(*pos, &e.to_string()).into())
            }
            _ => todo!(),
        }
    }

    pub fn position(&self) -> Span {
        match &self {
            Expr::Literal(tk) => *tk.span(),
            Expr::Grouping(expr) => expr.position(),
            Expr::Unary(op, expr) => Span::new(op.span().start(), expr.position().end()),
            Expr::Binary(lhs, _, rhs) => Span::new(lhs.position().start(), rhs.position().end()),
            _ => todo!(),
        }
    }
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