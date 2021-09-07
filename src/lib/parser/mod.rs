//! Parses a list of tokens into an Abstract Syntax Tree.
//!
//! **Context free grammar**
//! ```text
//! program        → declaration* EOF ;
//!
//! declaration    → classDecl
//!                | funDecl
//!                | varDecl
//!                | statement ;
//! classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
//!                  "{" function* "}" ;
//! funDecl        → "fun" function ;
//! varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
//!
//! statement      → exprStmt
//!                | forStmt
//!                | ifStmt
//!                | printStmt
//!                | returnStmt
//!                | whileStmt
//!                | block ;
//!
//! exprStmt       → expression ";" ;
//! forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
//!                            expression? ";"
//!                            expression? ")" statement ;
//! ifStmt         → "if" "(" expression ")" statement
//!                  ( "else" statement )? ;
//! printStmt      → "print" expression ";" ;
//! returnStmt     → "return" expression? ";" ;
//! whileStmt      → "while" "(" expression ")" statement ;
//! block          → "{" declaration* "}" ;
//!
//! expression     → assignment ;
//!
//! assignment     → ( call "." )? IDENTIFIER "=" assignment
//!                | logic_or ;
//!
//! logic_or       → logic_and ( "or" logic_and )* ;
//! logic_and      → equality ( "and" equality )* ;
//! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//! term           → factor ( ( "-" | "+" ) factor )* ;
//! factor         → unary ( ( "/" | "*" ) unary )* ;
//!
//! unary          → ( "!" | "-" ) unary | call ;
//! call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
//! primary        → "true" | "false" | "nil" | "this"
//!                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
//!                | "super" "." IDENTIFIER ;
//!
//! Utility rules:
//!
//! function       → IDENTIFIER "(" parameters? ")" block ;
//! parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
//! arguments      → expression ( "," expression )* ;
//!
//! ```
//! **Lexical grammar**
//! ```text
//! NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
//! STRING         → "\"" <any char except "\"">* "\"" ;
//! IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
//! ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
//! DIGIT          → "0" ... "9" ;
//! ```
//!
pub(crate) mod expression;
pub(crate) mod statements;
use super::{
    error::{InnerError, LoxError, LoxResult},
    token::{Keyword, Punctuator, Token, TokenKind},
};
pub(crate) use expression::Expr;
pub(crate) use statements::Stmt;
use Keyword::*;

pub(crate) struct Parser<'a> {
    inner: InnerIter<'a, Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            inner: InnerIter::new(tokens),
        }
    }

    pub fn parse(self) -> Result<Vec<Stmt>, Vec<LoxError>> {
        let mut statements = Vec::new();
        let mut errors: Vec<LoxError> = Vec::new();
        while self.inner.peek().is_some() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();
                }
            };
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(statements)
    }

    fn declaration(&self) -> LoxResult<Stmt> {
        if self.matches(Keyword::Class) {
            return self.class_decl();
        }
        if self.matches(Keyword::Fn) {
            return self.func_decl("function");
        }
        if self.matches(Keyword::Let) {
            return self.var_decl();
        }
        self.statement()
    }

    /// Parses a class declaration.
    fn class_decl(&self) -> LoxResult<Stmt> {
        let name = self.consume_ident("expected name after `class`")?;

        if self.matches(Keyword::Extends) {
            let _superclass = self.consume_ident("expected superclass name after `extends`")?;
        }

        self.consume(
            Punctuator::OpenBlock,
            "expected block before class declaration",
        )?;

        let mut methods = Vec::new();
        let mut static_methods = Vec::new();
        while !self.check(Punctuator::CloseBlock) && self.inner.peek().is_some() {
            if self.matches(Keyword::Static) {
                static_methods.push(self.func_decl("method")?);
            } else {
                methods.push(self.func_decl("method")?);
            }
        }

        self.consume(
            Punctuator::CloseBlock,
            "expected block after class declaration",
        )?;

        Ok(Stmt::Class(name.clone(), None, methods, static_methods))
    }

    /// Parses a function declaration.
    ///
    /// The `kind` here does not correspond to the TokenKind, but to wether we're declaring a
    /// function or a class method.
    fn func_decl(&self, kind: &str) -> LoxResult<Stmt> {
        let name = self.consume_ident(&format!("Expected {} name", &kind))?;
        self.consume(
            Punctuator::OpenParen,
            &format!("expected `(` after {}", &kind),
        )?;

        let mut params = Vec::new();

        if !self.check(Punctuator::CloseParen) {
            loop {
                if params.len() >= 255 {
                    return Err(InnerError::new(
                        *name.span(),
                        "exceeded parameter limit (max: 255)",
                    )
                    .into());
                }

                params.push(self.consume_ident("expected parameter name")?.to_owned());

                if !self.matches(Punctuator::Comma) {
                    break;
                }
            }
        }
        self.consume(Punctuator::CloseParen, "expected `)` after parameters")?;
        self.consume(
            Punctuator::OpenBlock,
            &format!("expected `{{` before {} body", &kind),
        )?;

        let body = self.block_stmt()?;
        Ok(Stmt::Function(name.to_owned(), params, body.into()))
    }

    fn var_decl(&self) -> LoxResult<Stmt> {
        let name = self.consume_ident("expected identifier")?;
        let initializer = if self.matches(Punctuator::Assign) {
            self.expression()?
        } else {
            // Unwrapping here is safe bc we definitely have a `previous` token in the buffer, the
            // variable identifier.
            Expr::Literal(Token::new(
                Keyword::Nil,
                *self.inner.previous().unwrap().span(),
            ))
        };

        self.consume(
            Punctuator::Semicolon,
            "expected `;` after variable declaration",
        )?;
        Ok(Stmt::Variable(name.to_owned(), initializer))
    }

    fn statement(&self) -> LoxResult<Stmt> {
        if self.matches(Keyword::For) {
            return self.for_stmt();
        }
        if self.matches(Keyword::If) {
            return self.if_stmt();
        }

        if self.matches(Keyword::Print) {
            return self.print_stmt();
        }

        if self.matches(Keyword::Return) {
            return self.return_stmt();
        }

        if self.matches(Keyword::While) {
            return self.while_stmt();
        }

        if self.matches(Punctuator::OpenBlock) {
            return self.block_stmt();
        }

        self.expression_stmt()
    }

    fn return_stmt(&self) -> LoxResult<Stmt> {
        let kw = self.inner.previous().unwrap().to_owned();

        let val = if !self.check(Punctuator::Semicolon) {
            self.expression()?
        } else {
            Expr::Literal(Token::new(
                TokenKind::Keyword(Keyword::Nil),
                kw.span().end().into(),
            ))
        };

        self.consume(Punctuator::Semicolon, "expected `;` after return statement")?;

        Ok(Stmt::Return(kw, val))
    }

    fn for_stmt(&self) -> LoxResult<Stmt> {
        self.consume(Punctuator::OpenParen, "expected `(` after `for` keyword")?;

        let initializer = if self.matches(Punctuator::Semicolon) {
            None
        } else if self.matches(Keyword::Let) {
            Some(self.var_decl()?)
        } else {
            Some(self.expression_stmt()?)
        };

        let condition = if !self.check(Punctuator::Semicolon) {
            self.expression()?
        } else {
            Expr::Literal(Token::new(
                TokenKind::BooleanLiteral(true),
                *self.inner.previous().unwrap().span(),
            ))
        };

        self.consume(Punctuator::Semicolon, "expected `;` after condition")?;

        let increment = if !self.check(Punctuator::CloseParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Punctuator::CloseParen, "expected `)` after for clauses")?;

        let mut body = self.statement()?;

        // Append the increment expression, if there's one, at the end of the loop
        if let Some(expr) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(expr)]);
        }

        body = Stmt::While(condition, body.into());

        // Place the initialization before the loop body
        if let Some(stmt) = initializer {
            body = Stmt::Block(vec![stmt, body]);
        }

        Ok(body)
    }

    fn while_stmt(&self) -> LoxResult<Stmt> {
        let condition = self.expression()?;
        self.consume(Punctuator::OpenBlock, "expected `{` after condition")?;

        let body = self.block_stmt()?;

        Ok(Stmt::While(condition, body.into()))
    }

    fn if_stmt(&self) -> LoxResult<Stmt> {
        let condition = self.expression()?;
        self.consume(Punctuator::OpenBlock, "expected `{` after condition")?;

        let then_branch = self.block_stmt()?;

        let else_branch = if self.matches(Keyword::Else) {
            self.consume(Punctuator::OpenBlock, "expected `{` after else keyword")?;
            let else_branch = self.block_stmt()?;
            Some(else_branch.into())
        } else {
            None
        };

        Ok(Stmt::If(condition, then_branch.into(), else_branch))
    }

    fn block_stmt(&self) -> LoxResult<Stmt> {
        let mut statements = Vec::new();
        while !self.check(Punctuator::CloseBlock) && self.inner.peek().is_some() {
            statements.push(self.declaration()?);
        }
        self.consume(Punctuator::CloseBlock, "expected `}` after block")?;
        Ok(Stmt::Block(statements))
    }

    fn print_stmt(&self) -> LoxResult<Stmt> {
        let value = self.expression()?;
        self.consume(Punctuator::Semicolon, "expected `;` after value")?;
        Ok(Stmt::Print(value))
    }

    fn expression_stmt(&self) -> LoxResult<Stmt> {
        let expr = self.expression()?;
        self.consume(Punctuator::Semicolon, "expected `;` after value")?;
        Ok(Stmt::Expression(expr))
    }

    /// Helper function for recovering from errors.
    /// It walks the token buffer until it finds a statement boundary.
    fn synchronize(&self) {
        while let Some(e) = self.inner.peek() {
            if let Some(t) = self.inner.previous() {
                if t.kind() == &TokenKind::Punctuator(Punctuator::Semicolon) {
                    self.inner.advance();
                    break;
                }
            }

            if let TokenKind::Keyword(Class | Fn | Let | For | If | While | Print | Return) =
                e.kind()
            {
                break;
            }

            self.inner.advance();
        }
    }

    /// Parses an expression.
    #[inline]
    fn expression(&self) -> LoxResult<Expr> {
        self.assignment()
    }

    fn assignment(&self) -> LoxResult<Expr> {
        let expr = self.or()?;

        if self.matches(Punctuator::Assign) {
            let val = self.assignment()?;
            return match expr {
                Expr::Variable(name) => Ok(Expr::Assign(name, val.into())),
                Expr::Get(object, name) => Ok(Expr::Set(object, name, val.into())),
                _ => Err(InnerError::new(expr.position(), "invalid assigment target").into()),
            };
        }
        Ok(expr)
    }

    /// Parses a logical OR
    fn or(&self) -> LoxResult<Expr> {
        let mut expr = self.and()?;
        while self.matches(Keyword::Or) {
            let op = self.inner.previous().unwrap().to_owned();
            let rhs = self.and()?;
            expr = Expr::Logical(expr.into(), op, rhs.into());
        }
        Ok(expr)
    }

    fn and(&self) -> LoxResult<Expr> {
        let mut expr = self.equality()?;
        while self.matches(Keyword::And) {
            let op = self.inner.previous().unwrap().to_owned();
            let rhs = self.equality()?;
            expr = Expr::Logical(expr.into(), op, rhs.into());
        }
        Ok(expr)
    }

    /// Parses (in)equality expressions
    #[inline]
    fn equality(&self) -> LoxResult<Expr> {
        self.parse_left(&[Punctuator::Eq, Punctuator::NotEq], Self::comparison)
    }

    /// Parses comparison expressions
    #[inline]
    fn comparison(&self) -> LoxResult<Expr> {
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

    /// Parses addition/subtraction expressions
    #[inline]
    fn term(&self) -> LoxResult<Expr> {
        self.parse_left(&[Punctuator::Add, Punctuator::Sub], Self::factor)
    }

    /// Parses division/multiplication expressions
    #[inline]
    fn factor(&self) -> LoxResult<Expr> {
        self.parse_left(&[Punctuator::Div, Punctuator::Mul], Self::unary)
    }

    /// Parses logic/arithmetic negation expressions
    fn unary(&self) -> LoxResult<Expr> {
        if self.multi_check(&[Punctuator::Not, Punctuator::Sub]) {
            // Unwrapping here is safe bc if we're in this block, we were at the `previous` token
            let op = self.inner.previous().unwrap().to_owned();
            let rhs = self.unary()?;
            return Ok(Expr::Unary(op, rhs.into()));
        }
        self.call()
    }

    /// Parses a function call (max. arity: 255).
    fn call(&self) -> LoxResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(Punctuator::OpenParen) {
                expr = self.finish_call(expr)?;
            } else if self.matches(Punctuator::Dot) {
                let name = self.consume_ident("expected property name after `.`")?;
                expr = Expr::Get(Box::new(expr), name.to_owned());
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&self, callee: Expr) -> LoxResult<Expr> {
        let mut args = Vec::new();

        if !self.check(Punctuator::CloseParen) {
            loop {
                if args.len().ge(&255) {
                    return Err(InnerError::new(
                        *self.inner.peek().unwrap().span(),
                        "cannot have more than 255 arguments",
                    )
                    .into());
                }

                args.push(self.expression()?);

                if !self.matches(Punctuator::Comma) {
                    break;
                }
            }
        }

        let paren = self
            .consume(Punctuator::CloseParen, "Expected `)` after arguments")?
            .to_owned();

        Ok(Expr::Call(callee.into(), paren, args))
    }

    /// Parses primary expressions (literals, groups)
    fn primary(&self) -> LoxResult<Expr> {
        if let Some(tk) = self.inner.peek() {
            let exp = match tk.kind() {
                TokenKind::BooleanLiteral(_) => Expr::Literal(tk.to_owned()),
                TokenKind::StringLiteral(_) => Expr::Literal(tk.to_owned()),
                TokenKind::NumericLiteral(_) => Expr::Literal(tk.to_owned()),
                TokenKind::Keyword(Keyword::Nil) => Expr::Literal(tk.to_owned()),
                TokenKind::Keyword(Keyword::This) => Expr::This(tk.to_owned()),
                TokenKind::Identifier(_) => Expr::Variable(tk.to_owned()),
                TokenKind::Punctuator(Punctuator::OpenParen) => {
                    self.inner.advance();
                    let expr = self.expression()?;
                    self.consume(Punctuator::CloseParen, "expected `)` after expression")?;
                    return Ok(Expr::Grouping(expr.into()));
                }
                TokenKind::Keyword(Keyword::Static) => {
                    return Err(InnerError::new(
                        *tk.to_owned().span(),
                        "cannot use static keyword outside of a class",
                    )
                    .into())
                }
                _ => return Err(InnerError::new(*tk.to_owned().span(), "unexpected token").into()),
            };
            self.inner.advance();
            return Ok(exp);
        }
        Err(InnerError::new(
            *self.inner.previous().unwrap().span(),
            "expected expression",
        )
        .into())
    }

    /// Checks to see if the current token has any of the give types.
    /// If so, it consumes the token and return true. [Parsing Expressions - 6.2.1](https://craftinginterpreters.com/parsing-expressions.html)
    fn multi_check<T: Into<TokenKind> + Clone>(&self, tks: &[T]) -> bool {
        for t in tks {
            let t: TokenKind = t.to_owned().into();
            if self.matches(t) {
                return true;
            }
        }
        false
    }

    fn matches<T: Into<TokenKind>>(&self, kind: T) -> bool {
        self.inner.next_if(self.check(kind))
    }

    /// Peeks the current token and returns true if its kind is equal to `kind`
    #[inline]
    fn check<T: Into<TokenKind>>(&self, kind: T) -> bool {
        matches!(self.inner.peek(), Some(e) if e.kind() == &kind.into())
    }

    /// Consumes the next token if its kind is `T`, otherwise return a [LoxError](super::error::LoxError::Inner) with `msg`
    fn consume<T: Into<TokenKind>>(&self, kind: T, msg: &str) -> LoxResult<&Token> {
        if self.check(kind) {
            // Unwrapping here is safe bc `self.check` returned true, so we know there's a next
            return Ok(self.inner.advance().unwrap());
        }
        Err(InnerError::new(*self.inner.previous().unwrap().span(), msg).into())
    }

    /// Consumes an identifier, or returns an Error.
    ///
    /// An identifier needs this special function because it
    /// carries a Box<str>, which can't be casted into a [TokenKind](super::token::TokenKind)
    fn consume_ident(&self, msg: &str) -> LoxResult<&Token> {
        if let Some(tk) = self.inner.peek() {
            if let TokenKind::Identifier(_) = *tk.kind() {
                // Unwrapping is safe here bc `inner.peek()` returned something, so we know there's
                // a next
                return Ok(self.inner.advance().unwrap());
            }
        };
        return Err(InnerError::new(*self.inner.previous().unwrap().span(), msg).into());
    }

    /// Parses left associative tokens.
    fn parse_left<T, F>(&self, token_kinds: &[T], op_func: F) -> LoxResult<Expr>
    where
        T: Into<TokenKind> + Clone,
        F: std::ops::Fn(&Self) -> LoxResult<Expr>,
    {
        let mut expr = op_func(self)?;

        while self.multi_check(token_kinds) {
            let op = match self.inner.previous() {
                Some(op) => op.clone(),
                None => break,
            };
            let rhs = op_func(self)?;
            expr = Expr::Binary(expr.into(), op, rhs.into());
        }

        Ok(expr)
    }
}

use std::cell::RefCell;

/// Inner iterator for the parser.
struct InnerIter<'a, T> {
    collection: &'a [T],
    current: std::cell::RefCell<usize>,
}

impl<'a, T> InnerIter<'a, T> {
    #[inline]
    fn new(collection: &'a [T]) -> Self {
        Self {
            collection,
            current: RefCell::new(0),
        }
    }

    #[inline]
    fn advance(&self) -> Option<&T> {
        if *self.current.borrow() <= self.collection.len() {
            *self.current.borrow_mut() += 1;
        }
        self.previous()
    }

    #[inline]
    fn previous(&self) -> Option<&T> {
        self.collection
            .get((*self.current.borrow()).checked_sub(1)?)
    }

    #[inline]
    fn peek(&self) -> Option<&T> {
        self.collection.get(*self.current.borrow())
    }

    #[inline]
    fn next_if(&self, test: bool) -> bool {
        if test {
            self.advance();
            return true;
        }
        false
    }
}

#[cfg(test)]
mod test {
    use crate::lib::lexer::Lexer;

    use super::*;

    #[test]
    fn parses_nested_expressions() {
        let src = "(1+(3-2)+4);";
        let tokens = Lexer::new(src).scan_tokens().unwrap();
        let expr = Parser::new(&tokens).parse().unwrap();
        let expected_expression = "(group (+ (+ 1 (group (- 3 2))) 4))";
        assert!(
            matches!(&expr[0], Stmt::Expression(expr) if expr.to_string() == expected_expression)
        )
    }

    #[test]
    fn parses_var_declaration() {
        let src = "let foo = true;";
        let tokens = Lexer::new(src).scan_tokens().unwrap();
        let expr = Parser::new(&tokens).parse().unwrap();

        assert!(
            matches!(&expr[0], Stmt::Variable(tk, expr) if &tk.to_string() == "foo" && &expr.to_string() == "true")
        );
    }
}
