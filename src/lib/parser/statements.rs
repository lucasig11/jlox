use std::io::Write;
use std::rc::Rc;

use crate::error::*;
use crate::lib::interpreter::{Environment, LoxFunction, LoxValue};
use crate::lib::position::Span;
use crate::lib::token::{Keyword, Token};

use super::Expr;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub(crate) enum Stmt {
    /// Expression statement(expression)
    Expression(Expr),
    /// Print statement(expression)
    Print(Expr),
    /// Return expression(keyword, value)
    Return(Token, Expr),
    /// If statement(condition, then, else)
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// Function statement(name, params, body)
    Function(Token, Vec<Token>, Box<Stmt>),
    /// Class statement(name, superclass: Expr::Variable, methods: Vec<Stmt::Function>)
    Class(Token, Expr, Vec<Stmt>),
    /// Variable statement(name, initializer)
    Variable(Token, Expr),
    /// While statement(condition, body)
    While(Expr, Box<Stmt>),
    /// Block statement(statements)
    Block(Vec<Stmt>),
}

impl Stmt {
    pub fn execute(&self, env: Rc<Environment>, writer: &mut dyn Write) -> LoxResult<()> {
        match &self {
            Stmt::Expression(expr) => {
                expr.evaluate(env)?;
            }
            Stmt::Print(expr) => {
                writer.write_all(format!("{}\n", expr.evaluate(env)?).as_bytes())?;
            }
            Stmt::Variable(name, initializer) => {
                let value = match initializer {
                    Expr::Literal(t) if *t.kind() == Keyword::Nil.into() => LoxValue::Nil,
                    _ => initializer.evaluate(env.clone())?,
                };

                env.define(&name.to_string(), value);
            }
            Stmt::Block(stmts) => {
                let scope = Rc::new(Environment::from(env));
                for stmt in stmts {
                    stmt.execute(scope.clone(), writer)?;
                }
            }
            Stmt::If(condition, then_branch, else_branch) => {
                let condition = condition.evaluate(env.clone())?;
                if condition.is_truthy() {
                    then_branch.execute(env, writer)?;
                } else if let Some(stmt) = else_branch {
                    stmt.execute(env, writer)?;
                }
            }
            Stmt::While(condition, body) => {
                while condition.evaluate(env.clone())?.is_truthy() {
                    body.execute(env.clone(), writer)?;
                }
            }
            Stmt::Function(name, _, _) => {
                let function = LoxFunction::new(self.to_owned(), env.clone())?;
                env.define(
                    &name.to_string(),
                    LoxValue::Callable(std::rc::Rc::new(function)),
                );
            }
            Stmt::Return(kw, val) => {
                return Err(ReturnVal::new(
                    val.evaluate(env)?,
                    Span::new(kw.span().start(), val.position().end()),
                )
                .into());
            }
            Stmt::Class(_, _, _) => todo!(),
        };
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ReturnVal {
    pub val: LoxValue,
    pub pos: Span,
}

impl ReturnVal {
    fn new(val: LoxValue, pos: Span) -> Self {
        Self { val, pos }
    }
}

// FIXME: refactor this out of here
impl From<ReturnVal> for LoxError {
    fn from(ret: ReturnVal) -> Self {
        Self::Return(ret)
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Stmt::Expression(_) => "expression",
                Stmt::Print(_) => "print",
                Stmt::Return(_, _) => "return",
                Stmt::If(_, _, _) => "if",
                Stmt::Function(_, _, _) => "function",
                Stmt::Class(_, _, _) => "class",
                Stmt::Variable(_, _) => "variable",
                Stmt::While(_, _) => "while",
                Stmt::Block(_) => "block",
            }
        )
    }
}
