use std::io::Write;

use crate::error::*;
use crate::lib::interpreter::{Environment, LoxValue};
use crate::lib::token::{Keyword, Token};

use super::Expr;

#[allow(dead_code)]
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
    Function(Token, Vec<Token>, Vec<Stmt>),
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
    pub fn execute(&self, env: &Environment, writer: &mut dyn Write) -> LoxResult<()> {
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
                    _ => initializer.evaluate(env)?,
                };

                env.define(&name.to_string(), value);
            }
            Stmt::Block(stmts) => {
                let scope = Environment::from(env);
                for stmt in stmts {
                    stmt.execute(&scope, writer)?;
                }
            }
            Stmt::If(condition, then_branch, else_branch) => {
                let condition = condition.evaluate(env)?;
                if condition.is_truthy() {
                    then_branch.execute(env, writer)?;
                } else if let Some(stmt) = else_branch {
                    stmt.execute(env, writer)?;
                }
            }
            Stmt::Return(_, _) => todo!(),
            Stmt::Function(_, _, _) => todo!(),
            Stmt::Class(_, _, _) => todo!(),
            Stmt::While(_, _) => todo!(),
        };
        Ok(())
    }
}
