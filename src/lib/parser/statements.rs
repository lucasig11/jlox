use crate::error::*;
use crate::lib::token::Token;

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
    If(Expr, Box<Stmt>, Box<Stmt>),
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
    pub fn execute(&self) -> LoxResult<()> {
        match &self {
            Stmt::Expression(expr) => {
                expr.evaluate()?;
            }
            Stmt::Print(expr) => println!("{}", expr.evaluate()?),
            Stmt::Return(_, _) => todo!(),
            Stmt::If(_, _, _) => todo!(),
            Stmt::Function(_, _, _) => todo!(),
            Stmt::Class(_, _, _) => todo!(),
            Stmt::Variable(_, _) => todo!(),
            Stmt::While(_, _) => todo!(),
            Stmt::Block(_) => todo!(),
        };
        Ok(())
    }
}
