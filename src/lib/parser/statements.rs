use std::{collections::HashMap, io::Write, rc::Rc};

use crate::lib::{
    error::*,
    interpreter::{Environment, LoxClass, LoxFunction, LoxValue},
    position::Span,
    token::Token,
};

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
    Class(Token, Option<Expr>, Vec<Stmt>, Vec<Stmt>),
    /// Variable statement(name, initializer)
    Variable(Token, Expr),
    /// While statement(condition, body)
    While(Expr, Box<Stmt>),
    /// Block statement(statements)
    Block(Vec<Stmt>),
}

impl Stmt {
    pub fn execute(
        &self,
        env: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
        writer: &mut dyn Write,
    ) -> LoxResult<()> {
        match &self {
            Stmt::Expression(expr) => {
                expr.evaluate(env, locals)?;
            }
            Stmt::Print(expr) => {
                writer.write_all(format!("{}\n", expr.evaluate(env, locals)?).as_bytes())?;
            }
            Stmt::Variable(name, initializer) => {
                let value = if initializer.is_nil_expr() {
                    Rc::new(LoxValue::Nil)
                } else {
                    initializer.evaluate(Rc::clone(&env), locals)?
                };

                env.define(&name.to_string(), (*value).to_owned());
            }
            Stmt::Block(stmts) => {
                let scope = Rc::new(Environment::from(env));
                for stmt in stmts {
                    stmt.execute(Rc::clone(&scope), locals, writer)?;
                }
            }
            Stmt::If(condition, then_branch, else_branch) => {
                let condition = condition.evaluate(Rc::clone(&env), locals)?;
                if condition.is_truthy() {
                    then_branch.execute(env, locals, writer)?;
                } else if let Some(stmt) = else_branch {
                    stmt.execute(env, locals, writer)?;
                }
            }
            Stmt::While(condition, body) => {
                while condition.evaluate(Rc::clone(&env), locals)?.is_truthy() {
                    body.execute(Rc::clone(&env), locals, writer)?;
                }
            }
            Stmt::Function(name, _, _) => {
                let function = LoxFunction::new(self.to_owned(), Rc::clone(&env), false)?;
                env.define(
                    &name.to_string(),
                    LoxValue::Callable(std::rc::Rc::new(function)),
                );
            }
            Stmt::Return(kw, val) => {
                return Err(ReturnVal::new(
                    (*val.evaluate(env, locals)?).to_owned(),
                    Span::new(kw.span().start(), val.position().end()),
                )
                .into());
            }
            Stmt::Class(name, _, methods, static_methods) => {
                let pos = name.span();
                let name = name.to_string();
                env.define(&name, LoxValue::Nil);
                let to_map = |v: &Vec<Stmt>, can_be_init: bool| {
                    (*v).iter()
                        .map(|el| {
                            let is_initializer = el.to_string().eq("init");
                            if !can_be_init && is_initializer {
                                return Err(
                                    InnerError::new(*pos, "constructor cannot be static").into()
                                );
                            }
                            let func = LoxFunction::new(
                                el.to_owned(),
                                Rc::clone(&env),
                                el.to_string().eq("init"),
                            )?;
                            Ok((el.to_string(), func))
                        })
                        .collect::<LoxResult<HashMap<_, _>>>()
                };

                let methods = to_map(methods, true)?;
                let static_methods = to_map(static_methods, false)?;

                let class = LoxClass::new(&name, methods, static_methods);
                env.assign(&name, &LoxValue::Callable(Rc::new(class)))?;
            }
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
                Stmt::Expression(..) => "expression".to_string(),
                Stmt::Print(..) => "print".to_string(),
                Stmt::Return(..) => "return".to_string(),
                Stmt::If(..) => "if".to_string(),
                Stmt::Function(name, _, _) => name.to_string(),
                Stmt::Class(..) => "class".to_string(),
                Stmt::Variable(name, _) => name.to_string(),
                Stmt::While(..) => "while".to_string(),
                Stmt::Block(..) => "block".to_string(),
            }
        )
    }
}
