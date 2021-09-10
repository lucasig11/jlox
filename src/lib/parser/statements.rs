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
    /// Variable declaration statement (names, initializers)
    ///
    /// Holds a vector because there can be more than one variable being declared at a time.
    /// ```text
    /// let a, b, c = 1, 2, 3;
    /// let a, b, c;
    /// let a, b, c = 1;
    /// ```
    Variable(Vec<Token>, Vec<Option<Expr>>),
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
            Stmt::Variable(names, initializers) => {
                let variables: Vec<_> = names
                    .iter()
                    .zip(initializers)
                    .map(|(name, initializer)| {
                        let value = match initializer {
                            Some(initializer) => initializer.evaluate(Rc::clone(&env), locals)?,
                            None => Rc::new(LoxValue::Nil),
                        };
                        Ok((name.to_string(), value))
                    })
                    .collect::<LoxResult<_>>()?;

                for (name, value) in variables {
                    env.define(&name, value);
                }
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
                let function = Rc::new(LoxFunction::new(self.to_owned(), Rc::clone(&env), false)?);
                env.define(&name.to_string(), Rc::new(LoxValue::Callable(function)));
            }
            Stmt::Return(kw, val) => {
                return Err(ReturnVal::new(
                    (*val.evaluate(env, locals)?).to_owned(),
                    Span::new(kw.span().start(), val.position().end()),
                )
                .into());
            }
            Stmt::Class(name, superclass, methods, static_methods) => {
                let pos = name.span();
                let name = name.to_string();

                let superclass = if let Some(superclass) = &superclass {
                    let superclass = superclass.evaluate(Rc::clone(&env), locals)?;
                    if let LoxValue::Callable(f) = &*superclass {
                        if f.as_any().downcast_ref::<LoxClass>().is_none() {
                            return Err(InnerError::new(*pos, "superclass must be a class").into());
                        }
                    }
                    Some(superclass)
                } else {
                    None
                };
                env.define(&name, Rc::new(LoxValue::Nil));

                let mut env = if let Some(ref superclass) = superclass {
                    let env = Environment::from(Rc::clone(&env));
                    env.define("super", Rc::clone(superclass));
                    Rc::new(env)
                } else {
                    env
                };

                let to_map = |v: &[Stmt], can_be_init: bool| {
                    (*v).iter()
                        .map(|el| {
                            let declaration = el.to_owned();
                            let func_name = declaration.name();
                            let is_initializer = func_name.eq("init");
                            if !can_be_init && is_initializer {
                                return Err(
                                    InnerError::new(*pos, "constructor cannot be static").into()
                                );
                            }
                            Ok((
                                func_name,
                                LoxFunction::new(declaration, Rc::clone(&env), is_initializer)?,
                            ))
                        })
                        .collect::<LoxResult<_>>()
                };

                let static_methods = to_map(static_methods, false)?;
                let methods = to_map(methods, true)?;

                if superclass.is_some() {
                    env = Rc::clone(env.enclosing().as_ref().unwrap());
                }

                let class = LoxClass::new(&name, superclass, methods, static_methods);

                env.assign(&name, &LoxValue::Callable(Rc::new(class)))?;
            }
        };
        Ok(())
    }

    pub fn name(&self) -> String {
        match self {
            Stmt::Function(name, ..) => name.to_string(),
            Stmt::Class(name, ..) => name.to_string(),
            _ => self.to_string(),
        }
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
                Stmt::Expression(..) => "expression",
                Stmt::Print(..) => "print",
                Stmt::Return(..) => "return",
                Stmt::If(..) => "if",
                Stmt::Function(..) => "function",
                Stmt::Class(..) => "class",
                Stmt::Variable(..) => "variable",
                Stmt::While(..) => "while",
                Stmt::Block(..) => "block",
            }
        )
    }
}
