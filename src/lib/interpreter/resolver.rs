use std::cell::RefCell;
use std::collections::HashMap;

use crate::{
    error::InnerError,
    lib::{
        interpreter::Interpreter,
        parser::{Expr, Stmt},
        token::{Keyword, Token},
        LoxResult,
    },
};

pub(crate) trait Resolvable {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()>;
}

impl Resolvable for Stmt {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()> {
        match &self {
            Stmt::Expression(_) => todo!(),
            Stmt::Print(_) => todo!(),
            Stmt::Return(_, _) => todo!(),
            Stmt::If(_, _, _) => todo!(),
            Stmt::Function(_, _, _) => todo!(),
            Stmt::Class(_, _, _) => todo!(),
            Stmt::Variable(name, initializer) => {
                resolver.declare(name);
                if matches!(&initializer, Expr::Literal(t) if *t.kind() == Keyword::Nil.into()) {
                    resolver.resolve(initializer)?;
                }
                resolver.define(name);
            }
            Stmt::Block(statements) => {
                resolver.begin_scope();
                statements.resolve(resolver)?;
                resolver.end_scope();
            }
            Stmt::While(_, _) => todo!(),
        }

        Ok(())
    }
}

impl Resolvable for Expr {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()> {
        match &self {
            Expr::Variable(ref name) => {
                resolver.check(name)?;
                resolver.resolve_local(self, name)?;
            }
            Expr::Assign(ref name, ref value) => {
                value.resolve(resolver)?;
                resolver.resolve_local(self, name)?;
            }
            Expr::Binary(_, _, _) => todo!(),
            Expr::Unary(_, _) => todo!(),
            Expr::Call(_, _, _) => todo!(),
            Expr::Get(_, _) => todo!(),
            Expr::Set(_, _, _) => todo!(),
            Expr::Grouping(_) => todo!(),
            Expr::Literal(_) => todo!(),
            Expr::Logical(_, _, _) => todo!(),
            Expr::Super(_, _) => todo!(),
            Expr::This(_) => todo!(),
        }
        Ok(())
    }
}

impl Resolvable for Vec<Stmt> {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()> {
        for stmt in self {
            stmt.resolve(resolver)?;
        }
        Ok(())
    }
}

type Scope = HashMap<String, bool>;

pub(crate) struct Resolver<'i> {
    interpreter: &'i Interpreter,
    scopes: RefCell<Vec<Scope>>,
}

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i Interpreter) -> Self {
        Self {
            interpreter,
            scopes: Default::default(),
        }
    }

    pub fn resolve(&self, resolvable: &dyn Resolvable) -> LoxResult<()> {
        resolvable.resolve(self)
    }

    pub fn resolve_local(&self, expr: &Expr, name: &Token) -> LoxResult<()> {
        let scopes = self.scopes.borrow();
        for (idx, scope) in scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.to_string()) {
                self.interpreter.resolve(expr, scopes.len() - idx)?;
                return Ok(());
            }
        }
        Ok(())
    }

    pub fn check(&self, token: &Token) -> LoxResult<()> {
        let scopes = self.scopes.borrow();
        if scopes.is_empty() {
            return Ok(());
        }

        if let Some(&false) = scopes.last().unwrap().get(&token.to_string()) {
            return Err(InnerError::new(
                *token.span(),
                "Cannot read local variable in its own initializer",
            )
            .into());
        }
        Ok(())
    }

    pub fn begin_scope(&self) {
        let scope = Scope::new();
        self.scopes.borrow_mut().push(scope);
    }

    pub fn end_scope(&self) {
        self.scopes.borrow_mut().pop();
    }

    pub fn define(&self, name: &Token) {
        self.put(name.to_string(), true);
    }

    pub fn declare(&self, name: &Token) {
        self.put(name.to_string(), false);
    }

    fn put(&self, name: String, val: bool) {
        if self.scopes.borrow().is_empty() {
            return;
        }

        self.scopes
            .borrow_mut()
            .last_mut()
            .unwrap()
            .insert(name, val);
    }
}
