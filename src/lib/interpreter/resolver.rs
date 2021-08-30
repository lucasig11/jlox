use std::cell::RefCell;
use std::collections::HashMap;

use crate::{
    error::InnerError,
    lib::{
        interpreter::Interpreter,
        parser::{Expr, Stmt},
        token::Token,
        LoxResult,
    },
};

pub(crate) trait Resolvable {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()>;
}

impl Resolvable for Stmt {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()> {
        match &self {
            Stmt::Variable(name, initializer) => {
                resolver.declare(name);
                if !initializer.is_nil_expr() {
                    resolver.resolve(initializer)?;
                }
                resolver.define(name);
            }
            Stmt::Block(statements) => {
                resolver.begin_scope();
                resolver.resolve(statements)?;
                resolver.end_scope();
            }
            Stmt::Function(ref name, _, _) => {
                resolver.declare(name);
                resolver.define(name);
                resolver.resolve_func(self)?;
            }
            Stmt::Expression(expr) => {
                resolver.resolve(expr)?;
            }
            Stmt::If(condition, then_branch, else_branch) => {
                resolver.resolve(condition)?;
                resolver.resolve(&**then_branch)?;
                if let Some(stmt) = else_branch {
                    resolver.resolve(&**stmt)?;
                }
            }
            Stmt::Print(expr) => {
                resolver.resolve(expr)?;
            }
            Stmt::Return(_, val) => {
                if !val.is_nil_expr() {
                    resolver.resolve(val)?;
                }
            }
            Stmt::While(condition, body) => {
                resolver.resolve(condition)?;
                resolver.resolve(&**body)?;
            }
            Stmt::Class(_, _, _) => todo!(),
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
            Expr::Binary(lhs, _, rhs) => {
                resolver.resolve(&**lhs)?;
                resolver.resolve(&**rhs)?;
            }
            Expr::Unary(_, rhs) => resolver.resolve(&**rhs)?,
            Expr::Call(callee, _, args) => {
                resolver.resolve(&**callee)?;
                resolver.resolve(args)?;
            }
            Expr::Grouping(expr) => resolver.resolve(&**expr)?,
            Expr::Literal(_) => (),
            Expr::Logical(lhs, _, rhs) => {
                resolver.resolve(&**lhs)?;
                resolver.resolve(&**rhs)?;
            }

            Expr::Get(_, _) => todo!(),
            Expr::Set(_, _, _) => todo!(),
            Expr::Super(_, _) => todo!(),
            Expr::This(_) => todo!(),
        }
        Ok(())
    }
}

impl<T: Resolvable> Resolvable for Vec<T> {
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

    fn resolve_local(&self, expr: &Expr, name: &Token) -> LoxResult<()> {
        let scopes = self.scopes.borrow();
        for (idx, scope) in scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.to_string()) {
                self.interpreter.resolve(expr, scopes.len() - idx)?;
                return Ok(());
            }
        }
        Ok(())
    }

    fn resolve_func(&self, stmt: &Stmt) -> LoxResult<()> {
        self.begin_scope();
        if let Stmt::Function(_, params, body) = stmt {
            for param in params {
                self.declare(param);
                self.define(param);
            }
            self.resolve(&**body)?;
        }
        self.end_scope();
        Ok(())
    }

    fn check(&self, token: &Token) -> LoxResult<()> {
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

    fn begin_scope(&self) {
        let scope = Scope::new();
        self.scopes.borrow_mut().push(scope);
    }

    fn end_scope(&self) {
        self.scopes.borrow_mut().pop();
    }

    fn define(&self, name: &Token) {
        self.put(name.to_string(), true);
    }

    fn declare(&self, name: &Token) {
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
