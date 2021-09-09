use std::cell::RefCell;
use std::collections::HashMap;

use crate::lib::{
    error::InnerError,
    interpreter::Interpreter,
    parser::{Expr, Stmt},
    token::Token,
    LoxResult,
};

#[derive(Clone, Copy)]
enum FunctionType {
    Method,
    Function,
}

#[derive(Clone, Copy)]
enum ClassType {
    Class,
    SubClass,
}

pub(crate) trait Resolvable {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()>;
}

impl Resolvable for Stmt {
    fn resolve(&self, resolver: &Resolver) -> LoxResult<()> {
        match &self {
            Stmt::Variable(name, initializer) => {
                for (name, initializer) in name.iter().zip(initializer) {
                    resolver.declare(name);
                    if let Some(initializer) = initializer {
                        resolver.resolve(initializer)?;
                    }
                    resolver.define(name);
                }
            }
            Stmt::Block(statements) => {
                resolver.begin_scope();
                resolver.resolve(statements)?;
                resolver.end_scope();
            }
            Stmt::Function(ref name, _, _) => {
                resolver.declare(name);
                resolver.define(name);
                resolver.resolve_func(self, FunctionType::Function)?;
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
            Stmt::Return(tk, val) => {
                if resolver.current_function.borrow().is_none() {
                    return Err(
                        InnerError::new(*tk.span(), "cannot return from top-level code").into(),
                    );
                }
                if !val.is_nil_expr() {
                    resolver.resolve(val)?;
                }
            }
            Stmt::While(condition, body) => {
                resolver.resolve(condition)?;
                resolver.resolve(&**body)?;
            }
            Stmt::Class(name, superclass, methods, static_methods) => {
                let enclosing_class = *resolver.current_class.borrow();
                *resolver.current_class.borrow_mut() = Some(ClassType::Class);

                resolver.declare(name);
                resolver.define(name);

                if let Some(superclass) = superclass {
                    *resolver.current_class.borrow_mut() = Some(ClassType::SubClass);
                    if name.to_string().eq(&superclass.to_string()) {
                        return Err(InnerError::new(
                            superclass.position(),
                            "a class cannot inherit itself",
                        )
                        .into());
                    }
                    resolver.resolve(superclass)?;
                    resolver.begin_scope();
                    resolver.put(String::from("super"), true);
                }

                for static_method in &*static_methods {
                    resolver.resolve_func(static_method, FunctionType::Method)?;
                }
                resolver.begin_scope();
                resolver.put(String::from("this"), true);
                for method in &*methods {
                    let declaration = FunctionType::Method;
                    resolver.resolve_func(method, declaration)?;
                }
                resolver.end_scope();
                if superclass.is_some() {
                    resolver.end_scope();
                }
                *resolver.current_class.borrow_mut() = enclosing_class;
            }
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
                resolver.resolve(&**value)?;
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
            Expr::Array(_, vals) => resolver.resolve(vals)?,
            Expr::Grouping(expr) => resolver.resolve(&**expr)?,
            Expr::Literal(_) => (),
            Expr::Logical(lhs, _, rhs) => {
                resolver.resolve(&**lhs)?;
                resolver.resolve(&**rhs)?;
            }

            Expr::Get(object, _) => {
                resolver.resolve(&**object)?;
            }
            Expr::Set(object, _, value) => {
                resolver.resolve(&**value)?;
                resolver.resolve(&**object)?;
            }
            Expr::This(ref keyword) => {
                if resolver.current_class.borrow().is_none() {
                    return Err(InnerError::new(
                        *keyword.span(),
                        "cannot use `this` outside of a class",
                    )
                    .into());
                }
                resolver.resolve_local(self, keyword)?
            }
            Expr::Super(ref keyword, _) => match *resolver.current_class.borrow() {
                Some(ClassType::SubClass) => resolver.resolve_local(self, keyword)?,
                None => {
                    return Err(InnerError::new(
                        *keyword.span(),
                        "cannot use `super` outside of a class",
                    )
                    .into())
                }
                Some(ClassType::Class) => {
                    return Err(InnerError::new(
                        *keyword.span(),
                        "cannot use `super` in a class with no superclass",
                    )
                    .into())
                }
            },
            Expr::ArrayIndex(_, idx) => resolver.resolve(&**idx)?,
            Expr::ArrayAssign(_, idx, val) => {
                resolver.resolve(&**idx)?;
                resolver.resolve(&**val)?;
            }
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

#[derive(Clone)]
pub(crate) struct Resolver<'i> {
    interpreter: &'i Interpreter,
    scopes: RefCell<Vec<Scope>>,
    current_function: RefCell<Option<FunctionType>>,
    current_class: RefCell<Option<ClassType>>,
}

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i Interpreter) -> Self {
        Self {
            interpreter,
            scopes: Default::default(),
            current_function: Default::default(),
            current_class: Default::default(),
        }
    }

    pub fn resolve(&self, resolvable: &dyn Resolvable) -> LoxResult<()> {
        resolvable.resolve(self)
    }

    fn resolve_local(&self, expr: &Expr, name: &Token) -> LoxResult<()> {
        let scopes = self.scopes.borrow();
        for (idx, scope) in scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.to_string()) {
                self.interpreter.resolve(expr, scopes.len() - idx - 1)?;
                return Ok(());
            }
        }
        Ok(())
    }

    fn resolve_func(&self, stmt: &Stmt, func_type: FunctionType) -> LoxResult<()> {
        let enclosing_function = *self.current_function.borrow();
        *self.current_function.borrow_mut() = Some(func_type);

        self.begin_scope();
        if let Stmt::Function(_, params, body) = stmt {
            for param in params {
                self.declare(param);
                self.define(param);
            }
            self.resolve(&**body)?;
        }
        self.end_scope();
        *self.current_function.borrow_mut() = enclosing_function;
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

    pub fn put(&self, name: String, val: bool) {
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
