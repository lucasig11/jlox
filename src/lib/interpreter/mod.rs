use crate::{
    error::LoxError,
    lib::{
        parser::{Expr, Stmt},
        LoxResult,
    },
};

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub(crate) use self::{function::LoxFunction, values::LoxValue};
pub(crate) use environment::Environment;
pub(crate) use resolver::Resolver;

#[macro_use]
pub(crate) mod util;
mod environment;
mod function;
mod resolver;
pub(crate) mod values;

mod builtins {
    use super::{
        values::{LoxCallable, LoxValue},
        Environment,
    };
    use crate::error::LoxResult;
    use std::{rc::Rc, time::UNIX_EPOCH};

    #[derive(new)]
    pub struct Clock;

    #[derive(new)]
    pub struct Read;

    /// Gets the system time as a unix timestamp and return it as a [`LoxValue::Decimal`].
    impl LoxCallable for Clock {
        fn call(&self, _: Rc<Environment>, _: &[LoxValue]) -> LoxResult<LoxValue> {
            Ok(LoxValue::Decimal(
                std::time::SystemTime::now()
                    .duration_since(UNIX_EPOCH)?
                    .as_millis() as f64,
            ))
        }
    }

    /// Reads a line from stdin, returning it as a [`LoxValue::String`]
    impl LoxCallable for Read {
        fn call(&self, _: Rc<Environment>, _: &[LoxValue]) -> LoxResult<LoxValue> {
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf)?;
            Ok(LoxValue::String(buf))
        }
    }
}

/// Executes the statements generated in the parsing stage.
/// This lifetime corresponds to the scope in [Lox::run](crate::lib::Lox::run)
pub(crate) struct Interpreter {
    globals: Rc<Environment>,
    statements: Vec<Stmt>,
    locals: RefCell<HashMap<Expr, usize>>,
}

impl Interpreter {
    pub fn new(statements: &[Stmt]) -> Self {
        let statements = Vec::from(statements);
        let globals = Environment::new();

        // Define native functions
        globals.define("clock", LoxValue::Callable(Rc::new(builtins::Clock::new())));
        globals.define("read", LoxValue::Callable(Rc::new(builtins::Read::new())));

        Self {
            statements,
            globals: Rc::new(globals),
            locals: Default::default(),
        }
    }

    pub fn resolve(&self, expr: &Expr, depth: usize) -> LoxResult<()> {
        self.locals.borrow_mut().insert(expr.to_owned(), depth);
        Ok(())
    }

    /// Executes a list of statements.
    pub fn interpret(&self) -> Result<(), Vec<LoxError>> {
        let mut errors = Vec::new();
        let env = Rc::new(Environment::from(self.globals.clone()));
        for stmt in &self.statements {
            if let Err(e) = stmt.execute(env.clone(), &mut std::io::stdout()) {
                errors.push(e);
            };
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }
}
