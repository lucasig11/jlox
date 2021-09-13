use crate::lib::{
    error::LoxError,
    parser::{Expr, Stmt},
    LoxResult,
};

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub(crate) use self::{class::LoxClass, function::LoxFunction, values::LoxValue};
pub(crate) use environment::Environment;
pub(crate) use resolver::Resolver;

#[macro_use]
pub(crate) mod util;
pub(crate) mod class;
mod environment;
mod function;
mod resolver;
pub(crate) mod values;

mod builtins {
    use super::{
        values::{LoxCallable, LoxValue},
        Environment,
    };
    use crate::lib::{error::LoxResult, parser::Expr};
    use derive_new::*;
    use std::{collections::HashMap, rc::Rc, time::UNIX_EPOCH};

    #[derive(new)]
    pub struct Clock;

    #[derive(new)]
    pub struct Input;

    /// Gets the system time as a unix timestamp and return it as a [`LoxValue::Decimal`].
    impl LoxCallable for Clock {
        fn call(
            &self,
            _: Rc<Environment>,
            _: &HashMap<Expr, usize>,
            _: &[Rc<LoxValue>],
        ) -> LoxResult<Rc<LoxValue>> {
            Ok(Rc::new(LoxValue::Decimal(
                std::time::SystemTime::now()
                    .duration_since(UNIX_EPOCH)?
                    .as_millis() as f64,
            )))
        }
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }
    }

    /// Reads a line from stdin, returning it as a [`LoxValue::String`]
    impl LoxCallable for Input {
        fn call(
            &self,
            _: Rc<Environment>,
            _: &HashMap<Expr, usize>,
            _: &[Rc<LoxValue>],
        ) -> LoxResult<Rc<LoxValue>> {
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf)?;
            Ok(Rc::new(LoxValue::String(buf)))
        }
        fn as_any(&self) -> &dyn std::any::Any {
            self
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
        let globals = Rc::new(Environment::new());

        Self::define_globals(Rc::clone(&globals));

        Self {
            statements,
            globals,
            locals: Default::default(),
        }
    }

    pub fn with_env(statements: &[Stmt], env: Rc<Environment>) -> Self {
        let statements = Vec::from(statements);

        Self::define_globals(Rc::clone(&env));

        Self {
            statements,
            globals: env,
            locals: Default::default(),
        }
    }

    fn define_globals(env: Rc<Environment>) {
        let clock = Rc::new(builtins::Clock::new());
        let input = Rc::new(builtins::Input::new());

        // Define native functions
        env.define("clock", Rc::new(LoxValue::Callable(clock)));
        env.define("input", Rc::new(LoxValue::Callable(input)));
    }

    pub fn resolve(&self, expr: &Expr, depth: usize) -> LoxResult<()> {
        self.locals.borrow_mut().insert(expr.to_owned(), depth);
        Ok(())
    }

    /// Executes a list of statements.
    pub fn interpret(&self) -> Result<(), Vec<LoxError>> {
        let mut errors = Vec::new();

        for stmt in &self.statements {
            if let Err(e) = stmt.execute(
                Rc::clone(&self.globals),
                &*self.locals.borrow(),
                &mut std::io::stdout(),
            ) {
                errors.push(e);
            };
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }
}
