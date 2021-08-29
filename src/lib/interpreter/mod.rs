use crate::error::LoxError;
use crate::lib::parser::Stmt;

use std::rc::Rc;

pub(crate) use self::{function::LoxFunction, values::LoxValue};
pub(crate) use environment::Environment;

#[macro_use]
pub(crate) mod util;
mod environment;
mod function;
pub(crate) mod values;

mod builtins {
    use std::rc::Rc;
    use std::time::UNIX_EPOCH;

    use super::values::{LoxCallable, LoxValue};
    use super::Environment;
    use crate::error::LoxResult;

    pub struct Clock;

    impl Clock {
        pub fn new() -> Self {
            Self {}
        }
    }

    impl LoxCallable for Clock {
        fn call(&self, _: Rc<Environment>, _: &[LoxValue]) -> LoxResult<LoxValue> {
            Ok(LoxValue::Decimal(
                std::time::SystemTime::now()
                    .duration_since(UNIX_EPOCH)?
                    .as_millis() as f64,
            ))
        }

        fn arity(&self) -> usize {
            0
        }

        fn to_string(&self) -> String {
            "<native fn>".into()
        }
    }
}

/// Executes the statements generated in the parsing stage.
/// This lifetime corresponds to the scope in [Lox::run](crate::lib::Lox::run)
pub(crate) struct Interpreter {
    globals: Rc<Environment>,
    statements: Vec<Stmt>,
}

impl Interpreter {
    pub fn new(statements: Vec<Stmt>) -> Self {
        let globals = Environment::new();

        // Define native functions
        globals.define("clock", LoxValue::Callable(Rc::new(builtins::Clock::new())));

        Self {
            statements,
            globals: Rc::new(globals),
        }
    }

    /// Executes a list of statements.
    pub fn interpret(&self) -> Result<(), Vec<LoxError>> {
        let mut errors = Vec::new();
        for stmt in &self.statements {
            if let Err(e) = stmt.execute(self.globals.clone(), &mut std::io::stdout()) {
                errors.push(e);
            };
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }
}
