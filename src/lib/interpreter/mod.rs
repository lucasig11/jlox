pub(crate) use self::values::LoxValue;
use crate::error::LoxError;

use crate::lib::parser::Stmt;
pub(crate) use environment::Environment;

mod environment;
mod values;

/// Executes the statements generated in the parsing stage.
/// This lifetime corresponds to the scope in [Lox::run](crate::lib::Lox::run)
pub(crate) struct Interpreter {
    env: Environment,
    statements: Vec<Stmt>,
}

impl Interpreter {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self {
            statements,
            env: Environment::new(),
        }
    }

    /// Executes a list of statements.
    pub fn interpret(&self) -> Result<(), Vec<LoxError>> {
        let mut errors = Vec::new();
        for stmt in &self.statements {
            if let Err(e) = stmt.execute(&self.env) {
                errors.push(e);
            };
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }
}
