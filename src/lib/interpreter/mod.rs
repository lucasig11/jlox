pub(crate) use self::values::LoxValue;
use crate::error::LoxError;

mod values;

pub(crate) struct Interpreter;
use crate::lib::parser::Stmt;
impl Interpreter {
    pub fn interpret(statements: Vec<Stmt>) -> Result<(), Vec<LoxError>> {
        let mut errors = Vec::new();
        for stmt in statements {
            if let Err(e) = stmt.execute() {
                errors.push(e);
            };
        }
        if errors.len() > 0 {
            return Err(errors);
        }
        Ok(())
    }
}
