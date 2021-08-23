pub(crate) use self::values::LoxValue;
use crate::error::LoxResult;

mod values;

pub(crate) struct Interpreter;
use crate::lib::parser::Stmt;
impl Interpreter {
    pub fn interpret(statements: Vec<Stmt>) -> LoxResult<()> {
        for stmt in statements {
            stmt.execute()?;
        }
        Ok(())
    }
}
