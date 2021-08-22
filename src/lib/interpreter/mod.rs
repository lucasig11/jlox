pub(crate) use self::values::LoxValue;
use crate::error::LoxResult;

use super::parser::expression::Expr;

mod values;

pub(crate) struct Interpreter;

impl Interpreter {
    pub fn interpret(expression: Expr) -> LoxResult<()> {
        let val = expression.evaluate()?;
        println!("{}", val);
        Ok(())
    }
}
