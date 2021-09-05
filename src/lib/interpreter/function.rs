use crate::lib::{
    interpreter::{class::LoxInstance, values::LoxCallable, Environment, LoxError, LoxValue},
    parser::{Expr, Stmt},
    LoxResult,
};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub(crate) struct LoxFunction {
    declaration: Stmt,
    arity: usize,
    closure: Rc<Environment>,
}

impl LoxFunction {
    pub fn new(declaration: Stmt, closure: Rc<Environment>) -> LoxResult<Self> {
        if let Stmt::Function(_, ref params, _) = declaration {
            let arity = params.len();
            Ok(Self {
                declaration,
                arity,
                closure,
            })
        } else {
            Err(LoxError::Generic(format!(
                "cannot create lox function from {} statement",
                declaration
            )))
        }
    }

    pub fn bind(&self, instance: &LoxInstance) -> LoxResult<LoxValue> {
        let env = Environment::from(Rc::clone(&self.closure));
        env.define("this", LoxValue::Instance(instance.to_owned()));
        let func = LoxFunction::new(self.declaration.clone(), env.into())?;
        Ok(LoxValue::Callable(Rc::new(func)))
    }
}

impl LoxCallable for LoxFunction {
    fn call(
        &self,
        _: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
        args: &[LoxValue],
    ) -> LoxResult<LoxValue> {
        let env = Environment::from(self.closure.clone());
        if let Stmt::Function(_name, params, body) = &self.declaration {
            for (ident, val) in params.iter().zip(args) {
                env.define(&ident.to_string(), val.to_owned())
            }
            if let Err(err) = body.execute(Rc::new(env), locals, &mut std::io::stdout()) {
                if let LoxError::Return(r) = err {
                    return Ok(r.val);
                }
                return Err(err);
            }
        }

        Ok(LoxValue::Nil)
    }

    fn arity(&self) -> usize {
        self.arity
    }

    fn to_string(&self) -> String {
        if let Stmt::Function(name, _, _) = &self.declaration {
            return format!("<fn {}>", name.to_string());
        }
        unreachable!()
    }
}
