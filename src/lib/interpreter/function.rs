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
    is_initializer: bool,
}

impl LoxFunction {
    pub fn new(
        declaration: Stmt,
        closure: Rc<Environment>,
        is_initializer: bool,
    ) -> LoxResult<Self> {
        if let Stmt::Function(_, ref params, _) = declaration {
            let arity = params.len();
            Ok(Self {
                declaration,
                arity,
                closure,
                is_initializer,
            })
        } else {
            Err(LoxError::Generic(format!(
                "cannot create lox function from {} statement",
                declaration
            )))
        }
    }

    pub fn bind(&self, instance: &LoxInstance) -> LoxResult<LoxFunction> {
        let env = Environment::from(Rc::clone(&self.closure));
        env.define("this", Rc::new(LoxValue::Instance(instance.to_owned())));
        LoxFunction::new(self.declaration.clone(), env.into(), self.is_initializer)
    }

    pub fn is_initializer(&self) -> bool {
        self.is_initializer
    }
}

impl LoxCallable for LoxFunction {
    fn call(
        &self,
        _: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
        args: &[Rc<LoxValue>],
    ) -> LoxResult<Rc<LoxValue>> {
        let env = Rc::new(Environment::from(Rc::clone(&self.closure)));
        if let Stmt::Function(_name, params, body) = &self.declaration {
            for (ident, val) in params.iter().zip(args) {
                env.define(&ident.to_string(), Rc::clone(val))
            }
            if let Err(err) = body.execute(Rc::clone(&env), locals, &mut std::io::stdout()) {
                // Capture the return value that is unwinding the call stack
                if let LoxError::Return(r) = err {
                    return Ok(Rc::new(r.val));
                }
                return Err(err);
            }
        }

        if self.is_initializer() {
            return self.closure.get_at(0, "this");
        }

        Ok(Rc::new(LoxValue::Nil))
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
