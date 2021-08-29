use crate::lib::{
    interpreter::{values::LoxCallable, Environment, LoxError, LoxValue},
    parser::Stmt,
    LoxResult,
};

pub(crate) struct LoxFunction {
    declaration: Stmt,
    arity: usize,
}

impl LoxFunction {
    pub fn new(declaration: Stmt) -> LoxResult<Self> {
        if let Stmt::Function(_, ref params, _) = declaration {
            let arity = params.len();
            Ok(Self { declaration, arity })
        } else {
            Err(LoxError::Generic(format!(
                "cannot create lox function from {} statement",
                declaration
            )))
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, env: &Environment<'_>, args: &[LoxValue]) -> LoxResult<LoxValue> {
        let env = Environment::from(env);
        if let Stmt::Function(_name, params, body) = &self.declaration {
            for (ident, val) in params.iter().zip(args) {
                env.define(&ident.to_string(), val.to_owned())
            }
            if let Err(err) = body.execute(&env, &mut std::io::stdout()) {
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
