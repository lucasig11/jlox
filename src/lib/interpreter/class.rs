use crate::lib::{
    error::LoxResult,
    interpreter::{values::LoxCallable, Environment, LoxValue},
    parser::Expr,
};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    rc::Rc,
};

#[derive(Clone)]
pub(crate) struct LoxClass {
    name: String,
}

impl LoxClass {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        _: Rc<Environment>,
        _: &HashMap<Expr, usize>,
        _: &[LoxValue],
    ) -> LoxResult<LoxValue> {
        let instance = LoxInstance::new(self.clone());

        Ok(LoxValue::Instance(instance))
    }

    fn to_string(&self) -> String {
        self.name.clone()
    }
}

#[derive(Clone)]
pub(crate) struct LoxInstance {
    class: LoxClass,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self { class }
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} instance", LoxCallable::to_string(&self.class))
    }
}
