use crate::lib::{
    error::{InnerError, LoxResult},
    interpreter::{values::LoxCallable, Environment, LoxValue},
    parser::Expr,
    token::Token,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, Formatter},
    rc::Rc,
};

use super::LoxFunction;

#[derive(Clone, Debug)]
pub(crate) struct LoxClass {
    name: String,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(name: &str, methods: HashMap<String, LoxFunction>) -> Self {
        Self {
            methods,
            name: name.to_string(),
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&LoxFunction> {
        self.methods.get(name)
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        env: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
        args: &[LoxValue],
    ) -> LoxResult<LoxValue> {
        let instance = LoxInstance::new(self.clone());
        if let Some(constructor) = self.find_method("init") {
            constructor
                .bind(&instance)?
                .call(Rc::clone(&env), locals, args)?;
        }

        Ok(LoxValue::Instance(instance))
    }

    fn to_string(&self) -> String {
        self.name.clone()
    }

    fn arity(&self) -> usize {
        if let Some(constructor) = self.find_method("init") {
            return constructor.arity();
        }
        0
    }
}

#[derive(Clone, Debug)]
pub(crate) struct LoxInstance {
    class: LoxClass,
    fields: RefCell<HashMap<String, LoxValue>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: Default::default(),
        }
    }

    pub fn get(&self, name: &Token) -> LoxResult<LoxValue> {
        let fields = self.fields.borrow();
        if let Some(prop) = fields.get(&name.to_string()).cloned() {
            return Ok(prop);
        }

        if let Some(method) = self.class.find_method(&name.to_string()) {
            return Ok(LoxValue::Callable(Rc::new(method.bind(self)?)));
        }

        Err(InnerError::new(*name.span(), &format!("undefined property `{}`", name)).into())
    }

    pub fn set(&self, name: &Token, val: &LoxValue) -> LoxResult<()> {
        self.fields
            .borrow_mut()
            .insert(name.to_string(), val.to_owned());
        Ok(())
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} instance {{", LoxCallable::to_string(&self.class))?;
        for (key, val) in &*self.fields.borrow() {
            write!(f, "\n\t{} = {},", key, val)?;
        }
        write!(f, "}}")
    }
}
