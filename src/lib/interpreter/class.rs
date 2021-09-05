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

    pub fn find_method(&self, name: &str) -> Option<LoxValue> {
        self.methods
            .get(name)
            .map(|f| LoxValue::Callable(Rc::new(f.clone())))
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
        fields
            .get(&name.to_string())
            .cloned()
            .or_else(|| self.class.find_method(&name.to_string()))
            .ok_or_else(|| {
                InnerError::new(*name.span(), &format!("undefined property `{}`", name)).into()
            })
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
