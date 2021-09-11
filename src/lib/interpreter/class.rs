use crate::lib::{
    error::{InnerError, LoxError, LoxResult},
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
    superclass: Option<Rc<LoxValue>>,
    static_methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(
        name: &str,
        superclass: Option<Rc<LoxValue>>,
        methods: HashMap<String, LoxFunction>,
        static_methods: HashMap<String, LoxFunction>,
    ) -> Self {
        Self {
            methods,
            superclass,
            static_methods,
            name: name.to_string(),
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&LoxFunction> {
        self.methods.get(name).or_else(|| match self.superclass {
            Some(ref superclass) => superclass.as_class().ok().and_then(|c| c.find_method(name)),
            _ => None,
        })
    }

    pub fn find_static(&self, name: &str) -> LoxResult<Rc<LoxValue>> {
        match self.static_methods.get(name) {
            Some(f) => return Ok(Rc::new(LoxValue::Callable(Rc::new(f.to_owned())))),
            None => {
                if let Some(ref superclass) = self.superclass {
                    return superclass.as_class()?.find_static(name);
                }
            }
        }
        Err(LoxError::Generic(format!(
            "static method `{}` not found for `{}` class",
            name, self.name
        )))
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        env: Rc<Environment>,
        locals: &HashMap<Expr, usize>,
        args: &[Rc<LoxValue>],
    ) -> LoxResult<Rc<LoxValue>> {
        let instance = LoxInstance::new(self.clone());
        if let Some(constructor) = self.find_method("init") {
            return constructor
                .bind(&instance)?
                .call(Rc::clone(&env), locals, args);
        }

        Ok(Rc::new(LoxValue::Instance(instance)))
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Clone, Debug)]
pub(crate) struct LoxInstance {
    class: LoxClass,
    fields: RefCell<HashMap<String, Rc<LoxValue>>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: Default::default(),
        }
    }

    pub fn get(&self, name: &Token) -> LoxResult<Rc<LoxValue>> {
        let fields = self.fields.borrow();
        if let Some(prop) = fields.get(&name.to_string()).cloned() {
            return Ok(prop);
        }

        if let Some(method) = self.class.find_method(&name.to_string()) {
            return Ok(Rc::new(LoxValue::Callable(Rc::new(method.bind(self)?))));
        }

        Err(InnerError::new(*name.span(), &format!("undefined property `{}`", name)).into())
    }

    pub fn set(&self, name: &Token, val: &Rc<LoxValue>) -> LoxResult<()> {
        self.fields
            .borrow_mut()
            .insert(name.to_string(), Rc::clone(val));
        Ok(())
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} instance {:#?}",
            LoxCallable::to_string(&self.class),
            &*self.fields.borrow()
        )
    }
}
