use std::collections::HashMap;

use std::cell::RefCell;

use crate::error::{LoxError, LoxResult};

use super::LoxValue;

#[derive(Debug)]
pub(crate) struct Environment {
    values: RefCell<HashMap<Box<str>, LoxValue>>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    /// Creates a new global environment
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: Default::default(),
        }
    }

    /// Creates a local environment from one in lower precedence (higher scope)
    pub fn from(oth: Self) -> Self {
        Self {
            enclosing: Some(Box::new(oth)),
            values: Default::default(),
        }
    }

    pub fn define(&self, name: &str, val: LoxValue) {
        self.values.borrow_mut().insert(name.into(), val);
    }

    pub fn assign(&self, name: &str, val: &LoxValue) -> LoxResult<LoxValue> {
        match self.values.borrow_mut().get_mut(name) {
            Some(v) => {
                *v = val.to_owned();
                Ok(v.clone())
            }
            None => {
                if let Some(env) = &self.enclosing {
                    return env.get(name);
                }
                Err(LoxError::Generic(format!("undefined variable `{}`", &name)))
            }
        }
    }

    pub fn get(&self, name: &str) -> LoxResult<LoxValue> {
        if let Some(t) = self.values.borrow().get(name) {
            return Ok(t.clone());
        }

        if let Some(env) = &self.enclosing {
            return env.get(name);
        }

        Err(LoxError::Generic(format!("undefined variable `{}`", &name)))
    }
}
