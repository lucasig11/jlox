use std::collections::HashMap;

use std::cell::RefCell;

use crate::error::{LoxError, LoxResult};

use super::LoxValue;

#[derive(Debug)]
pub(crate) struct Environment {
    values: RefCell<HashMap<Box<str>, LoxValue>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: Default::default(),
        }
    }

    pub fn define(&self, name: &str, val: LoxValue) {
        self.values.borrow_mut().insert(name.into(), val);
    }

    pub fn get(&self, name: &str) -> LoxResult<LoxValue> {
        if let Some(t) = self.values.borrow_mut().get(name) {
            Ok(t.clone())
        } else {
            Err(LoxError::Generic(format!("undefined variable `{}`", &name)))
        }
    }
}
