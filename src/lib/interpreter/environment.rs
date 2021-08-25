use std::collections::HashMap;

use std::cell::RefCell;

use crate::error::{LoxError, LoxResult};

use super::LoxValue;

/// Stores variables declared during the program, and keeps track of the scopes as well.
///
/// A local environment is created from, and keeps a reference to, it's parent (enclosing)
/// environment. The global one has no enclosing env.
/// The lifetime here bounds to the "outter" environment, which must live longer than
#[derive(Debug)]
pub(crate) struct Environment<'a> {
    values: RefCell<HashMap<Box<str>, LoxValue>>,
    enclosing: Option<&'a Environment<'a>>,
}

impl<'p> Environment<'p> {
    /// Creates a new global environment
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: Default::default(),
        }
    }

    /// Creates a local environment with a reference to its parent.
    pub fn from(oth: &'p Self) -> Self {
        Self {
            enclosing: Some(oth),
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
