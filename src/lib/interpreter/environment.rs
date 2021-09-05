use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::lib::error::{LoxError, LoxResult};

use super::LoxValue;

/// Stores variables declared during the program, and keeps track of the scopes as well.
///
/// A local environment is created from, and keeps a reference to, it's parent (enclosing)
/// environment. The global one has no enclosing env.
pub(crate) struct Environment {
    values: RefCell<HashMap<Box<str>, LoxValue>>,
    enclosing: Option<Rc<Environment>>,
}

impl Environment {
    /// Creates a new global environment
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: Default::default(),
        }
    }

    /// Creates a local environment with a reference to its parent.
    pub fn from(oth: Rc<Self>) -> Self {
        Self {
            enclosing: Some(oth),
            values: Default::default(),
        }
    }

    /// Define a new variable in the current scope.
    pub fn define(&self, name: &str, val: LoxValue) {
        self.values.borrow_mut().insert(name.into(), val);
    }

    /// Assign to a value at the innermost scope where it's found.
    pub fn assign(&self, name: &str, val: &LoxValue) -> LoxResult<LoxValue> {
        match self.values.borrow_mut().get_mut(name) {
            Some(v) => {
                *v = val.to_owned();
                Ok(v.clone())
            }
            None => {
                if let Some(env) = &self.enclosing {
                    return env.assign(name, val);
                }
                Err(LoxError::Generic(format!("`{}` is not defined", &name)))
            }
        }
    }

    /// Searches for a variable value from the innermost scope.
    pub fn get(&self, name: &str) -> LoxResult<LoxValue> {
        if let Some(t) = self.values.borrow().get(name) {
            return Ok(t.clone());
        }

        if let Some(env) = &self.enclosing {
            return env.get(name);
        }

        Err(LoxError::Generic(format!("`{}` is not defined", &name)))
    }

    /// Gets a value from a speficic scope.
    ///
    /// This function relies on the variable binding and resolution performed by the [`Resolver`].
    /// So we already know that the variable exists and where it was declared.
    pub fn get_at(&self, distance: usize, name: &str) -> LoxResult<LoxValue> {
        let values = self.ancestor(distance).values.borrow();
        let val = values.get(name).unwrap();
        Ok(val.to_owned())
    }

    pub fn assign_at(&self, distance: usize, name: &str, val: &LoxValue) -> LoxResult<()> {
        let mut values = self.ancestor(distance).values.borrow_mut();
        values.insert(name.into(), val.to_owned());
        Ok(())
    }

    /// Helper function that retrieves a scope at a given distance from the local scope.
    fn ancestor(&self, distance: usize) -> &Self {
        let mut env = self;
        for _ in 0..distance {
            env = env.enclosing.as_ref().unwrap();
        }
        env
    }

    pub fn global(&self) -> &Self {
        let mut env = self;
        while let Some(ref e) = env.enclosing {
            env = e
        }
        env
    }
}
