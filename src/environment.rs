use crate::Value;

use std::{cell::RefCell, fmt::Display, rc::Rc};

#[derive(Debug, Clone)]
pub struct Scope {
    enclosing: Option<Rc<RefCell<Scope>>>,
    values: Vec<Rc<RefCell<Value>>>,
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "-----")?;
        for (i, value) in self.values.iter().enumerate() {
            if let Value::Instance(instance) = value.borrow().clone() {
                write!(f, "\n{i}: ")?;
                instance.print_with_content(f)?;
            } else {
                write!(f, "\n{i}: {}", value.borrow())?;
            }
        }
        if let Some(enclosing) = &self.enclosing {
            write!(f, "\n{}", enclosing.borrow())?;
        }
        Ok(())
    }
}

impl Scope {
    pub fn new(enclosing: Option<Rc<RefCell<Scope>>>) -> Self {
        Self {
            enclosing,
            values: Vec::new(),
        }
    }

    pub fn define(&mut self, value: Value) {
        self.values.push(Rc::new(RefCell::new(value)));
    }

    pub fn get(&self, var_index: usize) -> Rc<RefCell<Value>> {
        self.values[var_index].clone()
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    scope: Rc<RefCell<Scope>>,
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.scope.borrow())
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scope: Rc::new(RefCell::new(Scope::new(None))),
        }
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Scope>>) -> Self {
        Self {
            scope: Rc::new(RefCell::new(Scope::new(Some(enclosing)))),
        }
    }

    pub fn define(&mut self, value: Value) {
        self.scope.borrow_mut().define(value);
    }

    pub fn get_scope(&self) -> Rc<RefCell<Scope>> {
        self.scope.clone()
    }

    pub fn get(&self, mut distance: usize, var_index: usize) -> Rc<RefCell<Value>> {
        let mut scope = self.scope.clone();
        while distance > 0 {
            let new_scope = scope
                .borrow()
                .enclosing
                .as_ref()
                .expect("Couldn't resolve environment")
                .clone();
            scope = new_scope;
            distance -= 1;
        }
        let value = scope.borrow().get(var_index);
        value
    }

    pub fn push_scope(&mut self) {
        let new_scope = Rc::new(RefCell::new(Scope::new(Some(self.scope.clone()))));
        self.scope = new_scope;
    }

    pub fn pop_scope(&mut self) {
        let new_scope = self
            .scope
            .borrow()
            .enclosing
            .clone()
            .expect("Couldn't pop scope");
        self.scope = new_scope;
    }
}
