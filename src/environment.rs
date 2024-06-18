use std::collections::HashMap;

use crate::Value;

#[derive(Debug, Clone)]
pub struct Scope {
    values: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.values.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.values.get_mut(name)
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    scopes: Vec<Scope>
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }
    pub fn define(&mut self, name: String, value: Value) {
        self.scopes.last_mut().expect("missing scope").define(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var)
            }
        }
        None
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(var) = scope.get_mut(name) {
                return Some(var)
            }
        }
        None
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() > 1);
        self.scopes.pop();
    }
}
