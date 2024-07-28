use crate::{
    Environment, FunctionStmt, Interpreter, InterpreterError, InterpreterErrorKind, Token, Value,
};

use dyn_clone::DynClone;

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

pub trait Callable: Debug + Display + DynClone {
    fn arity(&self) -> usize;
    fn call(&self, arguments: Vec<Value>) -> Result<Value, InterpreterError>;
}

pub trait CallableFunction: Callable {}

#[derive(Debug, Clone)]
pub struct ClockFn {}

impl ClockFn {
    pub fn new() -> Self {
        Self {}
    }
}

impl Display for ClockFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}

impl Callable for ClockFn {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _arguments: Vec<Value>) -> Result<Value, InterpreterError> {
        Ok(Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("couldn't retrieve current time")
                .as_secs_f64(),
        ))
    }
}

impl CallableFunction for ClockFn {}

#[derive(Debug, Clone)]
pub struct Function {
    function: FunctionStmt,
    closure: Environment,
    is_initializer: bool,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}> ", self.function.name.lexeme)
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.function.params.len()
    }

    fn call(&self, arguments: Vec<Value>) -> Result<Value, InterpreterError> {
        let mut interpreter = Interpreter::with_environment(self.closure.clone());
        interpreter.environment.push_scope();
        for i in 0..self.function.params.len() {
            interpreter.environment.define(arguments[i].clone());
        }

        let retval = interpreter
            .run(&self.function.body)
            .map(|x| x.unwrap_or(Value::Nil))?;
        if self.is_initializer {
            Ok(self.closure.get(0, 0).borrow().clone())
        } else {
            Ok(retval)
        }
    }
}

impl CallableFunction for Function {}

impl Function {
    pub fn new(function: FunctionStmt, closure: Environment) -> Self {
        Self {
            function,
            closure,
            is_initializer: false,
        }
    }

    pub fn bind(&self, instance: Instance) -> Self {
        let mut closure = self.closure.clone();
        closure.push_scope();
        closure.define(Value::from(instance));
        let mut function = Function::new(self.function.clone(), closure);
        function.is_initializer = self.is_initializer;
        function
    }
}

#[derive(Debug, Clone)]
struct ClassData {
    name: Token,
    superclass_data: Option<Rc<RefCell<ClassData>>>,
    methods: HashMap<String, Rc<Function>>,
}

impl ClassData {
    pub fn register_method(&mut self, method: FunctionStmt, environment: Environment) {
        let method_name = method.name.lexeme.clone();
        let mut function = Function::new(method, environment);
        function.is_initializer = method_name == String::from("init");
        self.methods.insert(method_name, Rc::new(function));
    }

    pub fn get_method(&self, name: &str) -> Option<Rc<Function>> {
        if let Some(method) = self.methods.get(name).map(|x| x.clone()) {
            Some(method)
        } else if let Some(superclass_data) = &self.superclass_data {
            superclass_data.borrow().get_method(name)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    data: Rc<RefCell<ClassData>>,
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data.borrow().name.lexeme)
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        self.get_method("init")
            .map(|initializer| initializer.arity())
            .unwrap_or(0)
    }

    fn call(&self, arguments: Vec<Value>) -> Result<Value, InterpreterError> {
        let instance = Instance::new(self.clone());
        if let Some(initializer) = self.get_method("init") {
            initializer.bind(instance.clone()).call(arguments)?;
        }
        Ok(Value::from(instance))
    }
}

impl Class {
    pub fn new(name: Token, superclass: Option<Class>) -> Self {
        Self {
            data: Rc::new(RefCell::new(ClassData {
                name,
                superclass_data: superclass.map(|x| x.data),
                methods: HashMap::new(),
            })),
        }
    }

    pub fn register_method(&mut self, method: FunctionStmt, environment: Environment) {
        self.data.borrow_mut().register_method(method, environment);
    }

    pub fn get_method(&self, name: &str) -> Option<Rc<Function>> {
        self.data.borrow().get_method(name)
    }
}

#[derive(Debug, Clone)]
struct InstanceData {
    class: Class,
    fields: HashMap<String, Value>,
}

impl InstanceData {
    pub fn set(&mut self, name: &Token, value: Value) {
        self.fields.insert(name.lexeme.clone(), value);
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    data: Rc<RefCell<InstanceData>>,
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} instance ({:p})",
            self.data.borrow().class.data.borrow().name.lexeme,
            self.data.borrow().deref()
        )
    }
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Self {
            data: Rc::new(RefCell::new(InstanceData {
                class,
                fields: HashMap::new(),
            })),
        }
    }

    pub fn get(&self, name: &Token) -> Result<Value, InterpreterError> {
        if let Some(v) = self.data.borrow().fields.get(&name.lexeme) {
            Ok(v.clone())
        } else if let Some(m) = self.data.borrow().class.get_method(&name.lexeme) {
            Ok(Value::from(m.bind(self.clone())))
        } else {
            Err(InterpreterError::new(
                name,
                InterpreterErrorKind::UndefinedProperty,
            ))
        }
    }

    pub fn set(&mut self, name: &Token, value: Value) {
        self.data.borrow_mut().set(name, value);
    }

    pub fn print_with_content(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)?;
        for (name, value) in self.data.borrow().class.data.borrow().methods.iter() {
            write!(f, "\n  {name}: {value}")?;
        }
        for (name, value) in self.data.borrow().fields.iter() {
            write!(f, "\n  {name}: {value}")?;
        }
        Ok(())
    }
}
