use crate::{
    types,
    type_enum::Type,
    state
};
use rustyline::DefaultEditor;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct Engine {
    pub scope: types::Scope,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            scope: BTreeMap::from([("new-line".to_string(), Type::Text("\n".to_string()))]),
        }
    }

    pub fn run_program(&mut self, program: types::Program) -> Option<Type> {
        let mut result = Type::Null;
        for code in program {
            result = self.run_opecode(code)?;
        }
        Some(result)
    }

    pub fn run_opecode(&mut self, code: state::Statement) -> Option<Type> {
        match code {
            state::Statement::Print(expr) => {
                print!("{}", expr.eval(self)?.get_text());
                Some(Type::Null)
            }
            state::Statement::Input(expr) => {
                let prompt = expr.eval(self)?.get_text();
                Some(Type::Text(
                    DefaultEditor::new()
                        .and_then(|mut rl| rl.readline(&prompt))
                        .unwrap_or_default(),
                ))
            }
            state::Statement::Cast(expr, to) => match to.as_str() {
                "number" => {
                    if let Ok(n) = expr.eval(self)?.get_text().parse() {
                        Some(Type::Number(n))
                    } else {
                        None
                    }
                }
                "text" => Some(Type::Text(expr.eval(self)?.get_text())),
                "symbol" => Some(Type::Symbol(expr.eval(self)?.get_symbol())),
                _ => None,
            },
            state::Statement::Let(name, expr) => {
                let val = expr.eval(&mut self.clone())?;
                self.scope.insert(name, val.clone());
                Some(val)
            }
            state::Statement::If(expr, then, r#else) => {
                if let Some(it) = expr.eval(self) {
                    self.scope.insert("it".to_string(), it);
                    then.eval(self)
                } else {
                    if let Some(r#else) = r#else {
                        r#else.eval(self)
                    } else {
                        Some(Type::Null)
                    }
                }
            }
            state::Statement::While(expr, code) => {
                let mut result = Type::Null;
                while let Some(it) = expr.eval(self) {
                    self.scope.insert("it".to_string(), it);
                    result = code.eval(self)?;
                }
                Some(result)
            }
            state::Statement::Lambda(args, code) => {
                let func_obj = Type::Function(args, Box::new(code));
                Some(func_obj)
            }
            state::Statement::Define(name, args, code) => {
                let func_obj = Type::Function(args, Box::new(code));
                self.scope.insert(name, func_obj.clone());
                Some(func_obj)
            }
            state::Statement::Call(func, value_args) => {
                let func = func.eval(self);
                if let Some(Type::Function(func_args, code)) = func {
                    if func_args.len() != value_args.len() {
                        return None;
                    }

                    for (arg, val) in func_args.iter().zip(value_args) {
                        let val = val.eval(self)?;
                        self.scope.insert(arg.to_string(), val);
                    }
                    code.eval(self)
                } else {
                    func
                }
            }
            state::Statement::Fault => None,
        }
    }
}
