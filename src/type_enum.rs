use crate::{
    expr,
    consts
};


#[derive(Debug, Clone)]
pub enum Type {
    Number(f64),
    Symbol(String),
    Text(String),
    Function(Vec<String>, Box<expr::Expr>),
    Null,
}

impl Type {
    pub fn get_number(&self) -> f64 {
        match self {
            Type::Number(n) => n.to_owned(),
            Type::Symbol(s) | Type::Text(s) => s.parse().unwrap_or(0.0),
            Type::Null | Type::Function(_, _) => 0.0,
        }
    }

    pub fn get_symbol(&self) -> String {
        match self {
            Type::Symbol(s) => s.to_string(),
            Type::Text(s) => format!("\"{s}\""),
            Type::Number(n) => n.to_string(),
            Type::Null => "null".to_string(),
            Type::Function(args, _) => {
                format!("func ( {} )", args.join(consts::SPACE[0].to_string().as_str()))
            }
        }
    }

    pub fn get_text(&self) -> String {
        match self {
            Type::Number(n) => n.to_string(),
            Type::Symbol(s) | Type::Text(s) => s.to_string(),
            Type::Null | Type::Function(_, _) => String::new(),
        }
    }
}



