use crate::{
    types,
    engine,
    infix::Infix,
    type_enum::Type
};

#[derive(Debug, Clone)]
pub enum Expr {
    Infix(Box<Infix>),
    Block(types::Program),
    Value(Type),
}

impl Expr {
    pub fn eval(&self, engine: &mut engine::Engine) -> Option<Type> {
        Some(match self {
            Expr::Infix(infix) => (*infix).eval(engine)?,
            Expr::Block(block) => engine.run_program(block.clone())?,
            Expr::Value(value) => {
                if let Type::Symbol(name) = value {
                    if let Some(refer) = engine.scope.get(name.as_str()) {
                        refer.clone()
                    } else {
                        value.clone()
                    }
                } else {
                    value.clone()
                }
            }
        })
    }
}
