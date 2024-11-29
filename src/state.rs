use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Statement {
    Print(Expr),
    Input(Expr),
    Cast(Expr, String),
    Let(String, Expr),
    If(Expr, Expr, Option<Expr>),
    While(Expr, Expr),
    Lambda(Vec<String>, Expr),
    Define(String, Vec<String>, Expr),
    Call(Expr, Vec<Expr>),
    Fault,
}
