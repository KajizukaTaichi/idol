use crate::{
    expr::Expr,
    operator::Operator,
    type_enum::Type,
    parse::parse_program,
    consts::SPACE,
    infix::Infix,
    tokenize,

};


pub fn parse_expr(soruce: String) -> Option<Expr> {
    let token_list: Vec<String> = tokenize::tokenize_expr(soruce, SPACE.to_vec())?;
    let token = token_list.last()?.trim().to_string();
    let token = if let Ok(n) = token.parse::<f64>() {
        Expr::Value(Type::Number(n))
    } else if token.starts_with('(') && token.ends_with(')') {
        let token = {
            let mut token = token.clone();
            token.remove(0);
            token.remove(token.len() - 1);
            token
        };
        parse_expr(token)?
    } else if token.starts_with('{') && token.ends_with('}') {
        let token = {
            let mut token = token.clone();
            token.remove(0);
            token.remove(token.len() - 1);
            token
        };
        Expr::Block(parse_program(token)?)
    } else if token.starts_with('"') && token.ends_with('"') {
        let token = {
            let mut token = token.clone();
            token.remove(0);
            token.remove(token.len() - 1);
            token
        };
        Expr::Value(Type::Text(token))
    } else {
        Expr::Value(Type::Symbol(token))
    };

    if let Some(operator) = token_list
        .len()
        .checked_sub(2)
        .and_then(|idx| token_list.get(idx))
    {
        let operator = match operator.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "^" => Operator::Pow,
            "==" => Operator::Equal,
            "!=" => Operator::NotEq,
            "<" => Operator::LessThan,
            "<=" => Operator::LessThanEq,
            ">" => Operator::GreaterThan,
            ">=" => Operator::GreaterThanEq,
            "&" => Operator::And,
            "|" => Operator::Or,
            _ => return None,
        };
        Some(Expr::Infix(Box::new(Infix {
            operator,
            values: (
                parse_expr(token_list.get(..token_list.len() - 2)?.to_vec().join(" "))?,
                token,
            ),
        })))
    } else {
        return Some(token);
    }
}
