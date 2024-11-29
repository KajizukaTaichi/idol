use crate::{
    consts,
    tokenize,
    state::Statement,
    parse_expr,
    types,
    expr::Expr,
    type_enum::Type
};


pub fn parse_program(source: String) -> Option<types::Program> {
    let mut program: types::Program = Vec::new();
    for line in tokenize::tokenize_expr(source, vec![';'])? {
        let line = line.trim().to_string();
        if line.is_empty() {
            continue;
        }
        program.push(parse_opecode(line.trim().to_string())?);
    }
    Some(program)
}


pub fn parse_opecode(code: String) -> Option<Statement> {
    let code = code.trim();
    Some(if code.starts_with("print") {
        Statement::Print(parse_expr::parse_expr(code["print".len()..].to_string())?)
    } else if code.starts_with("input") {
        Statement::Input(parse_expr::parse_expr(code["input".len()..].to_string())?)
    } else if code.starts_with("cast") {
        let code = code["cast".len()..].to_string();
        let (expr, r#type) = {
            let splited = code.split("to").collect::<Vec<&str>>();
            (
                parse_expr::parse_expr(splited.get(..splited.len() - 1)?.to_vec().join("to"))?,
                splited.last()?.trim().to_string(),
            )
        };
        Statement::Cast(expr, r#type)
    } else if code.starts_with("if") {
        let code = code["if".len()..].to_string();
        let code = tokenize::tokenize_expr(code, consts::SPACE.to_vec())?;
        if code.get(2).and_then(|x| Some(x == "else")).unwrap_or(false) {
            Statement::If(
                parse_expr::parse_expr(code.get(0)?.to_string())?,
                parse_expr::parse_expr(code.get(1)?.to_string())?,
                Some(parse_expr::parse_expr(code.get(3)?.to_string())?),
            )
        } else {
            Statement::If(
                parse_expr::parse_expr(code.get(0)?.to_string())?,
                parse_expr::parse_expr(code.get(1)?.to_string())?,
                None,
            )
        }
    } else if code.starts_with("while") {
        let code = code["while".len()..].to_string();
        let code = tokenize::tokenize_expr(code, consts::SPACE.to_vec())?;
        Statement::While(
            parse_expr::parse_expr(code.get(0)?.to_string())?,
            parse_expr::parse_expr(code.get(1)?.to_string())?,
        )
    } else if code.starts_with("func") {
        let code = code["func".len()..].to_string();
        let code = tokenize::tokenize_expr(code, consts::SPACE.to_vec())?;
        let header = code.get(0)?.trim().to_string();
        let header = tokenize::tokenize_expr(header[1..header.len() - 1].to_string(), consts::SPACE.to_vec())?;
        Statement::Define(
            header.get(0)?.to_string(),
            header.get(1..)?.to_vec(),
            parse_expr::parse_expr(code.get(1)?.to_string())?,
        )
    } else if code.starts_with("lambda") {
        let code = code["lambda".len()..].to_string();
        let code = tokenize::tokenize_expr(code, consts::SPACE.to_vec())?;
        let header = code.get(0)?.trim().to_string();
        let header = tokenize::tokenize_expr(header[1..header.len() - 1].to_string(), consts::SPACE.to_vec())?;
        Statement::Lambda(header, parse_expr::parse_expr(code.get(1)?.to_string())?)
    } else if code.starts_with("let") {
        let code = code["let".len()..].to_string();
        let (name, code) = code.split_once("=")?;
        Statement::Let(name.trim().to_string(), parse_expr::parse_expr(code.to_string())?)
    } else if code == "fault" {
        Statement::Fault
    } else {
        let code = tokenize::tokenize_expr(code.to_string(), consts::SPACE.to_vec())?;
        Statement::Call(
            parse_expr::parse_expr(code.get(0)?.to_string())?,
            code.get(1..)?
                .to_vec()
                .iter()
                .map(|i| parse_expr::parse_expr(i.to_string()).unwrap_or(Expr::Value(Type::Null)))
                .collect(),
        )
    })
}
