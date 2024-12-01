use std::{
    collections::BTreeMap,
    env::args,
    fs::read_to_string,
    io::{self, Write},
};

const VERSION: &str = "1.0.0";
const SPACE: [char; 5] = [' ', '　', '\n', '\t', '\r'];

fn main() {
    let args: Vec<String> = args().collect();
    if let Some(path) = args.get(1) {
        if let Ok(code) = read_to_string(path) {
            if let Some(ast) = Engine::parse(code) {
                let mut engine = Engine::new();
                engine.run(ast);
            } else {
                println!("Syntax Error! something is wrong")
            }
        } else {
            eprintln!("Error! the file can't be opened")
        }
    } else {
        println!("idol {VERSION}");
        println!("(c) 2024 梶塚太智 All rights reserved");
        println!("Repository: https://github.com/KajizukaTaichi/idol");
    }
}

type Scope = BTreeMap<String, Type>;
type Program = Vec<Statement>;
#[derive(Debug, Clone)]
struct Engine {
    scope: Scope,
}

impl Engine {
    fn new() -> Engine {
        Engine {
            scope: BTreeMap::from([
                ("new-line".to_string(), Type::Text("\n".to_string())),
                ("carriage-return".to_string(), Type::Text("\r".to_string())),
                ("double-quote".to_string(), Type::Text("\"".to_string())),
                ("space".to_string(), Type::Text(" ".to_string())),
                ("tab".to_string(), Type::Text("\t".to_string())),
            ]),
        }
    }

    fn parse(source: String) -> Option<Program> {
        let mut program: Program = Vec::new();
        for line in tokenize(source, vec![';'])? {
            let line = line.trim().to_string();
            if line.is_empty() {
                continue;
            }
            program.push(Statement::parse(line.trim().to_string())?);
        }
        Some(program)
    }

    fn run(&mut self, program: Program) -> Option<Type> {
        let mut result = Type::Null;
        for code in program {
            result = match code {
                Statement::Value(expr) => expr.eval(self)?,
                Statement::Print(expr) => {
                    print!("{}", expr.eval(self)?.get_text());
                    io::stdout().flush().unwrap();
                    Type::Null
                }
                Statement::Input(expr) => {
                    let prompt = expr.eval(self)?.get_text();
                    print!("{prompt}");
                    io::stdout().flush().unwrap();
                    let mut buffer = String::new();
                    if io::stdin().read_line(&mut buffer).is_ok() {
                        Type::Text(buffer)
                    } else {
                        return None;
                    }
                }
                Statement::Cast(expr, to) => {
                    let expr = expr.eval(self)?;
                    match to.eval(self)?.get_text().as_str().trim() {
                        "number" => Type::Number(expr.get_number()?),
                        "text" => Type::Text(expr.get_text()),
                        "symbol" => Type::Symbol(expr.get_symbol()),
                        _ => return None,
                    }
                }
                Statement::Let(name, expr) => {
                    let val = expr.eval(&mut self.clone())?;
                    self.scope.insert(name, val.clone());
                    Type::Null
                }
                Statement::If(expr, then, r#else) => {
                    if let Some(it) = expr.eval(self) {
                        self.scope.insert("it".to_string(), it);
                        then.eval(self)?
                    } else {
                        if let Some(r#else) = r#else {
                            r#else.eval(self)?
                        } else {
                            Type::Null
                        }
                    }
                }
                Statement::While(expr, code) => {
                    let mut result = Type::Null;
                    while let Some(it) = expr.eval(self) {
                        self.scope.insert("it".to_string(), it);
                        result = code.eval(self)?;
                    }
                    result
                }
                Statement::For(counter, expr, code) => {
                    let mut result = Type::Null;
                    for i in expr.eval(self)?.get_list() {
                        self.scope.insert(counter.clone(), i);
                        result = code.eval(self)?;
                    }
                    result
                }
                Statement::Lambda(args, code) => Type::Function(args, Box::new(code)),
                Statement::Define(name, args, code) => {
                    self.scope
                        .insert(name, Type::Function(args, Box::new(code)));
                    Type::Null
                }
                Statement::Call(func, value_args) => {
                    let func = func.eval(self);
                    if let Some(Type::Function(func_args, code)) = func {
                        if func_args.len() != value_args.len() {
                            return None;
                        }

                        for (arg, val) in func_args.iter().zip(value_args) {
                            let val = val.eval(self)?;
                            self.scope.insert(arg.to_string(), val);
                        }
                        code.eval(self)?
                    } else {
                        return None;
                    }
                }
                Statement::Fault => return None,
            };
        }
        Some(result)
    }
}

#[derive(Debug, Clone)]
enum Statement {
    Value(Expr),
    Print(Expr),
    Input(Expr),
    Cast(Expr, Expr),
    Let(String, Expr),
    If(Expr, Expr, Option<Expr>),
    For(String, Expr, Expr),
    While(Expr, Expr),
    Lambda(Vec<String>, Expr),
    Define(String, Vec<String>, Expr),
    Call(Expr, Vec<Expr>),
    Fault,
}

impl Statement {
    fn parse(code: String) -> Option<Statement> {
        let code = code.trim();
        if code.starts_with("(") && code.ends_with(")") {
            let code = {
                let mut code = code.to_string();
                code.remove(0);
                code.remove(code.len() - 1);
                code
            };
            Some(Statement::Value(Expr::parse(code)?))
        } else if code.starts_with("print") {
            Some(Statement::Print(Expr::parse(
                code["print".len()..].to_string(),
            )?))
        } else if code.starts_with("input") {
            Some(Statement::Input(Expr::parse(
                code["input".len()..].to_string(),
            )?))
        } else if code.starts_with("cast") {
            let code = code["cast".len()..].to_string();
            let code = tokenize(code, SPACE.to_vec())?;
            if code.get(1)? == "to" {
                Some(Statement::Cast(
                    Expr::parse(code.get(0)?.to_string())?,
                    Expr::parse(code.get(2)?.to_string())?,
                ))
            } else {
                None
            }
        } else if code.starts_with("if") {
            let code = code["if".len()..].to_string();
            let code = tokenize(code, SPACE.to_vec())?;
            if code.get(2).and_then(|x| Some(x == "else")).unwrap_or(false) {
                Some(Statement::If(
                    Expr::parse(code.get(0)?.to_string())?,
                    Expr::parse(code.get(1)?.to_string())?,
                    Some(Expr::parse(code.get(3)?.to_string())?),
                ))
            } else {
                Some(Statement::If(
                    Expr::parse(code.get(0)?.to_string())?,
                    Expr::parse(code.get(1)?.to_string())?,
                    None,
                ))
            }
        } else if code.starts_with("for") {
            let code = code["for".len()..].to_string();
            let code = tokenize(code, SPACE.to_vec())?;
            if code.get(1).and_then(|x| Some(x == "in")).unwrap_or(false) {
                Some(Statement::For(
                    code.get(0)?.to_string(),
                    Expr::parse(code.get(2)?.to_string())?,
                    Expr::parse(code.get(3)?.to_string())?,
                ))
            } else {
                None
            }
        } else if code.starts_with("while") {
            let code = code["while".len()..].to_string();
            let code = tokenize(code, SPACE.to_vec())?;
            Some(Statement::While(
                Expr::parse(code.get(0)?.to_string())?,
                Expr::parse(code.get(1)?.to_string())?,
            ))
        } else if code.starts_with("func") {
            let code = code["func".len()..].to_string();
            let code = tokenize(code, SPACE.to_vec())?;
            let header = code.get(0)?.trim().to_string();
            let header = tokenize(header[1..header.len() - 1].to_string(), SPACE.to_vec())?;
            Some(Statement::Define(
                header.get(0)?.to_string(),
                header.get(1..)?.to_vec(),
                Expr::parse(code.get(1)?.to_string())?,
            ))
        } else if code.starts_with("lambda") {
            let code = code["lambda".len()..].to_string();
            let code = tokenize(code, SPACE.to_vec())?;
            let header = code.get(0)?.trim().to_string();
            let header = tokenize(header[1..header.len() - 1].to_string(), SPACE.to_vec())?;
            Some(Statement::Lambda(
                header,
                Expr::parse(code.get(1)?.to_string())?,
            ))
        } else if code.starts_with("let") {
            let code = code["let".len()..].to_string();
            let (name, code) = code.split_once("=")?;
            Some(Statement::Let(
                name.trim().to_string(),
                Expr::parse(code.to_string())?,
            ))
        } else if code == "fault" {
            Some(Statement::Fault)
        } else {
            let code = tokenize(code.to_string(), SPACE.to_vec())?;
            Some(Statement::Call(
                Expr::parse(code.get(0)?.to_string())?,
                code.get(1..)?
                    .to_vec()
                    .iter()
                    .map(|i| Expr::parse(i.to_string()).unwrap_or(Expr::Value(Type::Null)))
                    .collect(),
            ))
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Infix(Box<Infix>),
    List(Vec<Expr>),
    Block(Program),
    Value(Type),
}

impl Expr {
    fn eval(&self, engine: &mut Engine) -> Option<Type> {
        Some(match self {
            Expr::Infix(infix) => (*infix).eval(engine)?,
            Expr::Block(block) => engine.run(block.clone())?,
            Expr::List(list) => {
                let mut result = vec![];
                for i in list {
                    result.push(i.eval(engine)?)
                }
                Type::List(result)
            }
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

    fn parse(soruce: String) -> Option<Expr> {
        let token_list: Vec<String> = tokenize(soruce, SPACE.to_vec())?;
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
            Expr::parse(token)?
        } else if token.starts_with('{') && token.ends_with('}') {
            let token = {
                let mut token = token.clone();
                token.remove(0);
                token.remove(token.len() - 1);
                token
            };
            Expr::Block(Engine::parse(token)?)
        } else if token.starts_with('[') && token.ends_with(']') {
            let token = {
                let mut token = token.clone();
                token.remove(0);
                token.remove(token.len() - 1);
                token
            };
            let mut list = vec![];
            for elm in tokenize(token, vec![','])? {
                list.push(Expr::parse(elm.trim().to_string())?);
            }
            Expr::List(list)
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
                "::" => Operator::Access,
                _ => return None,
            };
            Some(Expr::Infix(Box::new(Infix {
                operator,
                values: (
                    Expr::parse(token_list.get(..token_list.len() - 2)?.to_vec().join(" "))?,
                    token,
                ),
            })))
        } else {
            return Some(token);
        }
    }
}

#[derive(Debug, Clone)]
struct Infix {
    operator: Operator,
    values: (Expr, Expr),
}

impl Infix {
    fn eval(&self, engine: &mut Engine) -> Option<Type> {
        let left = self.values.0.eval(engine);
        let right = self.values.1.eval(engine);

        Some(match self.operator {
            Operator::Add => {
                if let (Some(Type::Number(left)), Some(Type::Number(right))) = (&left, &right) {
                    Type::Number(left + right)
                } else if let (Some(Type::Text(left)), Some(Type::Text(right))) = (&left, &right) {
                    Type::Text(left.clone() + right)
                } else if let (Some(Type::List(left)), Some(Type::List(right))) = (&left, &right) {
                    Type::List([left.clone(), right.clone()].concat())
                } else if let (Some(Type::List(left)), Some(right)) = (left, right) {
                    Type::List([left, vec![right]].concat())
                } else {
                    return None;
                }
            }
            Operator::Sub => {
                if let (Some(Type::Number(left)), Some(Type::Number(right))) = (&left, &right) {
                    Type::Number(left - right)
                } else if let (Some(Type::Text(left)), Some(Type::Text(right))) = (left, right) {
                    Type::Text(left.replace(&right, ""))
                } else {
                    return None;
                }
            }
            Operator::Mul => {
                if let (Some(Type::Number(left)), Some(Type::Number(right))) = (&left, &right) {
                    Type::Number(left * right)
                } else if let (Some(Type::Text(left)), Some(Type::Number(right))) = (left, right) {
                    Type::Text(left.repeat(right as usize))
                } else {
                    return None;
                }
            }
            Operator::Div => Type::Number(left?.get_number()? / right?.get_number()?),
            Operator::Mod => Type::Number(left?.get_number()? % right?.get_number()?),
            Operator::Pow => Type::Number(left?.get_number()?.powf(right?.get_number()?)),
            Operator::Equal => {
                if left?.get_symbol() == right.clone()?.get_symbol() {
                    right?
                } else {
                    return None;
                }
            }
            Operator::NotEq => {
                if left?.get_symbol() != right.clone()?.get_symbol() {
                    right?
                } else {
                    return None;
                }
            }
            Operator::LessThan => {
                if left.clone()?.get_number() < right.clone()?.get_number() {
                    right?
                } else {
                    return None;
                }
            }
            Operator::LessThanEq => {
                if left?.get_number() <= right.clone()?.get_number() {
                    right?
                } else {
                    return None;
                }
            }
            Operator::GreaterThan => {
                if left?.get_number() > right.clone()?.get_number() {
                    right?
                } else {
                    return None;
                }
            }
            Operator::GreaterThanEq => {
                if left?.get_number() >= right.clone()?.get_number() {
                    right?
                } else {
                    return None;
                }
            }
            Operator::And => {
                if left.is_some() && right.is_some() {
                    right?
                } else {
                    return None;
                }
            }
            Operator::Or => {
                if left.is_some() || right.is_some() {
                    right.unwrap_or(left?)
                } else {
                    return None;
                }
            }
            Operator::Access => {
                if let Some(Type::List(list)) = left.clone() {
                    let index = right?.get_number()?;
                    list.get(index as usize)?.clone()
                } else if let Some(Type::Text(text)) = left {
                    let index = right?.get_number()?;
                    Type::Text(
                        text.chars()
                            .collect::<Vec<char>>()
                            .get(index as usize)?
                            .to_string(),
                    )
                } else {
                    return None;
                }
            }
        })
    }
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Equal,
    NotEq,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    And,
    Or,
    Access,
}

#[derive(Debug, Clone)]
enum Type {
    Number(f64),
    Symbol(String),
    Text(String),
    List(Vec<Type>),
    Function(Vec<String>, Box<Expr>),
    Null,
}

impl Type {
    fn get_number(&self) -> Option<f64> {
        match self {
            Type::Number(n) => Some(n.to_owned()),
            Type::Symbol(s) | Type::Text(s) => {
                if let Ok(n) = s.trim().parse::<f64>() {
                    Some(n)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn get_symbol(&self) -> String {
        match self {
            Type::Symbol(s) => s.to_string(),
            Type::Text(s) => format!("\"{s}\""),
            Type::Number(n) => n.to_string(),
            Type::Null => "null".to_string(),
            Type::Function(args, _) => format!("func ( {} )", args.join(" ")),
            Type::List(l) => format!(
                "[{}]",
                l.iter()
                    .map(|i| i.get_symbol())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }

    fn get_text(&self) -> String {
        match self {
            Type::Number(n) => n.to_string(),
            Type::Symbol(s) | Type::Text(s) => s.to_string(),
            _ => String::new(),
        }
    }

    fn get_list(&self) -> Vec<Type> {
        match self {
            Type::List(list) => list.to_owned(),
            other => vec![other.to_owned()],
        }
    }
}

fn tokenize(input: String, delimiter: Vec<char>) -> Option<Vec<String>> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' | '{' | '[' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' | '}' | ']' if !in_quote => {
                current_token.push(c);
                if in_parentheses > 0 {
                    in_parentheses -= 1;
                } else {
                    return None;
                }
            }
            '"' => {
                in_quote = !in_quote;
                current_token.push(c);
            }
            other => {
                if delimiter.contains(&other) && !in_quote {
                    if in_parentheses != 0 {
                        current_token.push(c);
                    } else if !current_token.is_empty() {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                } else {
                    current_token.push(c);
                }
            }
        }
    }

    // Syntax error check
    if in_quote || in_parentheses != 0 {
        return None;
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }

    Some(tokens)
}
