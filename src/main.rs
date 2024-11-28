use clap::Parser;
use rustyline::DefaultEditor;
use std::{collections::BTreeMap, fs::read_to_string};

const VERSION: &str = "1.0.0";

#[derive(Parser, Debug)]
#[command(name = "idol", version=VERSION)]
struct Cli {
    #[arg(index = 1)]
    file: Option<String>,
}

fn main() {
    let cli = Cli::parse();
    let mut engine = Engine::new();

    if let Some(path) = cli.file {
        if let Ok(code) = read_to_string(path) {
            if let Some(ast) = parse_program(code) {
                engine.run_program(ast);
            }
        }
    } else {
        repl();
    }
}

fn repl() {
    println!("idol {VERSION}");
    let mut engine = Engine::new();
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        match rl.readline("> ") {
            Ok(code) => {
                if code.is_empty() {
                    continue;
                }

                if let Some(ast) = parse_program(code) {
                    if let Some(result) = engine.run_program(ast) {
                        println!("{}", result.get_symbol());
                    }
                }
            }
            Err(err) => println!("{err:?}"),
        }
    }
}

fn parse_expr(soruce: String) -> Option<Expr> {
    let token_list: Vec<String> = tokenize_expr(soruce, vec![' ', '　', '\n', '\t', '\r'])?;
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
        Expr::Value(Type::String(token))
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

fn tokenize_expr(input: String, delimiter: Vec<char>) -> Option<Vec<String>> {
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
                    eprintln!("Error! there's duplicate end of the parentheses");
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
    if in_quote {
        eprintln!("Error! there's not end of the quote");
        return None;
    }
    if in_parentheses != 0 {
        eprintln!("Error! there's not end of the parentheses");
        return None;
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }

    Some(tokens)
}

fn parse_program(source: String) -> Option<Program> {
    let mut program: Program = Vec::new();
    for line in tokenize_expr(source, vec![';'])? {
        let line = line.trim().to_string();
        if line.is_empty() {
            continue;
        }
        program.push(parse_opecode(line.trim().to_string())?);
    }
    Some(program)
}

fn parse_opecode(code: String) -> Option<Statement> {
    let code = code.trim();
    Some(if code.starts_with("print") {
        Statement::Print(parse_expr(code["print".len()..].to_string())?)
    } else if code.starts_with("if") {
        let code = code["if".len()..].to_string();
        let code = tokenize_expr(code, vec![' ', '　', '\n', '\t', '\r'])?;
        if code.get(2).and_then(|x| Some(x == "else")).unwrap_or(false) {
            Statement::If(
                parse_expr(code.get(0)?.to_string())?,
                parse_expr(code.get(1)?.to_string())?,
                Some(parse_expr(code.get(3)?.to_string())?),
            )
        } else {
            Statement::If(
                parse_expr(code.get(0)?.to_string())?,
                parse_expr(code.get(1)?.to_string())?,
                None,
            )
        }
    } else if code.starts_with("while") {
        let code = code["while".len()..].to_string();
        let code = tokenize_expr(code, vec![' ', '　', '\n', '\t', '\r'])?;
        Statement::While(
            parse_expr(code.get(0)?.to_string())?,
            parse_expr(code.get(1)?.to_string())?,
        )
    } else if code.starts_with("func") {
        let code = code["func".len()..].to_string();
        let code = tokenize_expr(code, vec![' ', '　', '\n', '\t', '\r'])?;
        Statement::Define(
            code.get(0)?.to_string(),
            {
                let args = code.get(1)?.to_string();
                args[1..args.len() - 1]
                    .split(",")
                    .map(String::from)
                    .collect()
            },
            parse_expr(code.get(2)?.to_string())?,
        )
    } else if code.starts_with("let") {
        let code = code["let".len()..].to_string();
        let (name, code) = code.split_once("=")?;
        Statement::Let(name.trim().to_string(), parse_expr(code.to_string())?)
    } else if code.starts_with("expr") {
        let code = code["expr".len()..].to_string();
        Statement::Expr(parse_expr(code)?)
    } else {
        let code = tokenize_expr(code.to_string(), vec![' ', '　', '\n', '\t', '\r'])?;
        Statement::Call(
            code.get(0)?.to_string(),
            code.get(1..)?
                .to_vec()
                .iter()
                .map(|i| parse_expr(i.to_string()).unwrap_or(Expr::Value(Type::Null)))
                .collect(),
        )
    })
}

#[derive(Debug, Clone)]
struct Engine {
    scope: Scope,
}

impl Engine {
    fn new() -> Engine {
        Engine {
            scope: BTreeMap::from([("new-line".to_string(), Type::String("\n".to_string()))]),
        }
    }

    fn run_program(&mut self, program: Program) -> Option<Type> {
        let mut result = Type::Null;
        for code in program {
            result = self.run_opecode(code)?;
        }
        Some(result)
    }

    fn run_opecode(&mut self, code: Statement) -> Option<Type> {
        match code {
            Statement::Print(expr) => {
                let val = expr.eval(self)?.get_string();
                print!("{val}");
                Some(Type::Null)
            }
            Statement::Let(name, expr) => {
                let val = expr.eval(&mut self.clone())?;
                self.scope.insert(name, val.clone());
                Some(val)
            }
            Statement::If(expr, then, r#else) => {
                if expr.eval(self).is_some() {
                    then.eval(self)
                } else {
                    if let Some(r#else) = r#else {
                        r#else.eval(self)
                    } else {
                        Some(Type::Null)
                    }
                }
            }
            Statement::While(expr, code) => {
                let mut result = Type::Null;
                while expr.eval(self).is_some() {
                    result = code.eval(self)?;
                }
                Some(result)
            }
            Statement::Define(name, args, code) => {
                let func_obj = Type::Function(args, Box::new(code));
                self.scope.insert(name, func_obj.clone());
                Some(func_obj)
            }
            Statement::Call(name, value_args) => {
                if let Some(Type::Function(func_args, code)) = self.scope.clone().get(&name) {
                    for (arg, val) in func_args.iter().zip(value_args) {
                        let val = val.eval(self)?;
                        self.scope.insert(arg.to_string(), val);
                    }
                    code.eval(self)
                } else {
                    return None;
                }
            }
            Statement::Expr(expr) => expr.eval(self),
        }
    }
}

type Scope = BTreeMap<String, Type>;
type Program = Vec<Statement>;
#[derive(Debug, Clone)]
enum Statement {
    Print(Expr),
    Let(String, Expr),
    If(Expr, Expr, Option<Expr>),
    While(Expr, Expr),
    Define(String, Vec<String>, Expr),
    Call(String, Vec<Expr>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
enum Type {
    Number(f64),
    Symbol(String),
    String(String),
    Function(Vec<String>, Box<Expr>),
    Null,
}

impl Type {
    fn get_number(&self) -> f64 {
        match self {
            Type::Number(n) => n.to_owned(),
            Type::Symbol(s) | Type::String(s) => s.parse().unwrap_or(0.0),
            Type::Null | Type::Function(_, _) => 0.0,
        }
    }

    fn get_symbol(&self) -> String {
        match self {
            Type::Symbol(s) => s.to_string(),
            Type::String(s) => format!("\"{s}\""),
            Type::Number(n) => n.to_string(),
            Type::Null => "null".to_string(),
            Type::Function(args, _) => format!("function({})", args.join(", ")),
        }
    }

    fn get_string(&self) -> String {
        match self {
            Type::Symbol(s) | Type::String(s) => s.to_string(),
            Type::Number(n) => n.to_string(),
            Type::Null | Type::Function(_, _) => String::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Infix(Box<Infix>),
    Block(Program),
    Value(Type),
}

impl Expr {
    fn eval(&self, engine: &mut Engine) -> Option<Type> {
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

#[derive(Debug, Clone)]
struct Infix {
    operator: Operator,
    values: (Expr, Expr),
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
}

impl Infix {
    fn eval(&self, engine: &mut Engine) -> Option<Type> {
        let left = self.values.0.eval(engine);
        let right = self.values.1.eval(engine);

        Some(match self.operator {
            Operator::Add => {
                if let (Some(Type::Number(left)), Some(Type::Number(right))) = (&left, &right) {
                    Type::Number(left + right)
                } else if let (Some(Type::String(left)), Some(Type::String(right))) = (left, right)
                {
                    Type::String(left + &right)
                } else {
                    return None;
                }
            }
            Operator::Sub => {
                if let (Some(Type::Number(left)), Some(Type::Number(right))) = (&left, &right) {
                    Type::Number(left - right)
                } else if let (Some(Type::String(left)), Some(Type::String(right))) = (left, right)
                {
                    Type::String(left.replace(&right, ""))
                } else {
                    return None;
                }
            }
            Operator::Mul => {
                if let (Some(Type::Number(left)), Some(Type::Number(right))) = (&left, &right) {
                    Type::Number(left * right)
                } else if let (Some(Type::String(left)), Some(Type::Number(right))) = (left, right)
                {
                    Type::String(left.repeat(right as usize))
                } else {
                    return None;
                }
            }
            Operator::Div => Type::Number(left?.get_number() / right?.get_number()),
            Operator::Mod => Type::Number(left?.get_number() % right?.get_number()),
            Operator::Pow => Type::Number(left?.get_number().powf(right?.get_number())),
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
                if left.is_some() || right.is_none() {
                    right.unwrap_or(left?)
                } else {
                    return None;
                }
            }
        })
    }
}
