use rustyline::DefaultEditor;
use std::collections::BTreeMap;

fn main() {
    println!("idol 0.1.0");
    let mut engine = Engine::new();
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let code = rl.readline("> ").unwrap();
        if code.is_empty() {
            continue;
        }

        if let Some(ast) = parse_program(code.clone()) {
            engine.set_program(ast);
            engine.run_program();
        }
    }
}

fn parse_expr(soruce: String) -> Option<Expr> {
    let token_list: Vec<String> = tokenize_expr(soruce)?;
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

fn tokenize_expr(input: String) -> Option<Vec<String>> {
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
            ' ' | '　' | '\t' if !in_quote => {
                if in_parentheses != 0 {
                    current_token.push(c);
                } else if !current_token.is_empty() {
                    tokens.push(current_token.clone());
                    current_token.clear();
                }
            }
            '"' => {
                in_quote = !in_quote;
                current_token.push(c);
            }
            _ => {
                current_token.push(c);
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
    for line in source.lines() {
        let line = line.trim().to_string();
        if let Some((ln, code)) = line.split_once(":") {
            program.push((
                Some(ln.to_string()),
                parse_opecode(code.trim().to_string())?,
            ));
        } else {
            program.push((None, parse_opecode(line.trim().to_string())?));
        }
    }
    Some(program)
}

fn parse_opecode(code: String) -> Option<Statement> {
    let code = code.trim();
    Some(if code.starts_with("print") {
        Statement::Print(parse_expr(code["print".len()..].to_string())?)
    } else if code.starts_with("goto") {
        Statement::Goto(parse_expr(code["goto".len()..].to_string())?)
    } else if code.starts_with("if") {
        let code = code["if".len()..].to_string();
        let (cond, code) = code.split_once("then")?;
        if let Some((then, elses)) = code.split_once("else") {
            Statement::If(
                parse_expr(cond.to_string())?,
                Box::new(parse_opecode(then.to_string())?),
                Some(Box::new(parse_opecode(elses.to_string())?)),
            )
        } else {
            Statement::If(
                parse_expr(cond.to_string())?,
                Box::new(parse_opecode(code.to_string())?),
                None,
            )
        }
    } else if code.starts_with("let") {
        let code = code["let".len()..].to_string();
        let (name, code) = code.split_once("=")?;
        Statement::Let(name.trim().to_string(), parse_expr(code.to_string())?)
    } else if code == "end" {
        Statement::End
    } else {
        return None;
    })
}

#[derive(Debug, Clone)]
struct Engine {
    pc: usize,
    program: Program,
    scope: Scope,
}

impl Engine {
    fn new() -> Engine {
        Engine {
            pc: 0,
            program: vec![],
            scope: BTreeMap::new(),
        }
    }

    fn set_program(&mut self, program: Program) {
        self.program.extend(program);
    }

    fn run_program(&mut self) -> Option<()> {
        while self.program.len() > self.pc {
            let code = self.program[self.pc].1.clone();
            if self.run_opecode(code)? {
                self.pc += 1;
            }
        }
        self.pc = 0;
        Some(())
    }

    fn run_opecode(&mut self, code: Statement) -> Option<bool> {
        match code {
            Statement::Print(expr) => println!("{}", expr.eval(&mut self.scope)?.get_symbol()),
            Statement::Let(name, expr) => {
                self.scope.insert(name, expr.eval(&mut self.scope.clone())?);
            }
            Statement::If(expr, then, elses) => {
                if expr.eval(&mut self.scope).is_some() {
                    if !self.run_opecode(*then)? {
                        return Some(false);
                    }
                } else {
                    if let Some(elses) = elses {
                        if !self.run_opecode(*elses)? {
                            return Some(false);
                        }
                    }
                }
            }
            Statement::Goto(addr) => {
                let addr = addr.eval(&mut self.scope)?.get_symbol();
                self.pc = self.program.iter().position(|(x, _)| {
                    if let Some(x) = x {
                        x.clone() == addr
                    } else {
                        false
                    }
                })?;
                return Some(false);
            }
            Statement::End => std::process::exit(0),
        }
        Some(true)
    }
}

type Scope = BTreeMap<String, Type>;
type Program = Vec<(Option<String>, Statement)>;
#[derive(Debug, Clone)]
enum Statement {
    Print(Expr),
    Let(String, Expr),
    Goto(Expr),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    End,
}

#[derive(Debug, Clone)]
enum Type {
    Number(f64),
    Symbol(String),
}

impl Type {
    fn get_number(&self) -> f64 {
        match self {
            Type::Number(n) => n.to_owned(),
            Type::Symbol(s) => s.parse().unwrap_or(0.0),
        }
    }

    fn get_symbol(&self) -> String {
        match self {
            Type::Symbol(s) => s.to_string(),
            Type::Number(n) => n.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Infix(Box<Infix>),
    Value(Type),
}

impl Expr {
    fn eval(&self, scope: &mut BTreeMap<String, Type>) -> Option<Type> {
        Some(match self {
            Expr::Infix(infix) => (*infix).eval(scope)?,
            Expr::Value(value) => {
                if let Type::Symbol(name) = value {
                    if let Some(refer) = scope.get(name.as_str()) {
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
    fn eval(&self, scope: &mut BTreeMap<String, Type>) -> Option<Type> {
        let left = self.values.0.eval(scope);
        let right = self.values.1.eval(scope);

        Some(match self.operator {
            Operator::Add => Type::Number(left?.get_number() + right?.get_number()),
            Operator::Sub => Type::Number(left?.get_number() - right?.get_number()),
            Operator::Mul => Type::Number(left?.get_number() * right?.get_number()),
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
