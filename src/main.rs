use std::collections::HashMap;

fn main() {
    println!("Hello, idol!");
    let scope = &mut HashMap::new();
    dbg!(parse_expr("1 < 2 < 3".to_string(), scope)
        .unwrap()
        .eval(scope));
}

fn parse_expr(soruce: String, scope: &mut HashMap<String, Type>) -> Option<Expr> {
    let tokens: Vec<String> = tokenize_expr(soruce)?;
    let left = tokens.last()?.trim().to_string();
    let left = if let Ok(n) = left.parse::<f64>() {
        Expr::Value(Type::Number(n))
    } else if left.starts_with('(') && left.ends_with(')') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        parse_expr(left, scope)?
    } else {
        Expr::Value(Type::Symbol(left))
    };

    if let Some(operator) = {
        let mut tokens = tokens.clone();
        tokens.reverse();
        tokens
    }
    .get(1)
    {
        let operator = match operator.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "^" => Operator::Pow,
            "==" => Operator::Equal,
            "<" => Operator::LessThan,
            ">" => Operator::GreaterThan,
            _ => return None,
        };
        Some(Expr::Infix(Box::new(Infix {
            operator,
            values: (
                parse_expr(tokens.get(..tokens.len() - 2)?.to_vec().join(" "), scope)?,
                left,
            ),
        })))
    } else {
        return Some(left);
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
    fn eval(&self, scope: &mut HashMap<String, Type>) -> Option<Type> {
        Some(match self {
            Expr::Infix(infix) => (*infix).eval(scope)?,
            Expr::Value(value) => {
                if let Type::Symbol(name) = value {
                    if let Some(refer) = scope.get(name.as_str()).cloned() {
                        refer
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
    LessThan,
    GreaterThan,
}

impl Infix {
    fn eval(&self, scope: &mut HashMap<String, Type>) -> Option<Type> {
        let left = self.values.0.eval(scope)?;
        let right = self.values.1.eval(scope)?;
        dbg!(&left, &right);

        Some(match self.operator {
            Operator::Add => Type::Number(left.get_number() + right.get_number()),
            Operator::Sub => Type::Number(left.get_number() - right.get_number()),
            Operator::Mul => Type::Number(left.get_number() * right.get_number()),
            Operator::Div => Type::Number(left.get_number() / right.get_number()),
            Operator::Mod => Type::Number(left.get_number() % right.get_number()),
            Operator::Pow => Type::Number(left.get_number().powf(right.get_number())),
            Operator::Equal => {
                if left.get_symbol() == right.get_symbol() {
                    left
                } else {
                    return None;
                }
            }
            Operator::LessThan => {
                if left.get_number() < right.get_number() {
                    right
                } else {
                    return None;
                }
            }
            Operator::GreaterThan => {
                if left.get_number() > right.get_number() {
                    right
                } else {
                    return None;
                }
            }
        })
    }
}
