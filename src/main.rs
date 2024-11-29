use clap::Parser;
use rustyline::DefaultEditor;
use std::fs;

mod parse_expr;
mod consts;
mod tokenize;
mod parse;
mod types;
mod state;
mod engine;
mod expr;
mod infix;
mod operator;
mod type_enum;


#[derive(Parser, Debug)]
#[command(name = "idol", version = consts::VERSION, about = "Goal-directed evaluation programming language inspired by Icon")]
struct Cli {
    /// Script file to be running
    #[arg(index = 1)]
    file: Option<String>,

    /// Enable debug mode to show AST
    #[arg(long, short)]
    debug: bool,
}

fn main() {
    let cli = Cli::parse();
    let mut engine = engine::Engine::new();

    if let Some(path) = cli.file {
        if let Ok(code) = fs::read_to_string(path) {
            if let Some(ast) = parse::parse_program(code) {
                if cli.debug {
                    println!("{ast:?}")
                }
                engine.run_program(ast);
            }
        } else {
            eprintln!("Error! the file can't be opened")
        }
    } else {
        repl(cli.debug);
    }
}

fn repl(debug: bool) {
    println!("idol {}", consts::VERSION);
    let mut engine = engine::Engine::new();
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        match rl.readline("> ") {
            Ok(code) => {
                let code = code.trim().to_string();
                if code.is_empty() {
                    continue;
                } else if code == ":q" {
                    break;
                }

                rl.add_history_entry(&code).unwrap_or_default();
                if let Some(ast) = parse::parse_program(code) {
                    if debug {
                        println!("AST = {ast:?}")
                    }
                    if let Some(result) = engine.run_program(ast) {
                        if debug {
                            println!("Result = {:?}", result);
                        } else {
                            if let type_enum::Type::Null = result {
                            } else {
                                println!("{}", result.get_symbol());
                            }
                        }
                    } else {
                        println!("Fault")
                    }
                } else {
                    println!("Error");
                }
            }
            Err(err) => println!("{err:?}"),
        }
    }
}






