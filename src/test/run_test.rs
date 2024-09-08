use std::{error::Error, fs, process::exit};

use colored::Colorize;
use inkwell::{context::Context, execution_engine::JitFunction};
use itertools::Itertools;

use crate::{
    ast::{Statement, StatementKind},
    codegen::{self, CodeGen},
    lexer::{Lexer, TokenKind},
    parser::Parser,
    symboltable::SymbolTable,
};

#[test]
fn test() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(String::from("./test/source/") + "test" + ".db")?;
    let mut lexer = Lexer::new(contents.chars().collect_vec());
    let tokens = lexer.tokenise();
    let mut parser = Parser { tokens, pos: 0 };

    let mut statements = Vec::new();
    while !parser.peek().is_some_and(|f| *f == TokenKind::Eof) {
        let function = match parser.parse_top_level_declaration() {
            Ok(func) => func,
            Err(e) => {
                println!("{}", e);
                return Ok(());
            }
        };
        statements.push(function);
    }

    let context = Context::create();
    let module = context.create_module("global");
    let builder = context.create_builder();

    let symboltable = SymbolTable::new();

    let mut compiler = CodeGen {
        context: &context,
        module,
        builder,
        symbol_table: symboltable,
        debug_info: None,
    };

    build_statements(&mut compiler, statements);

    compiler.finalise();
    for fun in compiler.module.get_functions() {
        println!(
            "{}: {}",
            "Verifying the function".bright_purple(),
            fun.get_name().to_str()?
        );
        if !fun.verify(true) {
            println!();
        }
    }

    if let Err(str) = compiler.module.verify() {
        println!(
            "{}:\n{}",
            "Errors from LLVM while verifying".red(),
            str.to_string().replace("\\n", "\n")
        );
    }

    println!("{}", "Compiled succesfully".green().bold());

    let execution_engine = compiler
        .module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    unsafe {
        type Main = unsafe extern "C" fn() -> u32;
        let main: JitFunction<Main> = execution_engine.get_function("main").unwrap();
        println!("Return code: {}", main.call());
        // println!("Return code: {}", main.call(10, 3));
    }
    Ok(())
}

pub fn build_statements(codegen: &mut CodeGen, statements: Vec<Statement>) {
    // Add the function declarations first
    for statement in &statements {
        if let StatementKind::Function(fun) = &statement.kind {
            codegen.build_function_declaration(&fun.prototype);
        }
        if let StatementKind::Extern(prototype) = &statement.kind {
            codegen.build_extern(prototype);
        }
    }

    for statement in &statements {
        match codegen.build_statement(statement.clone()) {
            Ok(_) => {}
            Err(e) => {
                println!("{}", e);
                exit(1);
            }
        }
    }
}
