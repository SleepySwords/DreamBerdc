use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;
use std::process::exit;

use ast::Statement;
use clap::Parser;
use colored::Colorize;
use inkwell::context::Context;
use inkwell::OptimizationLevel;
use itertools::Itertools;

use crate::args::{Args, Mode};
use crate::ast::StatementKind;
use crate::lexer::Lexer;
use crate::symboltable::SymbolTable;
use crate::{codegen::CodeGen, lexer::TokenKind, parser::Parser as CodeParser};

pub mod args;
mod ast;
mod codegen;
pub mod compile_error;
mod lexer;
#[macro_use]
pub mod macros;
mod parser;
mod symboltable;
mod types;
mod utils;

#[cfg(test)]
mod test;
mod traits;

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let optimisation = match args.optimisation {
        args::Optimisation::None => OptimizationLevel::None,
        args::Optimisation::Less => OptimizationLevel::Less,
        args::Optimisation::Default => OptimizationLevel::Default,
        args::Optimisation::Aggresive => OptimizationLevel::Aggressive,
    };
    let file = read_to_string(&args.input)?;
    let data = file.chars().collect_vec();

    let statements = tokenise_and_parse(data, args.log_info);

    let file = read_to_string("std/prelude.db")?;
    let data = file.chars().collect_vec();
    let prelude_statements = tokenise_and_parse(data, false);

    // Step 3: Codegen
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
    if args.mode == Mode::Object {
        compiler.create_debug_symbols(Path::new(&args.input));
    }

    build_statements(&mut compiler, prelude_statements);
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
        if args.mode == args::Mode::LLVMIR {
            compiler.write_llvm_ir(Path::new(&args.output.unwrap_or(String::from("output.ir"))));
        }
        exit(1);
    }

    println!("{}", "Compiled succesfully".green().bold());

    match args.mode {
        args::Mode::Jit => {
            compiler.run_jit(optimisation);
        }
        args::Mode::LLVMIR => {
            compiler.write_llvm_ir(Path::new(&args.output.unwrap_or(String::from("output.ir"))));
        }
        args::Mode::Object => {
            compiler.compile_to_obj(
                Path::new(&args.output.unwrap_or(String::from("output.o"))),
                optimisation,
            );
        }
    }
    Ok(())
}

pub fn tokenise_and_parse(data: Vec<char>, log: bool) -> Vec<Statement> {
    // Step 1: Tokenise
    let mut lexer = Lexer::new(data);
    let tokens = lexer.tokenise();

    if log {
        println!("{:#?}", tokens);
    }

    // Step 2: Parse
    let mut parser = CodeParser { tokens, pos: 0 };

    let mut statements = Vec::new();
    while !parser.peek().is_some_and(|f| *f == TokenKind::Eof) {
        let function = match parser.parse_top_level_declaration() {
            Ok(func) => func,
            Err(e) => {
                println!("{}", e);
                exit(1);
            }
        };
        statements.push(function);
    }

    if log {
        println!("{:#?}", statements);
    }

    statements
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
