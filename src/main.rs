use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;

use clap::Parser;
use inkwell::context::Context;
use inkwell::OptimizationLevel;
use itertools::Itertools;

use crate::args::Args;
use crate::lexer::Lexer;
use crate::symboltable::SymbolTable;
use crate::{codegen::Compiler, lexer::TokenKind, parser::Parser as CodeParser};

pub mod args;
mod ast;
mod codegen;
pub mod compile_error;
mod lexer;
mod parser;
mod symboltable;
mod types;
mod utils;

#[cfg(test)]
mod test;

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let optimisation = match args.optimisation {
        args::Optimisation::None => OptimizationLevel::None,
        args::Optimisation::Less => OptimizationLevel::Less,
        args::Optimisation::Default => OptimizationLevel::Default,
        args::Optimisation::Aggresive => OptimizationLevel::Aggressive,
    };
    let file = read_to_string(args.input)?;
    let data = file.chars().collect_vec();

    // Step 2: Tokenise
    let mut lexer = Lexer::new(data);

    let tokens = lexer.tokenise();
    println!("{:#?}", tokens);

    // Step 2: Parse
    let mut parser = CodeParser {
        tokens,
        pos: 0,
    };

    let mut statements = Vec::new();
    while !parser.peek().is_some_and(|f| *f == TokenKind::Eof) {
        let function = match parser.parse_function() {
            Ok(func) => func,
            Err(e) => {
                println!("{}", e);
                return Ok(());
            }
        };
        statements.push(function);
    }
    println!("{:#?}", statements);

    // Step 3: Codegen
    let context = Context::create();
    let module = context.create_module("global");
    let builder = context.create_builder();

    // Add functions
    let putchar_fn_type = context
        .i32_type()
        .fn_type(&[context.i32_type().into()], false);

    module.add_function(
        "putchar",
        putchar_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    let put_fn_type = context
        .i32_type()
        .fn_type(&[context.i32_type().into()], false);

    module.add_function(
        "puts",
        put_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    let getchar_fn_type = context.i32_type().fn_type(&[], false);
    module.add_function(
        "getchar",
        getchar_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    let getchar_fn_type = context.i32_type().fn_type(&[context.i32_type().into()], false);
    module.add_function(
        "gets",
        getchar_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    let mut compiler = Compiler {
        context: &context,
        module,
        builder,
        symbol_table: SymbolTable::default(),
    };

    for statement in &statements {
        compiler.build_statement(statement.clone());
    }
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
