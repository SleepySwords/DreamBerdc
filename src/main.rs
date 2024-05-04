use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;
use std::process::exit;

use clap::Parser;
use colored::Colorize;
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::{AddressSpace, OptimizationLevel};
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

    // Step 1: Tokenise
    let mut lexer = Lexer::new(data);
    let tokens = lexer.tokenise();

    if args.log_info {
        println!("{:#?}", tokens);
    }

    // Step 2: Parse
    let mut parser = CodeParser { tokens, pos: 0 };

    let mut statements = Vec::new();
    while !parser.peek().is_some_and(|f| *f == TokenKind::Eof) {
        let function = match parser.parse_function() {
            Ok(func) => func,
            Err(e) => {
                println!("{}", e);
                exit(1);
            }
        };
        statements.push(function);
    }
    if args.log_info {
        println!("{:#?}", statements);
    }

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

    let put_fn_type = context.i32_type().as_basic_type_enum().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        false,
    );

    module.add_function(
        "puts",
        put_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    let printf_fn_type = context.i32_type().as_basic_type_enum().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        true,
    );

    module.add_function(
        "printf",
        printf_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    let getchar_fn_type = context.i32_type().fn_type(&[], false);
    module.add_function(
        "getchar",
        getchar_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    let getchar_fn_type = context
        .i32_type()
        .fn_type(&[context.i32_type().into()], false);
    module.add_function(
        "gets",
        getchar_fn_type,
        Some(inkwell::module::Linkage::External),
    );

    // FIXME: functions must be defined first before can be used.
    // Should be able to be done by just adding the function first, and then build it later.
    let mut compiler = CodeGen {
        context: &context,
        module,
        builder,
        symbol_table: SymbolTable::new(),
        debug_info: None,
    };
    if args.mode == Mode::Object {
        compiler.create_debug_symbols(Path::new(&args.input));
    }

    // Add the function declarations first
    for statement in &statements {
        if let StatementKind::Function(fun) = &statement.kind {
            compiler.build_function_declaration(fun);
        }
    }

    for statement in &statements {
        match compiler.build_statement(statement.clone()) {
            Ok(_) => {}
            Err(e) => {
                println!("{}", e);
                exit(1);
            }
        }
    }

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
