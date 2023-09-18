use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;
use std::collections::HashMap;

use clap::Parser;
use inkwell::OptimizationLevel;
use inkwell::context::Context;

use crate::args::Args;
use crate::{
    codegen::Compiler,
    lexer::{tokenize, Token},
    parser::Parser as CodeParser,
};

mod ast;
mod codegen;
mod lexer;
mod parser;
mod utils;
pub mod args;

fn main() -> Result<(), Box<dyn Error>> {
    // FIX: bangs are currently not recongnisd
    // could lead to `var var test = tes print("AWEf")`
    // being interperted as `var var test = tes!print("AWEF")`
    // Rather than poducing a syntax error.
    //
    // Order function defintion matters for this!!
    //
    // for (var var i = 0; i > b; i++) {
    //     i + a!
    // }
    //
    // FIX: potential error when storing a boolean as it stores with alignment of 1
    // and fetches with alignment of 4
    let args = Args::parse();
    let optimisation = match args.optimisation {
        args::Optimisation::None => OptimizationLevel::None,
        args::Optimisation::Less => OptimizationLevel::Less,
        args::Optimisation::Default => OptimizationLevel::Default,
        args::Optimisation::Aggresive => OptimizationLevel::Aggressive,
    };
    let file = read_to_string(args.input)?;
    let mut tokens = file // return add(23, add(43, 1)) + add(23, add(43, 1))!
        .chars()
        .peekable();

    let ts = tokenize(&mut tokens);

    println!("{:?}", ts);

    // let mut tokens = ts.into_iter().peekable();
    let mut statements = Vec::new();

    let mut parser = CodeParser { tokens: ts, pos: 0 };

    while !parser.peek().is_some_and(|f| *f == Token::Eof) {
        statements.push(parser.parse_function());
    }
    println!("{:?}", statements);
    let context = Context::create();
    let module = context.create_module("global");
    let builder = context.create_builder();

    // Add functions
    let putchar_fn_type = context.i32_type().fn_type(&[context.i32_type().into()], false);
    module.add_function("putchar", putchar_fn_type, Some(inkwell::module::Linkage::External));

    let putchar_fn_type = context.i32_type().fn_type(&[], false);
    module.add_function("getchar", putchar_fn_type, Some(inkwell::module::Linkage::External));

    let compiler = Compiler {
        context: &context,
        module,
        builder,
    };

    let mut hashmap = HashMap::new();
    let mut ptr_hashmap = HashMap::new();
    for statement in &statements {
        compiler.build_statement(statement.clone(), &mut hashmap, &mut ptr_hashmap);
    }
    match args.mode {
        args::Mode::Jit => {
            compiler.run_jit(optimisation);
        },
        args::Mode::LLVMIR => {
            compiler.write_llvm_ir(Path::new(&args.output.unwrap_or(String::from("output.ir"))));
        },
        args::Mode::Object => {
            compiler.compile_to_obj(Path::new(&args.output.unwrap_or(String::from("output.o"))), optimisation);
        },
    }
    Ok(())
}
