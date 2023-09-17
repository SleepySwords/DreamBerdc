use std::env;
use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;
use std::{collections::HashMap, fs::File};

use inkwell::context::Context;

use crate::{
    codegen::Compiler,
    lexer::{tokenize, Token},
    parser::Parser,
};

mod ast;
mod codegen;
mod lexer;
mod parser;
mod utils;

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
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
    let file = read_to_string(args[1].clone())?;
    let mut tokens = file // return add(23, add(43, 1)) + add(23, add(43, 1))!
        .chars()
        .peekable();

    let ts = tokenize(&mut tokens);

    println!("{:?}", ts);

    // let mut tokens = ts.into_iter().peekable();
    let mut statements = Vec::new();

    let mut parser = Parser { tokens: ts, pos: 0 };

    while !parser.peek().is_some_and(|f| *f == Token::Eof) {
        statements.push(parser.parse_function());
    }
    println!("{:?}", statements);
    let context = Context::create();
    let module = context.create_module("global");
    let builder = context.create_builder();
    let putchar_fn_type = context.i32_type().fn_type(&[context.i32_type().into()], false);
    module.add_function("putchar", putchar_fn_type, Some(inkwell::module::Linkage::External));

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
    compiler.compile_to_obj(&Path::new(&args[2]));
    Ok(())
}
