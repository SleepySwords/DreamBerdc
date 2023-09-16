use std::collections::HashMap;

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

fn main() {
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
    let mut tokens = "
function max(a: int, b: int, c: int) => {
    var var d = 10!
    if (a < b) {
        d = 0!
    }
    if (a > b) {
        d = d + 1!
    }
    d = d +  1!
    return d!
}

function add(a: int, b: int) => {
    return a * b + a!
}

function main(a: int, b: int) => {
    return max(4, 12, 3)
}
"
    // return add(23, add(43, 1)) + add(23, add(43, 1))!
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
    compiler.compile();
}
