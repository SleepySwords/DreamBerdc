use std::{collections::HashMap, iter::Peekable};

use inkwell::context::Context;

use crate::{codegen::Compiler, lexer::{tokenize, Token}, parser::parse_function};

mod ast;
mod codegen;
mod lexer;
mod parser;
mod utils;

fn main() {
    // let mut tokens = "
    //     print(\"Hello world\", \"awefafwe\", \"aewf\", print(\"Awef\", zfeaf(dsg)))?
    // "
    // let mut tokens = "
    // var const efja = ajiwoe
    // "
    // let mut tokens = "
    // function test(a, b) => print(\"aweihfaefji\", ajfeoijawieof)!
    // "
    // FIX: bangs are currently not recongnisd
    // could lead to `var var test = tes print("AWEf")`
    // being interperted as `var var test = tes!print("AWEF")`
    // Rather than poducing a syntax error.
    let mut tokens = "
function add(a: int, b: int) => {
    return a * b + a!
}

function main(a: int, b: int) => {
    return add(23, add(43, 1))!
}
"
    .chars()
    .into_iter()
    .peekable();

    let ts = tokenize(&mut tokens);

    println!("{:?}", ts);

    // let exp = parse_expression(&mut ts.into_iter().peekable());
    // let exp = parse_assignment(&mut ts.into_iter().peekable());
    let mut tokens = ts.into_iter().peekable();
    let mut statements = Vec::new();
    while !tokens.peek().is_some_and(|f| *f == Token::EOF) {
        statements.push(parse_function(&mut tokens));
    }
    // println!("{:?}", exp);
    let context = Context::create();
    let module = context.create_module("global");
    let builder = context.create_builder();
    let compiler = Compiler {
        context: &context,
        module,
        builder,
    };
    let hashmap = HashMap::new();
    for statement in &statements {
        compiler.build_statement(statement.clone(), &hashmap);
    }
    compiler.compile();
}
