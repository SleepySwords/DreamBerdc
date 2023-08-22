use std::{collections::HashMap, iter::Peekable};

use inkwell::context::Context;

use crate::{codegen::Compiler, lexer::tokenize, parser::parse_function};

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
function main(a: int, b: int) => {
    return 2 * 129 + 2 / 2!
}
"
    .chars()
    .into_iter()
    .peekable();

    let ts = tokenize(&mut tokens);

    println!("{:?}", ts);

    // let exp = parse_expression(&mut ts.into_iter().peekable());
    // let exp = parse_assignment(&mut ts.into_iter().peekable());
    let exp = parse_function(&mut ts.into_iter().peekable());
    println!("{:?}", exp);
    let context = Context::create();
    let module = context.create_module("addition");
    let builder = context.create_builder();
    let compiler = Compiler {
        context: &context,
        module,
        builder,
    };
    let hashmap = HashMap::new();
    compiler.build_statement(exp, &hashmap);
    compiler.compile();
}
