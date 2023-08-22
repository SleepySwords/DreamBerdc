use std::{iter::Map, collections::HashMap};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    types::BasicMetadataTypeEnum, values::IntValue,
};
use itertools::Itertools;

use crate::ast::{FunctionStatement, Statement, Expression};

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl Compiler<'_> {
    pub fn build_function(&self, function: FunctionStatement) {
        let i32_type = self.context.i32_type();
        let types = function
            .prototype
            .arguments
            .iter()
            .map(|(_, t)| i32_type.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let fn_type = self.context.void_type().fn_type(types.as_slice(), false);
        let fn_val = self
            .module
            .add_function(&function.prototype.name, fn_type, None);

        let entry_basic_box = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry_basic_box);

        let symbol_table = function
            .prototype
            .arguments
            .into_iter()
            .enumerate()
            .map(|(index, (name, _))| (name, fn_val.get_nth_param(index as u32).unwrap().into_int_value()))
            .collect();

        for statement in function.body {
            self.build_statement(statement, &symbol_table);
        }
    }

    fn build_expression<'ctx>(&'ctx self, expression: Expression, symbol_table: &HashMap<String, IntValue<'ctx>>) -> IntValue {
        match expression {
            Expression::BinaryExpression(expr) => {
                let lhs = self.build_expression(*expr.lhs, symbol_table);
                let rhs = self.build_expression(*expr.rhs, symbol_table);
                match expr.operation {
                    crate::ast::Operation::Add => self.builder.build_int_add(lhs, rhs, "add"),
                    crate::ast::Operation::Subtract => self.builder.build_int_sub(lhs, rhs, "sub"),
                    crate::ast::Operation::Multiply => self.builder.build_int_mul(lhs, rhs, "mul"),
                    crate::ast::Operation::Divide => self.builder.build_int_signed_div(lhs, rhs, "div"),
                }
            },
            Expression::CallExpression(_) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::LiteralValue(_) => todo!(),
            Expression::Identifier(id) => {
                if symbol_table.contains_key(&id) {
                    return symbol_table[&id];
                } else {
                    let i32_type = self.context.i32_type();
                    i32_type.const_int(id.parse().expect("Invalid constant"), false)
                }
            },
            Expression::Unkown => todo!(),
        }
    }

    pub fn build_statement<'ctx>(&'ctx self, statement: Statement, symbol_table: &HashMap<String, IntValue<'ctx>>) {
        match statement {
            Statement::Declaration(_) => {
                todo!("Implement declaration")
            },
            Statement::Return(ret) => {
                let value = self.build_expression(ret.return_value, symbol_table);
                self.builder.build_return(Some(&value));
            },
            Statement::Function(function) => {
                self.build_function(*function);
            },
            Statement::Expression(_) => todo!(),
        }
    }

    pub fn compile(&self) {
        let execution_engine = self
            .module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        unsafe {
            type Main = unsafe extern "C" fn(i32, i32) -> i32;
            let main: JitFunction<Main> = execution_engine.get_function("main").unwrap();
            println!("Return code: {}", main.call(10, 3));
        }
        return
    }
}
