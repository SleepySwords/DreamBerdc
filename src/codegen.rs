use std::collections::HashMap;

use inkwell::{
    builder::{Builder},
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::IntValue,
};
use itertools::Itertools;

use crate::ast::{Call, Expression, Function, IfStatement, Statement};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl Compiler<'_> {
    pub fn build_function(&self, function: Function) {
        let i32_type = self.context.i32_type();
        let types = function
            .prototype
            .arguments
            .iter()
            .map(|(_, _t)| i32_type.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        // FIXME: No good forced i32 return types for now.
        let fn_type = self.context.i32_type().fn_type(types.as_slice(), false);
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
            .map(|(index, (name, _))| {
                (
                    name,
                    fn_val.get_nth_param(index as u32).unwrap().into_int_value(),
                )
            })
            .collect();

        for statement in function.body {
            self.build_statement(statement, &symbol_table);
        }
    }

    fn build_expression<'ctx>(
        &'ctx self,
        expression: Expression,
        symbol_table: &HashMap<String, IntValue<'ctx>>,
    ) -> IntValue<'ctx> {
        match expression {
            Expression::Binary {
                lhs,
                operation,
                rhs,
            } => {
                let lhs = self.build_expression(*lhs, symbol_table);
                let rhs = self.build_expression(*rhs, symbol_table);
                match operation {
                    crate::ast::Operation::Add => self.builder.build_int_add(lhs, rhs, "add"),
                    crate::ast::Operation::Subtract => self.builder.build_int_sub(lhs, rhs, "sub"),
                    crate::ast::Operation::Multiply => self.builder.build_int_mul(lhs, rhs, "mul"),
                    crate::ast::Operation::Divide => {
                        self.builder.build_int_signed_div(lhs, rhs, "div")
                    }
                    _ => panic!("aefj"),
                }
            }
            Expression::Call(call) => self.build_call(call, symbol_table),
            Expression::Assignment(_) => todo!(),
            Expression::LiteralValue(_) => todo!(),
            Expression::Identifier(id) => {
                if symbol_table.contains_key(&id) {
                    symbol_table[&id]
                } else {
                    let i32_type = self.context.i32_type();
                    i32_type.const_int(id.parse().expect("Invalid constant"), false)
                }
            }
            Expression::Unkown => todo!(),
        }
    }

    // FIX: ensure that basic blocks have stuff in them.
    pub fn build_if<'ctx>(
        &'ctx self,
        if_statement: IfStatement,
        symbol_table: &HashMap<String, IntValue<'ctx>>,
    ) {
        let condition = self.build_expression(if_statement.boolean_op, symbol_table);
        let condition = self.builder.build_int_compare(
            inkwell::IntPredicate::NE,
            condition,
            self.context.i32_type().const_zero(),
            "ifcond",
        );

        let current_function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let then_bb = self.context.append_basic_block(current_function, "then");
        // let else_bb = self.context.append_basic_block(current_function, "else");
        let merge_bb = self.context.append_basic_block(current_function, "ifcont");

        self.builder
            .build_conditional_branch(condition, then_bb, merge_bb);

        self.builder.position_at_end(then_bb);
        for statement in if_statement.then_statements {
            self.build_statement(statement, symbol_table);
        }
        self.builder.build_unconditional_branch(merge_bb);
        self.builder.position_at_end(merge_bb);
        // then_bb = self.builder.get_insert_block();

        // let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");
        // phi.add_incoming(incoming)

        // let entry_basic_box = self.context.append_basic_block(fn_val, "entry");
        // self.builder.position_at_end(entry_basic_box);
    }

    pub fn build_statement<'ctx>(
        &'ctx self,
        statement: Statement,
        symbol_table: &HashMap<String, IntValue<'ctx>>,
    ) {
        match statement {
            Statement::Declaration(_) => {
                todo!("Implement declaration")
            }
            Statement::Return { return_value } => {
                let value = self.build_expression(*return_value, symbol_table);
                self.builder.build_return(Some(&value));
            }
            Statement::Function(function) => {
                self.build_function(*function);
            }
            Statement::Expression(_) => todo!(),
            Statement::If(if_statement) => self.build_if(*if_statement, symbol_table),
        }
    }

    pub fn build_call<'ctx>(
        &'ctx self,
        call: Call,
        symbol_table: &HashMap<String, IntValue<'ctx>>,
    ) -> IntValue<'ctx> {
        if let Some(function) = self.module.get_function(&call.callee) {
            if function.count_params() != call.arguments.len() as u32 {
                panic!("Not enough arguments")
            }

            let value = self.builder.build_call(
                function,
                call.arguments
                    .iter()
                    .map(|f| self.build_expression(f.clone(), symbol_table).into())
                    .collect_vec()
                    .as_slice(),
                "calltmp",
            );
            value.try_as_basic_value().unwrap_left().into_int_value()
        } else {
            panic!("Function not defined")
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
    }
}
