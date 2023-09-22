use std::{path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine},
    types::BasicMetadataTypeEnum,
    values::IntValue,
    IntPredicate, OptimizationLevel,
};
use itertools::Itertools;

use crate::{
    ast::{Call, Declaration, Expression, ForStatement, Function, IfStatement, Statement},
    symboltable::SymbolTable,
};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl Compiler<'_> {
    pub fn build_function<'ctx>(&'ctx self, symbol_table: &mut SymbolTable<'ctx>, function: Function) {
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

        symbol_table.push_scope();

        for (index, (name, _)) in function.prototype.arguments.into_iter().enumerate() {
            symbol_table.store_variable_ptr(
                name,
                fn_val
                    .get_nth_param(index as u32)
                    .unwrap()
                    .into_pointer_value(),
            )
        }

        for statement in function.body {
            self.build_statement(symbol_table, statement);
        }
        // FIX: catch all return...
        self.builder.build_return(None).expect("Build failed");
    }

    fn build_expression<'ctx>(&'ctx self, symbol_table: &mut SymbolTable<'ctx>, expression: Expression) -> IntValue<'ctx> {
        match expression {
            Expression::Binary {
                lhs,
                operation,
                rhs,
            } => {
                let lhs = self.build_expression(symbol_table, *lhs);
                let rhs = self.build_expression(symbol_table, *rhs);
                match operation {
                    crate::ast::Operation::Add => self
                        .builder
                        .build_int_add(lhs, rhs, "add")
                        .expect("Build failed"),
                    crate::ast::Operation::Subtract => self
                        .builder
                        .build_int_sub(lhs, rhs, "sub")
                        .expect("Build failed"),
                    crate::ast::Operation::Multiply => self
                        .builder
                        .build_int_mul(lhs, rhs, "mul")
                        .expect("Build failed"),
                    crate::ast::Operation::Divide => self
                        .builder
                        .build_int_signed_div(lhs, rhs, "div")
                        .expect("Build failed"),
                    crate::ast::Operation::Less => self
                        .builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "cond")
                        .expect("Build failed"),
                    crate::ast::Operation::Greater => self
                        .builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "cond")
                        .expect("Build failed"),
                    crate::ast::Operation::Equal => self
                        .builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "cond")
                        .expect("Build failed"),
                    _ => panic!("aefj"),
                }
            }
            Expression::Call(call) => self.build_call(symbol_table, call),
            Expression::Assignment(assignment) => {
                let ptr = symbol_table
                    .fetch_variable_ptr(&assignment.lhs)
                    .unwrap();
                self.builder
                    .build_store(ptr, self.build_expression(symbol_table, *assignment.rhs))
                    .expect("Build failed");
                self.context.i32_type().const_zero()
            }
            Expression::LiteralValue(_) => todo!(),
            Expression::Identifier(id) => {
                if let Some(ptr) = symbol_table.fetch_variable_ptr(&id) {
                    let value = self.builder.build_load(self.context.i32_type(), ptr, &id);
                    value.unwrap().into_int_value()
                } else {
                    let i32_type = self.context.i32_type();
                    i32_type.const_int(
                        id.parse()
                            .unwrap_or_else(|_| panic!("Invalid constant: {}", id)),
                        false,
                    )
                }
            }
            Expression::Unkown => todo!(),
        }
    }

    // FIX: ensure that basic blocks have stuff in them.
    pub fn build_if<'ctx>(&'ctx self, symbol_table: &mut SymbolTable<'ctx>, if_statement: IfStatement) {
        // let condition =
        //     self.build_expression(if_statement.boolean_op, symbol_table, ptr_symbol_table);
        let (lhs, rhs, operation) = match if_statement.boolean_op {
            Expression::Binary {
                lhs,
                operation,
                rhs,
            } => match operation {
                crate::ast::Operation::Equal => (
                    self.build_expression(symbol_table, *lhs),
                    self.build_expression(symbol_table, *rhs),
                    IntPredicate::EQ,
                ),
                crate::ast::Operation::Greater => (
                    self.build_expression(symbol_table, *lhs),
                    self.build_expression(symbol_table, *rhs),
                    IntPredicate::SGT,
                ),
                crate::ast::Operation::Less => (
                    self.build_expression(symbol_table, *lhs),
                    self.build_expression(symbol_table, *rhs),
                    IntPredicate::SLT,
                ),
                op => (
                    self.build_expression(symbol_table, Expression::Binary {
                        lhs,
                        operation: op,
                        rhs,
                    }),
                    self.context.i32_type().const_zero(),
                    IntPredicate::NE,
                ),
            },
            expr => (
                self.build_expression(symbol_table, expr),
                self.context.i32_type().const_zero(),
                IntPredicate::NE,
            ),
        };
        let condition = self
            .builder
            .build_int_compare(operation, lhs, rhs, "ifcond")
            .expect("Build failed");

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
            .build_conditional_branch(condition, then_bb, merge_bb)
            .expect("Build failed");

        self.builder.position_at_end(then_bb);
        for statement in if_statement.then_statements {
            self.build_statement(symbol_table, statement);
        }
        self.builder
            .build_unconditional_branch(merge_bb)
            .expect("Build failed");
        self.builder.position_at_end(merge_bb);
    }

    pub fn build_for<'ctx>(&'ctx self, symbol_table: &mut SymbolTable<'ctx>, for_statement: ForStatement) {
        let current_function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        if let Statement::Declaration(decl) = for_statement.initialiser {
            let Declaration {
                mutable: _,
                lhs,
                rhs,
            } = *decl;

            let variable = self
                .builder
                .build_alloca(self.context.i32_type(), &lhs)
                .expect("Build failed");

            symbol_table.store_variable_ptr(lhs, variable);

            self.builder
                .build_store(variable, self.build_expression(symbol_table, rhs))
                .expect("Build failed");

            let loop_bb = self.context.append_basic_block(current_function, "loop");
            self.builder
                .build_unconditional_branch(loop_bb)
                .expect("Build failed");
            self.builder.position_at_end(loop_bb);

            // May shadow, but too lazy

            for statement in for_statement.body.unwrap() {
                self.build_statement(symbol_table, statement)
            }

            let step_value = self.context.i32_type().const_int(1, false);
            let next_var = self
                .builder
                .build_int_add(
                    self.builder
                        .build_load(self.context.i32_type(), variable, "var")
                        .expect("Failed")
                        .into_int_value(),
                    step_value,
                    "nextvar",
                )
                .expect("Build failed");

            self.builder
                .build_store(variable, next_var)
                .expect("Build failed");

            let (lhs, rhs, operation) = match for_statement.condition {
                Expression::Binary {
                    lhs,
                    operation,
                    rhs,
                } => match operation {
                    crate::ast::Operation::Equal => (
                        self.build_expression(symbol_table, *lhs),
                        self.build_expression(symbol_table, *rhs),
                        IntPredicate::EQ,
                    ),
                    crate::ast::Operation::Greater => (
                        self.build_expression(symbol_table, *lhs),
                        self.build_expression(symbol_table, *rhs),
                        IntPredicate::SGT,
                    ),
                    crate::ast::Operation::Less => (
                        self.build_expression(symbol_table, *lhs),
                        self.build_expression(symbol_table, *rhs),
                        IntPredicate::SLT,
                    ),
                    op => (
                        self.build_expression(symbol_table, Expression::Binary {
                            lhs,
                            operation: op,
                            rhs,
                        }),
                        self.context.i32_type().const_zero(),
                        IntPredicate::NE,
                    ),
                },
                expr => (
                    self.build_expression(symbol_table, expr),
                    self.context.i32_type().const_zero(),
                    IntPredicate::NE,
                ),
            };

            let end_cond = self
                .builder
                .build_int_compare(operation, lhs, rhs, "loopcond")
                .expect("Build failed");

            let after_bb = self
                .context
                .append_basic_block(current_function, "afterloop");

            self.builder
                .build_conditional_branch(end_cond, loop_bb, after_bb)
                .expect("Build failed");
            self.builder.position_at_end(after_bb);
        } else {
            panic!("What the flip")
        }
    }

    pub fn build_statement<'ctx>(&'ctx self, symbol_table: &mut SymbolTable<'ctx>, statement: Statement) {
        match statement {
            Statement::Declaration(declaration) => {
                let variable = self
                    .builder
                    .build_alloca(self.context.i32_type(), &declaration.lhs)
                    .expect("Build failed");
                self.builder
                    .build_store(variable, self.build_expression(symbol_table, declaration.rhs))
                    .expect("Build failed");
                symbol_table
                    .store_variable_ptr(declaration.lhs, variable)
            }
            Statement::Return { return_value } => {
                let value = self.build_expression(symbol_table, *return_value);
                self.builder
                    .build_return(Some(&value))
                    .expect("Build failed");
            }
            Statement::Function(function) => {
                self.build_function(symbol_table, *function);
            }
            Statement::Expression(expr) => {
                self.build_expression(symbol_table, expr);
            }
            Statement::If(if_statement) => self.build_if(symbol_table, *if_statement),
            Statement::For(for_statement) => self.build_for(symbol_table, *for_statement),
        }
    }

    pub fn build_call<'ctx>(&'ctx self, symbol_table: &mut SymbolTable<'ctx>, call: Call) -> IntValue<'ctx> {
        if let Some(function) = self.module.get_function(&call.callee) {
            if function.count_params() != call.arguments.len() as u32 {
                panic!("Not enough arguments")
            }

            let value = self.builder.build_call(
                function,
                call.arguments
                    .iter()
                    .map(|f| self.build_expression(symbol_table, f.clone()).into())
                    .collect_vec()
                    .as_slice(),
                "calltmp",
            );
            value
                .expect("Build failed")
                .try_as_basic_value()
                .unwrap_left()
                .into_int_value()
        } else {
            panic!("Function not defined")
        }
    }

    pub fn run_jit(&self, optimisation: OptimizationLevel) {
        let execution_engine = self
            .module
            .create_jit_execution_engine(optimisation)
            .unwrap();
        unsafe {
            type Main = unsafe extern "C" fn(i32, i32) -> i32;
            let main: JitFunction<Main> = execution_engine.get_function("main").unwrap();
            println!("Return code: {}", main.call(10, 3));
        }
    }

    pub fn write_llvm_ir(&self, path: &Path) {
        self.module.print_to_file(path).expect("Error");
    }

    pub fn compile_to_obj(&self, path: &Path, optimisation: OptimizationLevel) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target = Target::from_triple(&target_triple).expect("Error");
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                optimisation,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("Error");

        target_machine
            .write_to_file(&self.module, inkwell::targets::FileType::Object, path)
            .expect("Error");
    }
}
