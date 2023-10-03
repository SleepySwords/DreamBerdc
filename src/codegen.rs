use std::{path::Path, thread::panicking};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine},
    types::BasicMetadataTypeEnum,
    values::BasicValueEnum,
    IntPredicate, OptimizationLevel,
};
use itertools::Itertools;

use crate::{
    ast::{
        Call, Declaration, Expression, ForStatement, Function, IfStatement, Operation, Statement,
    },
    symboltable::SymbolTable,
};

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub symbol_table: SymbolTable<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn build_function(&mut self, function: Function) {
        let i32_type = self.context.i32_type();
        let types = function
            .prototype
            .arguments
            .iter()
            .map(|(_, _t)| i32_type.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        // FIXME: No good forced i32 return types for now.
        let fn_type =
            function
                .prototype
                .return_type
                .function(self.context, types.as_slice(), false);
        let fn_val = self
            .module
            .add_function(&function.prototype.name, fn_type, None);

        let entry_basic_box = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry_basic_box);

        self.symbol_table.push_scope();

        for (index, (name, _)) in function.prototype.arguments.into_iter().enumerate() {
            self.symbol_table.store_value(
                name,
                fn_val.get_nth_param(index as u32).unwrap().into_int_value(),
            )
        }

        for statement in function.body {
            self.build_statement(statement);
        }
        // FIX: catch all return...
        self.builder.build_return(None).expect("Build failed");

        self.symbol_table.pop_scope()
    }

    fn build_expression(&mut self, expression: Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::Binary {
                lhs,
                operation,
                rhs,
            } => {
                let lhs = self.build_expression(*lhs);
                let rhs = self.build_expression(*rhs);
                if !lhs.is_int_value() || !rhs.is_int_value() {
                    panic!("Invalid int value")
                }
                let lhs = lhs.into_int_value();
                let rhs = rhs.into_int_value();
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
                .into()
            }
            Expression::Call(call) => self.build_call(call),
            Expression::Assignment(assignment) => {
                let ptr = self
                    .symbol_table
                    .fetch_variable_ptr(&assignment.lhs)
                    .unwrap();
                let expression = self.build_expression(*assignment.rhs);
                self.builder
                    .build_store(ptr, expression)
                    .expect("Build failed");
                self.context.i32_type().const_zero().into()
            }
            Expression::LiteralValue(_) => todo!(),
            Expression::Identifier(id) => {
                if let Some(ptr) = self.symbol_table.fetch_variable_ptr(&id) {
                    let value = self.builder.build_load(self.context.i32_type(), ptr, &id);
                    value.unwrap().into()
                } else if let Some(value) = self.symbol_table.fetch_value(&id) {
                    value.into()
                } else {
                    let i32_type = self.context.i32_type();
                    i32_type
                        .const_int(
                            id.parse()
                                .unwrap_or_else(|_| panic!("Invalid constant: {}", id)),
                            false,
                        )
                        .into()
                }
            }
            Expression::Unkown => todo!(),
        }
    }

    // FIX: ensure that basic blocks have stuff in them.
    pub fn build_if(&mut self, if_statement: IfStatement) {
        // let condition =
        //     self.build_expression(if_statement.boolean_op, symbol_table, ptr_symbol_table);
        self.symbol_table.push_scope();
        let (lhs, rhs, operation) = match if_statement.boolean_op {
            Expression::Binary {
                lhs,
                operation,
                rhs,
            } => {
                if let Operation::Equal | Operation::Greater | Operation::Less = operation {
                    let lhs_value = self.build_expression(*lhs).into_int_value();
                    let rhs_value = self.build_expression(*rhs).into_int_value();
                    match operation {
                        crate::ast::Operation::Equal => (lhs_value, rhs_value, IntPredicate::EQ),
                        crate::ast::Operation::Greater => (lhs_value, rhs_value, IntPredicate::SGT),
                        crate::ast::Operation::Less => (lhs_value, rhs_value, IntPredicate::SLT),
                        _ => panic!("Impossible"),
                    }
                } else {
                    (
                        self.build_expression(Expression::Binary {
                            lhs,
                            operation,
                            rhs,
                        })
                        .into_int_value(),
                        self.context.i32_type().const_zero(),
                        IntPredicate::NE,
                    )
                }
            }
            expr => (
                self.build_expression(expr).into_int_value(),
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
            self.build_statement(statement);
        }
        self.builder
            .build_unconditional_branch(merge_bb)
            .expect("Build failed");
        self.builder.position_at_end(merge_bb);
        self.symbol_table.pop_scope();
    }

    pub fn build_for(&mut self, for_statement: ForStatement) {
        let current_function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        if let Statement::Declaration(decl) = for_statement.initialiser {
            self.symbol_table.push_scope();
            let Declaration {
                mutable: _,
                lhs,
                rhs,
            } = *decl;

            let variable = self
                .builder
                .build_alloca(self.context.i32_type(), &lhs)
                .expect("Build failed");

            self.symbol_table.store_variable_ptr(lhs, variable);

            let initial_expression = self.build_expression(rhs);
            self.builder
                .build_store(variable, initial_expression)
                .expect("Build failed");

            let loop_bb = self.context.append_basic_block(current_function, "loop");
            self.builder
                .build_unconditional_branch(loop_bb)
                .expect("Build failed");
            self.builder.position_at_end(loop_bb);

            // May shadow, but too lazy

            for statement in for_statement.body.unwrap() {
                self.build_statement(statement)
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
                } => {
                    if let Operation::Equal | Operation::Greater | Operation::Less = operation {
                        let lhs_value = self.build_expression(*lhs).into_int_value();
                        let rhs_value = self.build_expression(*rhs).into_int_value();
                        match operation {
                            crate::ast::Operation::Equal => {
                                (lhs_value, rhs_value, IntPredicate::EQ)
                            }
                            crate::ast::Operation::Greater => {
                                (lhs_value, rhs_value, IntPredicate::SGT)
                            }
                            crate::ast::Operation::Less => {
                                (lhs_value, rhs_value, IntPredicate::SLT)
                            }
                            _ => panic!("Impossible"),
                        }
                    } else {
                        (
                            self.build_expression(Expression::Binary {
                                lhs,
                                operation,
                                rhs,
                            })
                            .into_int_value(),
                            self.context.i32_type().const_zero(),
                            IntPredicate::NE,
                        )
                    }
                }
                expr => (
                    self.build_expression(expr).into_int_value(),
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
            self.symbol_table.pop_scope();
        } else {
            panic!("Expected declaration found {:?}", for_statement.initialiser)
        }
    }

    pub fn build_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Declaration(declaration) => {
                let variable = self
                    .builder
                    .build_alloca(self.context.i32_type(), &declaration.lhs)
                    .expect("Build failed");
                let rhs = self.build_expression(declaration.rhs);
                self.builder
                    .build_store(variable, rhs)
                    .expect("Build failed");
                self.symbol_table
                    .store_variable_ptr(declaration.lhs, variable)
            }
            Statement::Return { return_value } => {
                let value = self.build_expression(*return_value);
                self.builder
                    .build_return(Some(&value))
                    .expect("Build failed");
            }
            Statement::Function(function) => {
                self.build_function(*function);
            }
            Statement::Expression(expr) => {
                self.build_expression(expr);
            }
            Statement::If(if_statement) => self.build_if(*if_statement),
            Statement::For(for_statement) => self.build_for(*for_statement),
        }
    }

    pub fn build_call(&mut self, call: Call) -> BasicValueEnum<'ctx> {
        if let Some(function) = self.module.get_function(&call.callee) {
            if function.count_params() != call.arguments.len() as u32 {
                panic!("Not enough arguments")
            }

            let args = call
                .arguments
                .iter()
                .map(|f| self.build_expression(f.clone()).into())
                .collect_vec();
            let value = self
                .builder
                .build_call(function, args.as_slice(), "calltmp");
            value
                .expect("Build failed")
                .try_as_basic_value()
                .left_or(self.context.i32_type().const_int(0, false).into())
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
