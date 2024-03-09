use colored::Colorize;
use inkwell::{types::BasicMetadataTypeEnum, values::FunctionValue, IntPredicate};

use crate::{
    ast::{Declaration, ForStatement, Function, IfStatement, Statement, StatementKind},
    compile_error::CompilerError,
    types::Type,
};

use super::{CodeGen, CompileInfo};

impl<'ctx> CodeGen<'ctx> {
    pub fn build_statement(&mut self, statement: Statement) -> Result<CompileInfo, CompilerError> {
        let statement_pos = statement.pos();
        match statement.kind {
            StatementKind::Declaration(declaration) => {
                let var_type = if let Some(t) = declaration.var_type {
                    t
                } else {
                    // FIXME: need declaration inference...
                    Type::Int
                };
                let basic_type_enum = if let Some(t) = var_type.basic_type_enum(self.context) {
                    t
                } else {
                    return Err(CompilerError::code_gen_error(
                        statement_pos,
                        "Cannot use the void type as a variable.",
                    ));
                };
                let variable = self
                    .builder
                    .build_alloca(basic_type_enum, &declaration.lhs)?;
                let rhs = self.build_expression(declaration.rhs)?;
                self.builder.build_store(variable, rhs)?;
                self.symbol_table.store_variable_ptr(
                    declaration.lhs,
                    variable,
                    var_type,
                    declaration.mutable,
                )
            }
            StatementKind::Return { return_value } => {
                if let Some(return_expression) = return_value {
                    let value = self.build_expression(return_expression)?;
                    self.builder.build_return(Some(&value))?;
                } else {
                    self.builder.build_return(None)?;
                }
                return Ok(CompileInfo {
                    terminator_instruction: true,
                });
            }
            StatementKind::Function(function) => {
                self.build_function(function)?;
            }
            StatementKind::Expression(expr) => {
                self.build_expression(expr)?;
            }
            StatementKind::If(if_statement) => self.build_if(if_statement, statement_pos)?,
            StatementKind::For(for_statement) => self.build_for(*for_statement)?,
        }
        Ok(CompileInfo {
            terminator_instruction: false,
        })
    }

    pub fn build_function_declaration(
        &mut self,
        function: &Function,
    ) -> Option<FunctionValue<'ctx>> {
        let types = function
            .prototype
            .arguments
            .iter()
            .map(|(_, t)| t.basic_metadata_enum(self.context))
            .collect::<Option<Vec<BasicMetadataTypeEnum>>>();

        let fn_type =
            function
                .prototype
                .return_type
                .function(self.context, types?.as_slice(), false);
        return Some(
            self.module
                .add_function(&function.prototype.name, fn_type?, None),
        );
    }

    /// Builds a function
    pub fn build_function(&mut self, function: Function) -> Result<(), CompilerError> {
        let fn_val = if let Some(fn_val) = self.module.get_function(&function.prototype.name) {
            fn_val
        } else if let Some(fun) = self.build_function_declaration(&function) {
            fun
        } else {
            return Err(CompilerError::code_gen_error(
                (0, 0),
                "Invalid return type!",
            ));
        };

        let entry_basic_box = self.context.append_basic_block(fn_val, "entry");

        self.builder.position_at_end(entry_basic_box);
        self.symbol_table.push_scope();

        for (index, (name, _)) in function.prototype.arguments.into_iter().enumerate() {
            self.symbol_table
                .store_value(name, fn_val.get_nth_param(index as u32).unwrap())
        }

        let mut build_ret = true;

        for statement in function.body {
            if self.build_statement(statement)?.terminator_instruction {
                build_ret = false;
                continue;
            }
        }

        // FIX: catch all return...
        // Should actually check if it returns something in all branches.

        if build_ret {
            self.builder.build_return(None)?;
        }

        self.symbol_table.pop_scope();

        println!(
            "{}: {}",
            "Verifying the function".bright_yellow(),
            function.prototype.name
        );
        if !fn_val.verify(true) {
            println!();
        }

        Ok(())
    }

    pub fn build_if(
        &mut self,
        if_statement: IfStatement,
        statement_pos: (usize, usize),
    ) -> Result<(), CompilerError> {
        self.symbol_table.push_scope();

        let value = self
            .build_expression(if_statement.boolean_op)?
            .into_int_value();

        // FIXME: need to not rely on int stuff
        let condition = self.builder.build_int_compare(
            IntPredicate::NE,
            value.get_type().const_zero(),
            value,
            "ifcond",
        )?;

        let current_function = self
            .builder
            .get_insert_block()
            .ok_or_else(|| {
                CompilerError::code_gen_error(statement_pos, "Cannot find insert block")
            })?
            .get_parent()
            .ok_or_else(|| {
                CompilerError::code_gen_error(statement_pos, "Cannot get parent")
            })?;

        let then_bb = self.context.append_basic_block(current_function, "then");
        let else_bb = self.context.append_basic_block(current_function, "else");
        let merge_bb = self.context.append_basic_block(current_function, "ifcont");

        self.builder
            .build_conditional_branch(condition, then_bb, else_bb)?;

        self.builder.position_at_end(then_bb);

        let mut then_terminated = false;
        for statement in if_statement.then_statements {
            if self.build_statement(statement)?.terminator_instruction {
                then_terminated = true;
                continue;
            }
        }
        if !then_terminated {
            self.builder.build_unconditional_branch(merge_bb)?;
        }

        let mut else_terminated = false;
        self.builder.position_at_end(else_bb);
        if let Some(else_st) = if_statement.else_statements {
            for statement in else_st {
                if self.build_statement(statement)?.terminator_instruction {
                    else_terminated = true;
                }
            }
        }
        if !else_terminated {
            self.builder.build_unconditional_branch(merge_bb)?;
        }

        // FIXME: if both `then_terminated` and `else_terminated` is true
        // don't have to generate the merge branch
        // Relies on the function not generating the return in this case.

        self.builder.position_at_end(merge_bb);
        self.symbol_table.pop_scope();

        Ok(())
    }

    pub fn build_for(&mut self, for_statement: ForStatement) -> Result<(), CompilerError> {
        let current_function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let StatementKind::Declaration(Declaration {
            mutable, lhs, rhs, ..
        }) = for_statement.initialiser.kind
        else {
            return Err(CompilerError::CodeGenError(
                (
                    for_statement.initialiser.col,
                    for_statement.initialiser.lnum,
                ),
                format!("Expected declaration found {:?}", for_statement.initialiser),
            ));
        };
        self.symbol_table.push_scope();

        let variable = self.builder.build_alloca(self.context.i32_type(), &lhs)?;

        self.symbol_table
            .store_variable_ptr(lhs, variable, Type::Int, mutable);

        let initial_expression = self.build_expression(rhs)?;
        self.builder.build_store(variable, initial_expression)?;

        let loop_bb = self.context.append_basic_block(current_function, "loop");
        self.builder.build_unconditional_branch(loop_bb)?;
        self.builder.position_at_end(loop_bb);

        // May shadow, but too lazy

        for statement in for_statement.body.unwrap() {
            self.build_statement(statement)?;
        }

        self.build_expression(for_statement.accumalator)?;

        let value = self
            .build_expression(for_statement.condition)?
            .into_int_value();

        // FIXME: need to not rely on int stuff
        let end_cond = self.builder.build_int_compare(
            IntPredicate::NE,
            value.get_type().const_zero(),
            value,
            "loopcond",
        )?;

        let after_bb = self
            .context
            .append_basic_block(current_function, "afterloop");

        self.builder
            .build_conditional_branch(end_cond, loop_bb, after_bb)?;
        self.builder.position_at_end(after_bb);
        self.symbol_table.pop_scope();

        Ok(())
    }
}
