use colored::Colorize;
use inkwell::{types::BasicMetadataTypeEnum, values::FunctionValue, IntPredicate};

use crate::{
    ast::{Declaration, ForStatement, Function, IfStatement, Statement, StatementKind},
    compile_error::CompilerError,
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn build_statement(&mut self, statement: Statement) -> Result<bool, CompilerError> {
        match statement.kind {
            StatementKind::Declaration(declaration) => {
                let variable = self
                    .builder
                    .build_alloca(self.context.i32_type(), &declaration.lhs)?;
                let rhs = self.build_expression(declaration.rhs)?;
                self.builder.build_store(variable, rhs)?;
                self.symbol_table
                    .store_variable_ptr(declaration.lhs, variable, declaration.mutable)
            }
            StatementKind::Return { return_value } => {
                let value = self.build_expression(return_value)?;
                self.builder.build_return(Some(&value))?;
                return Ok(true);
            }
            StatementKind::Function(function) => {
                self.build_function(function)?;
            }
            StatementKind::Expression(expr) => {
                self.build_expression(expr)?;
            }
            StatementKind::If(if_statement) => self.build_if(if_statement)?,
            StatementKind::For(for_statement) => self.build_for(*for_statement)?,
        }
        Ok(false)
    }

    pub fn build_function_declaration(&mut self, function: &Function) -> FunctionValue<'ctx> {
        let types = function
            .prototype
            .arguments
            .iter()
            .map(|(_, t)| t.basic_metadata_enum(self.context))
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let fn_type =
            function
                .prototype
                .return_type
                .function(self.context, types.as_slice(), false);
        return self
            .module
            .add_function(&function.prototype.name, fn_type, None);
    }

    /// Builds a function
    pub fn build_function(&mut self, function: Function) -> Result<(), CompilerError> {
        let fn_val = if let Some(fn_val) = self.module.get_function(&function.prototype.name) {
            fn_val
        } else {
            self.build_function_declaration(&function)
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
            if self.build_statement(statement)? {
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

    pub fn build_if(&mut self, if_statement: IfStatement) -> Result<(), CompilerError> {
        self.symbol_table.push_scope();

        let value = self
            .build_expression(if_statement.boolean_op)?
            .into_int_value();

        // FIXME: need to not rely on int stuff
        let condition = self.builder.build_int_compare(
            IntPredicate::NE,
            self.context.i32_type().const_zero(),
            value,
            "ifcond",
        )?;

        let current_function = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CompilerError::CodeGenError("Cannot find insert block".to_string()))?
            .get_parent()
            .ok_or_else(|| CompilerError::CodeGenError("Cannot get parent".to_string()))?;

        let then_bb = self.context.append_basic_block(current_function, "then");
        let else_bb = self.context.append_basic_block(current_function, "else");
        let merge_bb = self.context.append_basic_block(current_function, "ifcont");

        self.builder
            .build_conditional_branch(condition, then_bb, else_bb)?;

        self.builder.position_at_end(then_bb);

        let mut build_branch = true;
        for statement in if_statement.then_statements {
            if self.build_statement(statement)? {
                build_branch = false;
                continue;
            }
        }
        if build_branch {
            self.builder.build_unconditional_branch(merge_bb)?;
        }

        build_branch = true;
        self.builder.position_at_end(else_bb);
        if let Some(else_st) = if_statement.else_statements {
            for statement in else_st {
                if self.build_statement(statement)? {
                    build_branch = false;
                }
            }
        }
        if build_branch {
            self.builder.build_unconditional_branch(merge_bb)?;
        }

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
            return Err(CompilerError::CodeGenErrorWithPos(
                (
                    for_statement.initialiser.col,
                    for_statement.initialiser.lnum,
                ),
                format!("Expected declaration found {:?}", for_statement.initialiser),
            ));
        };
        self.symbol_table.push_scope();

        let variable = self.builder.build_alloca(self.context.i32_type(), &lhs)?;

        self.symbol_table.store_variable_ptr(lhs, variable, mutable);

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
            self.context.i32_type().const_zero(),
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
