use inkwell::IntPredicate;

use crate::{
    ast::{Declaration, ForStatement, IfStatement, SourcePosition, Statement, StatementKind},
    compile_error::CompilerError,
    types::Type,
};

use super::{CodeGen, CompileInfo};

impl<'ctx> CodeGen<'ctx> {
    pub fn build_block(
        &mut self,
        statements: Vec<Statement>,
    ) -> Result<CompileInfo, CompilerError> {
        self.symbol_table.push_scope();
        let mut terminator_instruction = false;
        for statement in statements {
            if self.build_statement(statement)?.terminator_instruction {
                terminator_instruction = true;
                break;
            }
        }
        self.symbol_table.pop_scope();
        Ok(CompileInfo {
            terminator_instruction,
        })
    }

    pub fn build_if(
        &mut self,
        if_statement: IfStatement,
        statement_pos: SourcePosition,
    ) -> Result<CompileInfo, CompilerError> {
        self.symbol_table.push_scope();

        let value = self
            .build_expression(if_statement.boolean_op)?
            .value
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
            .ok_or_else(|| CompilerError::code_gen_error(statement_pos, "Cannot get parent"))?;

        let then_bb = self.context.append_basic_block(current_function, "then");
        let else_bb = self.context.append_basic_block(current_function, "else");

        self.builder
            .build_conditional_branch(condition, then_bb, else_bb)?;

        self.builder.position_at_end(then_bb);
        let then_terminated = self
            .build_block(if_statement.then_statements)?
            .terminator_instruction;

        self.builder.position_at_end(else_bb);
        let else_terminated = if let Some(else_st) = if_statement.else_statements {
            self.build_block(else_st)?.terminator_instruction
        } else {
            false
        };

        if !then_terminated || !else_terminated {
            let merge_bb = self.context.append_basic_block(current_function, "ifcont");
            if !then_terminated {
                self.builder.position_at_end(then_bb);
                self.builder.build_unconditional_branch(merge_bb)?;
            }
            if !else_terminated {
                self.builder.position_at_end(else_bb);
                self.builder.build_unconditional_branch(merge_bb)?;
            }
            self.builder.position_at_end(merge_bb);
        }

        self.symbol_table.pop_scope();
        Ok(CompileInfo {
            terminator_instruction: then_terminated && else_terminated,
        })
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
        self.builder
            .build_store(variable, initial_expression.value)?;

        let loop_bb = self.context.append_basic_block(current_function, "loop");
        self.builder.build_unconditional_branch(loop_bb)?;
        self.builder.position_at_end(loop_bb);

        // May shadow, but too lazy

        self.build_block(for_statement.body)?;

        self.build_expression(for_statement.accumalator)?;

        let value = self
            .build_expression(for_statement.condition)?
            .value
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
