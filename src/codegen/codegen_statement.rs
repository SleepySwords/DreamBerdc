use colored::Colorize;
use inkwell::{types::BasicMetadataTypeEnum, values::FunctionValue, IntPredicate};

use crate::{
    ast::{
        Declaration, ForStatement, Function, IfStatement, SourcePosition, Statement, StatementKind,
    },
    compile_error::CompilerError,
    types::Type,
};

use super::{CodeGen, CompileInfo};

impl<'ctx> CodeGen<'ctx> {
    pub fn build_statement(&mut self, statement: Statement) -> Result<CompileInfo, CompilerError> {
        let statement_pos = statement.pos();
        self.emit_location_debug_info(statement_pos);
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

                self.create_debug_variable(variable, declaration.lhs.clone(), statement_pos);

                self.builder.build_store(variable, rhs)?;
                self.symbol_table.store_variable_ptr(
                    declaration.lhs,
                    variable,
                    var_type,
                    declaration.mutable,
                )
            }
            StatementKind::Return { return_value } => {
                println!("{:?}", statement_pos);
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
                self.build_function(function, statement_pos)?;
            }
            StatementKind::Expression(expr) => {
                self.build_expression(expr)?;
            }
            StatementKind::If(if_statement) => return self.build_if(if_statement, statement_pos),
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
    pub fn build_function(
        &mut self,
        function: Function,
        position: SourcePosition,
    ) -> Result<(), CompilerError> {
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

        self.emit_function_debug_info(&function, &fn_val, position);

        for (index, (name, _)) in function.prototype.arguments.into_iter().enumerate() {
            self.symbol_table
                .store_value(name, fn_val.get_nth_param(index as u32).unwrap())
        }

        let mut build_ret = true;

        for statement in function.body {
            if self.build_statement(statement)?.terminator_instruction {
                build_ret = false;
                break;
            }
        }

        // FIX: catch all return...
        // Should actually check if it returns something in all branches.

        if build_ret {
            self.builder.build_return(None)?;
        }

        self.finalise_function_debug_info();
        self.symbol_table.pop_scope();

        Ok(())
    }

    pub fn build_if(
        &mut self,
        if_statement: IfStatement,
        statement_pos: SourcePosition,
    ) -> Result<CompileInfo, CompilerError> {
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
        self.builder.build_store(variable, initial_expression)?;

        let loop_bb = self.context.append_basic_block(current_function, "loop");
        self.builder.build_unconditional_branch(loop_bb)?;
        self.builder.position_at_end(loop_bb);

        // May shadow, but too lazy

        self.build_block(for_statement.body)?;

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
        return Ok(CompileInfo {
            terminator_instruction,
        });
    }
}
