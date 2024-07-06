use crate::{
    ast::{Statement, StatementKind},
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
                let rhs_exp = self.build_expression(declaration.rhs)?;
                let var_type = if let Some(t) = declaration.var_type {
                    t
                } else {
                    rhs_exp.value_type.unwrap_or(Type::Int)
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
                    .build_alloca(basic_type_enum, &(declaration.lhs.clone() + "_var"))?;

                self.create_debug_variable(variable, declaration.lhs.clone(), statement_pos);

                self.builder.build_store(variable, rhs_exp.value)?;
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
                    self.builder.build_return(Some(&value.value))?;
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
            StatementKind::Class(c) => self.build_class(c),
            StatementKind::Free(expression) => {
                let expression = self.build_expression(*expression)?;
                if expression.value.is_pointer_value() {
                    self.builder
                        .build_free(expression.value.into_pointer_value())?;
                }
            }
        }
        Ok(CompileInfo {
            terminator_instruction: false,
        })
    }
}
