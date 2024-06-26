use inkwell::{types::BasicMetadataTypeEnum, values::FunctionValue};

use crate::{
    ast::{
        Function, SourcePosition,
    },
    compile_error::CompilerError,
    types::Type,
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
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

        if build_ret {
            if function.prototype.return_type == Type::Void {
                self.builder.build_return(None)?;
            } else {
                return Err(CompilerError::code_gen_error(
                    position,
                    format!(
                        "Expected return type {:?} for {}, void was found",
                        function.prototype.return_type, function.prototype.name
                    )
                    .as_str(),
                ));
            }
        }
        self.finalise_function_debug_info();
        self.symbol_table.pop_scope();

        Ok(())
    }
}
