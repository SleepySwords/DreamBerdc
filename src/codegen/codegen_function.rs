use inkwell::{types::BasicMetadataTypeEnum, values::FunctionValue};

use crate::{
    ast::{Function, Prototype, SourcePosition},
    compile_error::CompilerError,
    types::{Type, Value},
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn build_extern(&mut self, prototype: &Prototype) {
        self.build_function_declaration(prototype);
    }

    pub fn build_function_declaration(
        &mut self,
        prototype: &Prototype,
    ) -> Option<FunctionValue<'ctx>> {
        let types = prototype
            .arguments
            .iter()
            .map(|(_, t)| t.basic_type_enum(self.context, &self.symbol_table).map(|f| f.into()))
            .collect::<Option<Vec<BasicMetadataTypeEnum>>>();

        let fn_type =
            prototype
                .return_type
                .function(self.context, &self.symbol_table, types?.as_slice(), prototype.is_var_args);

        self.symbol_table
            .store_function(prototype.name.clone(), prototype.clone());

        return Some(self.module.add_function(&prototype.name, fn_type?, None));
    }

    /// Builds a function
    pub fn build_function(
        &mut self,
        function: Function,
        position: SourcePosition,
    ) -> Result<(), CompilerError> {
        let fn_val = if let Some(fn_val) = self.module.get_function(&function.prototype.name) {
            fn_val
        } else if let Some(fun) = self.build_function_declaration(&function.prototype) {
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

        for (index, (name, arg_type)) in function.prototype.arguments.into_iter().enumerate() {
            self.symbol_table.store_argument(
                name,
                Value {
                    value_type: arg_type,
                    value: fn_val.get_nth_param(index as u32).unwrap(),
                },
            )
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
