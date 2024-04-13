use inkwell::{
    debug_info::{AsDIScope, DIFlagsConstants, DISubprogram},
    values::FunctionValue,
};

use crate::ast::{Function, SourcePosition};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn add_function_debug_info(
        &self,
        function: &Function,
        func_val: &FunctionValue<'ctx>,
        position: SourcePosition,
    ) {
        if let Some((dibuilder, compile_unit)) = &self.debug_info {
            let ditype = dibuilder
                .create_basic_type("type_name", 0_u64, 0x00, DIFlagsConstants::PUBLIC)
                .unwrap();
            let subroutine_type = dibuilder.create_subroutine_type(
                compile_unit.get_file(),
                Some(ditype.as_type()),
                &[],
                DIFlagsConstants::PUBLIC,
            );
            let func_scope: DISubprogram<'_> = dibuilder.create_function(
                compile_unit.as_debug_info_scope(),
                &function.prototype.name,
                None,
                compile_unit.get_file(),
                position.1 as u32 + 1,
                subroutine_type,
                true,
                true,
                0,
                DIFlagsConstants::PUBLIC,
                false,
            );
            func_val.set_subprogram(func_scope);

            let lexical_block = dibuilder.create_lexical_block(
                /* scope */ func_scope.as_debug_info_scope(),
                /* file */ compile_unit.get_file(),
                /* line_no */ position.1 as u32 + 1,
                /* column_no */ position.0 as u32,
            );

            let loc = dibuilder.create_debug_location(
                self.context,
                position.1 as u32 + 1,
                position.0 as u32,
                lexical_block.as_debug_info_scope(),
                None,
            );
            self.builder.set_current_debug_location(loc);
        }
    }
}
