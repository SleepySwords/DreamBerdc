use std::{collections::HashMap, path::Path};

use inkwell::{
    debug_info::{
        AsDIScope, DICompileUnit, DIFlagsConstants, DIScope, DISubprogram, DebugInfoBuilder,
    },
    values::FunctionValue,
};

use crate::ast::{Function, SourcePosition};

use super::{CodeGen, CompileInfo};

pub struct DebugInfo<'ctx> {
    dibuilder: DebugInfoBuilder<'ctx>,
    compile_unit: DICompileUnit<'ctx>,
    scopes: Vec<DIScope<'ctx>>,
}

// Should be in a dedicated struct...
impl<'ctx> CodeGen<'ctx> {
    pub fn emit_function_debug_info(
        &mut self,
        function: &Function,
        func_val: &FunctionValue<'ctx>,
        (col, lnum): SourcePosition,
    ) {
        if let Some(DebugInfo {
            dibuilder,
            compile_unit,
            scopes,
        }) = &mut self.debug_info
        {
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
                // This is cool, modifying this modifies name when
                // debugging
                &function.prototype.name,
                None,
                compile_unit.get_file(),
                lnum as u32 + 1,
                subroutine_type,
                true,
                true,
                lnum as u32 + 1,
                DIFlagsConstants::PUBLIC,
                false,
            );
            func_val.set_subprogram(func_scope);

            scopes.push(func_scope.as_debug_info_scope());

            // let lexical_block = dibuilder.create_lexical_block(
            //     func_scope.as_debug_info_scope(),
            //     compile_unit.get_file(),
            //     lnum as u32 + 1,
            //     col as u32,
            // );

            self.emit_scope_debug_info((col, lnum));
        }
    }

    pub fn finalise_function_debug_info(&mut self) {
        if let Some(DebugInfo { scopes, .. }) = &mut self.debug_info {
            scopes.pop();
        }
    }

    pub fn emit_scope_debug_info(&self, (col, lnum): SourcePosition) -> Option<()> {
        if let Some(DebugInfo {
            dibuilder, scopes, ..
        }) = &self.debug_info
        {
            let loc = dibuilder.create_debug_location(
                self.context,
                lnum as u32 + 1,
                col as u32,
                *scopes.last()?,
                None,
            );
            self.builder.set_current_debug_location(loc);
        }
        Some(())
    }

    pub fn emit_location_debug_info(&self, (col, lnum): SourcePosition) {
        // if let Some((dibuilder, compile_unit)) = &self.debug_info {
        //     let loc = dibuilder.create_debug_location(
        //         self.context,
        //         lnum as u32 + 1,
        //         col as u32,
        //         lexical_block.as_debug_info_scope(),
        //         None,
        //     );
        //     self.builder.set_current_debug_location(loc);
        // }
    }

    // FIXME: need to implement ast locations
    pub fn create_debug_symbols(&mut self, path: &Path) -> Option<()> {
        let path_buff = path.to_path_buf();
        let (dibuilder, compile_unit) = self.module.create_debug_info_builder(
            false,
            inkwell::debug_info::DWARFSourceLanguage::C,
            path_buff.file_name()?.to_str()?,
            path_buff.parent()?.to_str()?,
            "DreamberdC",
            false,
            "",
            0,
            "",
            inkwell::debug_info::DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );
        self.debug_info = Some(DebugInfo {
            dibuilder,
            compile_unit,
            scopes: vec![],
        });
        Some(())
    }

    pub fn finalise(&self) {
        if let Some(DebugInfo { dibuilder, .. }) = &self.debug_info {
            dibuilder.finalize();
        }
    }
}
