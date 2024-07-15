use std::path::Path;

use inkwell::{
    debug_info::{
        AsDIScope, DICompileUnit, DIFlagsConstants, DIScope, DISubprogram, DIType, DebugInfoBuilder,
    },
    values::{FunctionValue, PointerValue},
    AddressSpace,
};

use crate::{
    ast::{Function, SourcePosition},
    types::Type,
};

use super::CodeGen;

pub struct DebugInfo<'ctx> {
    dibuilder: DebugInfoBuilder<'ctx>,
    compile_unit: DICompileUnit<'ctx>,
    scopes: Vec<DIScope<'ctx>>,
    arg_no: u32,
}

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
            ..
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

            self.emit_location_debug_info((col, lnum));
        }
    }

    pub fn finalise_function_debug_info(&mut self) {
        if let Some(DebugInfo { scopes, .. }) = &mut self.debug_info {
            scopes.pop();
        }
    }

    pub fn emit_location_debug_info(&self, (col, lnum): SourcePosition) -> Option<()> {
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

    // FIXME: actually emit this
    pub fn emit_scope_debug_info(&mut self, (col, lnum): SourcePosition) {
        if let Some(DebugInfo {
            dibuilder,
            scopes,
            compile_unit,
            ..
        }) = &mut self.debug_info
        {
            if let Some(scope) = scopes.last() {
                let lexical_block = dibuilder.create_lexical_block(
                    *scope,
                    compile_unit.get_file(),
                    lnum as u32 + 1,
                    col as u32,
                );
                scopes.push(lexical_block.as_debug_info_scope());
            }
        }
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
            arg_no: 1,
        });
        Some(())
    }

    pub fn create_debug_variable(
        &mut self,
        value: PointerValue<'ctx>,
        value_type: &Type,
        name: String,
        (col, lnum): SourcePosition,
    ) -> Option<()> {
        if let Some(debug) = &mut self.debug_info {
            if let Some(scope) = debug.scopes.last() {
                let variable = debug.dibuilder.create_auto_variable(
                    *scope,
                    &name,
                    // debug.arg_no,
                    debug.compile_unit.get_file(),
                    lnum as u32,
                    value_type.debug_type(&debug.dibuilder),
                    false,
                    DIFlagsConstants::ZERO,
                    0_u32
                );
                let loc = debug.dibuilder.create_debug_location(
                    self.context,
                    lnum as u32 + 1,
                    col as u32,
                    *scope,
                    None,
                );
                debug.dibuilder.insert_declare_at_end(
                    value,
                    Some(variable),
                    None, // Some(debug.dibuilder.create_expression(vec![])),
                    loc,
                    self.builder.get_insert_block()?,
                );
                debug.arg_no += 1;
            }
        }
        Some(())
    }

    pub fn finalise(&self) {
        if let Some(DebugInfo { dibuilder, .. }) = &self.debug_info {
            dibuilder.finalize();
        }
    }
}

impl Type {
    fn debug_type<'ctx>(&self, dibuilder: &DebugInfoBuilder<'ctx>) -> DIType<'ctx> {
        match self {
            Type::Int => dibuilder
                .create_basic_type("int", 32_u64, 0x5, DIFlagsConstants::PUBLIC)
                .unwrap()
                .as_type(),
            Type::Short => dibuilder
                .create_basic_type("int", 16_u64, 0x5, DIFlagsConstants::PUBLIC)
                .unwrap()
                .as_type(),
            Type::Long => dibuilder
                .create_basic_type("int", 64_u64, 0x5, DIFlagsConstants::PUBLIC)
                .unwrap()
                .as_type(),
            Type::Byte => dibuilder
                .create_basic_type("int", 8_u64, 0x5, DIFlagsConstants::PUBLIC)
                .unwrap()
                .as_type(),
            Type::Float => dibuilder
                .create_basic_type("int", 32_u64, 0x5, DIFlagsConstants::PUBLIC)
                .unwrap()
                .as_type(),
            Type::Double => dibuilder
                .create_basic_type("int", 64_u64, 0x5, DIFlagsConstants::PUBLIC)
                .unwrap()
                .as_type(),
            Type::Void => todo!(),
            Type::Pointer(ptr) => dibuilder
                .create_pointer_type(
                    "ptr",
                    ptr.debug_type(dibuilder),
                    64_u64,
                    0_u32,
                    AddressSpace::default(),
                )
                .as_type(),
            Type::Array(array_type, size) => dibuilder
                .create_array_type(
                    array_type.debug_type(dibuilder),
                    64_u64,
                    0_u32,
                    &[0..(*size as i64)],
                )
                .as_type(),
            Type::Class(class) => dibuilder
                .create_basic_type("int", 64_u64, 0x5, DIFlagsConstants::PUBLIC)
                .unwrap()
                .as_type(),
        }
    }
}
