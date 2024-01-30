mod codegen_expression;
mod codegen_statement;

use std::path::Path;

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::JitFunction,
    module::Module,
    targets::{InitializationConfig, Target, TargetMachine},
    OptimizationLevel,
};

use crate::symboltable::SymbolTable;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub symbol_table: SymbolTable<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn run_jit(&self, optimisation: OptimizationLevel) {
        let execution_engine = self
            .module
            .create_jit_execution_engine(optimisation)
            .unwrap();
        unsafe {
            type Main = unsafe extern "C" fn() -> u32;
            let main: JitFunction<Main> = execution_engine.get_function("main").unwrap();
            println!("Return code: {}", main.call());
            // println!("Return code: {}", main.call(10, 3));
        }
    }

    pub fn write_llvm_ir(&self, path: &Path) {
        self.module.print_to_file(path).expect("Error");
    }

    pub fn compile_to_obj(&self, path: &Path, optimisation: OptimizationLevel) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target = Target::from_triple(&target_triple).expect("Error");
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                optimisation,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("Error");

        target_machine
            .write_to_file(&self.module, inkwell::targets::FileType::Object, path)
            .expect("Error");
    }

    // FIXME: need to implement ast locations
    // pub fn create_debug_symbols(&self) {
    //     let (dibuilder, compile_unit) = self.module.create_debug_info_builder(
    //         false,
    //         inkwell::debug_info::DWARFSourceLanguage::C,
    //         "source_file",
    //         ".",
    //         "my llvm compiler frontend",
    //         false,
    //         "",
    //         0,
    //         "",
    //         inkwell::debug_info::DWARFEmissionKind::Full,
    //         0,
    //         false,
    //         false,
    //         "",
    //         "",
    //     );
    //     dibuilder.finalize();
    //     dibuilder.create_file(compile_unit.get_file(), compile_unit.get_file())
    // }
}

struct CompileInfo {
    early_return: bool,
}
