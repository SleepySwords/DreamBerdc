use inkwell::{builder::Builder, context::Context, execution_engine::JitFunction, module::Module};

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl Compiler<'_> {
    pub fn build_add(&self) {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
        let fn_val = self.module.add_function("add", fn_type, None);

        let entry_basic_box = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry_basic_box);

        let x = fn_val.get_nth_param(0).unwrap().into_int_value();
        let y = fn_val.get_nth_param(1).unwrap().into_int_value();

        let ret = self.builder.build_int_add(x, y, "add");
        self.builder.build_return(Some(&ret));
    }

    pub fn compile(&self) {
        let execution_engine = self
            .module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        unsafe {
            type Addition = unsafe extern "C" fn(i32, i32) -> i32;
            let add: JitFunction<Addition> = execution_engine.get_function("add").unwrap();
            let x = 1;
            let y = 2;
            assert_eq!(add.call(x, y), x + y);
        }
    }
}
