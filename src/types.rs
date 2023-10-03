use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, FunctionType},
};

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Int,
    Float,
    Void,
}

impl Type {
    pub fn function<'a>(
        self,
        context: &'a Context,
        param_types: &[BasicMetadataTypeEnum<'a>],
        is_var_args: bool,
    ) -> FunctionType<'a> {
        match self {
            Type::Int => context.i32_type().fn_type(param_types, is_var_args),
            Type::Float => context.f32_type().fn_type(param_types, is_var_args),
            Type::Void => context.void_type().fn_type(param_types, is_var_args),
        }
    }

    pub(crate) fn parse(t: String) -> Type {
        match t.as_str() {
            "int" => Type::Int,
            "float" => Type::Int,
            _ => panic!("Type {t} not implemented!"),
        }
    }
}
