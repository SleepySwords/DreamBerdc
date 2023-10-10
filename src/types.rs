use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, FunctionType},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    pub fn basic_metadata_enum<'a>(self, context: &'a Context) -> BasicMetadataTypeEnum<'a> {
        match self {
            Type::Int => context.i32_type().into(),
            Type::Float => context.f32_type().into(),
            Type::Void => panic!("Invalid type"),
        }
    }

    pub(crate) fn parse(t: String) -> Type {
        match t.as_str() {
            "int" => Type::Int,
            "float" => Type::Float,
            _ => panic!("Type {t} not implemented!"),
        }
    }
}
