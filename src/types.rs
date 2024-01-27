use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, FunctionType},
    values::{AnyValueEnum, BasicValueEnum, FloatValue, IntValue, PointerValue},
};

use crate::utils::Mutable;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Void,
    Pointer(Box<Type>),
}

impl Type {
    pub fn function<'a>(
        &self,
        context: &'a Context,
        param_types: &[BasicMetadataTypeEnum<'a>],
        is_var_args: bool,
    ) -> FunctionType<'a> {
        match self {
            Type::Int => context.i32_type().fn_type(param_types, is_var_args),
            Type::Float => context.f32_type().fn_type(param_types, is_var_args),
            Type::Void => context.void_type().fn_type(param_types, is_var_args),
            Type::Pointer(_) => context.i64_type().fn_type(param_types, is_var_args),
        }
    }

    pub fn basic_metadata_enum<'a>(&self, context: &'a Context) -> BasicMetadataTypeEnum<'a> {
        match &self {
            Type::Int => context.i32_type().into(),
            Type::Float => context.f32_type().into(),
            Type::Pointer(_) => context.i64_type().into(),
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

pub struct Value<'ctx> {
    // value could also act as value_type
    pub value_type: Type,
    pub value: AnyValueEnum<'ctx>,
    pub mutability: Mutable,
}

impl<'ctx> Value<'ctx> {
    pub fn new(value_type: Type, value: AnyValueEnum<'ctx>) -> Self {
        Self {
            value_type,
            value,
            mutability: Mutable::NONE,
        }
    }

    pub fn int_value(&self) -> IntValue<'ctx> {
        // Should panic if not correct type.
        return self.value.into_int_value();
    }

    pub fn float_value(&self) -> FloatValue<'ctx> {
        // Should panic if not correct cast.
        return self.value.into_float_value();
    }

    pub fn pointer_value(&self) -> PointerValue<'ctx> {
        // Should panic if not correct cast.
        return self.value.into_pointer_value();
    }

    // Returns basic values
    // pub fn basic_value(&self) -> Option<dyn BasicValue<'ctx>> {
    //     match self.value_type {
    //         Type::Int => Some(self.value.into_int_value().into()),
    //         Type::Float => Some(self.value.into_float_value().into()),
    //         Type::Void => None,
    //         Type::Pointer(_) => Some(self.value.into_pointer_value().into()),
    //     }
    // }

    // Returns basic values
    pub fn basic_value_enum(&self) -> Option<BasicValueEnum<'ctx>> {
        match self.value_type {
            Type::Int => Some(self.value.into_int_value().into()),
            Type::Float => Some(self.value.into_float_value().into()),
            Type::Void => None,
            Type::Pointer(_) => Some(self.value.into_pointer_value().into()),
        }
    }
}
