use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, PointerValue},
    AddressSpace,
};

use crate::utils::Mutable;

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Deserialize))]
pub enum Type {
    Int,
    Short,
    Long,
    Byte,
    Float,
    Double,
    Void,
    Pointer(Box<Type>),
    Array(Box<Type>, u32),
}

impl Type {
    pub fn function<'a>(
        &self,
        context: &'a Context,
        param_types: &[BasicMetadataTypeEnum<'a>],
        is_var_args: bool,
    ) -> Option<FunctionType<'a>> {
        Some(match self {
            Type::Int => context.i32_type().fn_type(param_types, is_var_args),
            Type::Short => context.i16_type().fn_type(param_types, is_var_args),
            Type::Long => context.i64_type().fn_type(param_types, is_var_args),
            Type::Byte => context.i8_type().fn_type(param_types, is_var_args),
            Type::Float => context.f32_type().fn_type(param_types, is_var_args),
            Type::Double => context.f64_type().fn_type(param_types, is_var_args),
            Type::Void => context.void_type().fn_type(param_types, is_var_args),
            Type::Pointer(t) => t
                .basic_type_enum(context)?
                .ptr_type(AddressSpace::default())
                .fn_type(param_types, is_var_args),
            Type::Array(t, s) => t
                .basic_type_enum(context)?
                .array_type(*s)
                .fn_type(param_types, is_var_args),
        })
    }

    pub fn basic_type_enum<'a>(&self, context: &'a Context) -> Option<BasicTypeEnum<'a>> {
        match self {
            Type::Int => Some(context.i32_type().as_basic_type_enum()),
            Type::Short => Some(context.i16_type().as_basic_type_enum()),
            Type::Long => Some(context.i64_type().as_basic_type_enum()),
            Type::Byte => Some(context.i8_type().as_basic_type_enum()),
            Type::Float => Some(context.f32_type().as_basic_type_enum()),
            Type::Double => Some(context.f64_type().as_basic_type_enum()),
            Type::Void => None,
            Type::Pointer(t) => Some(
                t.basic_type_enum(context)?
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            ),
            Type::Array(t, s) => Some(
                t.basic_type_enum(context)?
                    .array_type(*s)
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            ),
        }
    }

    pub fn basic_metadata_enum<'a>(
        &self,
        context: &'a Context,
    ) -> Option<BasicMetadataTypeEnum<'a>> {
        Some(match &self {
            Type::Int => context.i32_type().into(),
            Type::Short => context.i16_type().into(),
            Type::Long => context.i64_type().into(),
            Type::Byte => context.i8_type().into(),
            Type::Float => context.f32_type().into(),
            Type::Double => context.f64_type().into(),
            Type::Pointer(t) => t
                .basic_type_enum(context)?
                .ptr_type(AddressSpace::default())
                .into(),
            Type::Array(t, s) => t.basic_type_enum(context)?.array_type(*s).into(),
            Type::Void => return None,
        })
    }
}

pub struct Variable<'ctx> {
    // value could also act as value_type
    pub value_type: Type,
    pub pointer: PointerValue<'ctx>,
    pub mutability: Mutable,
}

impl<'ctx> Variable<'ctx> {
    pub fn pointer(&self) -> PointerValue<'ctx> {
        self.pointer
    }
}

#[derive(Clone)]
pub struct Value<'ctx> {
    pub value_type: Type,
    pub value: BasicValueEnum<'ctx>,
}
