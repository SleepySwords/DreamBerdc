use inkwell::{
    values::{ArrayValue, BasicValueEnum},
    FloatPredicate, IntPredicate,
};
use itertools::Itertools;

use crate::{
    ast::{Expression, Operation},
    compile_error::CompilerError,
    utils::Mutable,
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn build_expression(&mut self, expression: Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::Binary {
                lhs,
                operation,
                rhs,
            } => self.parse_binary(*lhs, operation, *rhs),
            Expression::Call { callee, arguments } => self.build_call(callee, arguments),
            Expression::Assignment { lhs, rhs } => {
                let var = self.symbol_table.fetch_variable(&lhs).unwrap();
                if !var.mutability.contains(Mutable::Reassignable) {
                    // FIXME: proper error handling for compiling
                    panic!(
                        "Compile Error: {}",
                        CompilerError::CodeGenError(
                            "Cannot reassign a constant variable".to_string()
                        )
                    )
                }
                let ptr = var.pointer_value();
                let expression = self.build_expression(*rhs);
                self.builder
                    .build_store(ptr, expression)
                    .expect("Build failed");
                self.context.i32_type().const_zero().into()
            }
            Expression::LiteralValue(strs) => {
                let mut string = strs
                    .chars()
                    .map(|f| self.context.i8_type().const_int(f.into(), false))
                    .collect_vec();
                string.push(self.context.i8_type().const_zero());
                let value: ArrayValue = self.context.i8_type().const_array(&string[..]);
                let ptr = self
                    .builder
                    .build_array_alloca(
                        self.context.i8_type(),
                        self.context.i8_type().const_int(string.len() as u64, false),
                        "pointer",
                    )
                    .unwrap();
                self.builder.build_store(ptr, value).expect("Build failed");
                ptr.into()
            }
            Expression::Identifier(id) => {
                if let Some(ptr) = self.symbol_table.fetch_variable_ptr(&id) {
                    let value = self.builder.build_load(self.context.i32_type(), ptr, &id);
                    value.unwrap()
                } else if let Some(value) = self.symbol_table.fetch_value(&id) {
                    value
                } else {
                    let var_i32 = id.parse::<i32>();
                    if let Ok(var) = var_i32 {
                        let i32_type = self.context.i32_type();
                        i32_type.const_int(var as u64, false).into()
                    } else {
                        let var_f32 = id.parse::<f32>().expect("Invalid constant type");
                        let f32_type = self.context.f32_type();
                        f32_type.const_float(var_f32 as f64).into()
                    }
                }
            }
            _ => todo!(),
        }
    }

    pub fn build_call(
        &mut self,
        callee: String,
        arguments: Vec<Expression>,
    ) -> BasicValueEnum<'ctx> {
        let Some(function) = self.module.get_function(&callee) else {
            panic!("Function not defined")
        };
        // FIXME:, verify arguments with function arguments, and this should be an option
        // The None signifying null.
        if function.count_params() != arguments.len() as u32 {
            panic!("Not enough arguments")
        }

        let args = arguments
            .iter()
            .map(|f| self.build_expression(f.clone()).into())
            .collect_vec();
        let value = self
            .builder
            .build_call(function, args.as_slice(), "calltmp");
        value
            .expect("Build failed")
            .try_as_basic_value()
            .left_or(self.context.i32_type().const_int(0, false).into())
    }

    fn parse_binary(
        &mut self,
        lhs: Expression,
        operation: Operation,
        rhs: Expression,
    ) -> BasicValueEnum<'ctx> {
        let lhs = self.build_expression(lhs);
        let rhs = self.build_expression(rhs);
        if lhs.is_int_value() && rhs.is_int_value() {
            // Need to abstract this!
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();
            match operation {
                Operation::Add => self
                    .builder
                    .build_int_add(lhs, rhs, "add")
                    .expect("Build failed"),
                Operation::Subtract => self
                    .builder
                    .build_int_sub(lhs, rhs, "sub")
                    .expect("Build failed"),
                Operation::Multiply => self
                    .builder
                    .build_int_mul(lhs, rhs, "mul")
                    .expect("Build failed"),
                Operation::Divide => self
                    .builder
                    .build_int_signed_div(lhs, rhs, "div")
                    .expect("Build failed"),
                Operation::Less => self
                    .builder
                    .build_int_compare(IntPredicate::SLT, lhs, rhs, "cond")
                    .expect("Build failed"),
                Operation::Greater => self
                    .builder
                    .build_int_compare(IntPredicate::SGT, lhs, rhs, "cond")
                    .expect("Build failed"),
                Operation::Equal => self
                    .builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "cond")
                    .expect("Build failed"),
                _ => panic!("aefj"),
            }
            .into()
        } else if lhs.is_float_value() && rhs.is_float_value() {
            // Need to abstract this!
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();
            match operation {
                Operation::Add => self
                    .builder
                    .build_float_add(lhs, rhs, "add")
                    .expect("Build failed")
                    .into(),
                Operation::Subtract => self
                    .builder
                    .build_float_sub(lhs, rhs, "sub")
                    .expect("Build failed")
                    .into(),
                Operation::Multiply => self
                    .builder
                    .build_float_mul(lhs, rhs, "mul")
                    .expect("Build failed")
                    .into(),
                Operation::Divide => self
                    .builder
                    .build_float_div(lhs, rhs, "div")
                    .expect("Build failed")
                    .into(),
                Operation::Less => self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "cond")
                    .expect("Build failed")
                    .into(),
                Operation::Greater => self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "cond")
                    .expect("Build failed")
                    .into(),
                Operation::Equal => self
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "cond")
                    .expect("Build failed")
                    .into(),
                _ => panic!("aefj"),
            }
        } else {
            panic!(
                "Cannot use the operation {:?} on incompatible types of: {} and {}",
                operation,
                lhs.get_type(),
                rhs.get_type()
            )
        }
    }
}
