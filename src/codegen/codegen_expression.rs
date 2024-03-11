use inkwell::{
    values::{ArrayValue, BasicMetadataValueEnum, BasicValueEnum},
    FloatPredicate, IntPredicate,
};
use itertools::Itertools;

use crate::{
    ast::{Expression, ExpressionKind, Operation},
    compile_error::CompilerError,
    utils::Mutable,
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn build_expression(
        &mut self,
        expression: Expression,
    ) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let expression_pos = expression.pos();
        match expression.kind {
            ExpressionKind::Binary {
                lhs,
                operation,
                rhs,
            } => self.build_binary(*lhs, operation, *rhs),
            ExpressionKind::Call { callee, arguments } => {
                self.build_call(callee, arguments, expression_pos)
            }
            ExpressionKind::Assignment { lhs, rhs } => {
                let Some(var) = self.symbol_table.fetch_variable(&lhs) else {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        format!("Unknown varaible: {}", lhs),
                    ));
                };
                if !var.mutability.contains(Mutable::Reassignable) {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        "Cannot reassign a constant variable",
                    ));
                }
                let ptr = var.pointer();
                let expression = self.build_expression(*rhs)?;
                self.builder.build_store(ptr, expression)?;
                Ok(self.context.i32_type().const_zero().into())
            }
            ExpressionKind::LiteralValue(strs) => {
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
                self.builder.build_store(ptr, value)?;
                Ok(ptr.into())
            }
            ExpressionKind::Dereference(exp) => {
                let exp = self.build_expression(*exp)?;
                if !exp.is_pointer_value() {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        "The value attempted to dereference is not an pointer.",
                    ));
                }
                // FIXME: Opaque types have taken over it seems...
                // Need to refactor to also include the type..
                Ok(self.builder.build_load(
                    self.context.i32_type(),
                    exp.into_pointer_value(),
                    "dereference",
                )?)
            }
            ExpressionKind::Identifier(id) => {
                if let Some(ptr) = self.symbol_table.fetch_variable(&id) {
                    let basic_type = ptr.value_type.basic_type_enum(self.context).ok_or(
                        CompilerError::code_gen_error(expression_pos, "Invalid type"),
                    )?;
                    let value = self.builder.build_load(basic_type, ptr.pointer(), &id);
                    Ok(value.unwrap())
                } else if let Some(value) = self.symbol_table.fetch_value(&id) {
                    Ok(value)
                } else {
                    let var_i32 = id.parse::<i32>();
                    if let Ok(var) = var_i32 {
                        let i32_type = self.context.i32_type();
                        Ok(i32_type.const_int(var as u64, false).into())
                    } else {
                        let var_f32 = id.parse::<f32>();
                        if let Ok(var) = var_f32 {
                            let f32_type = self.context.f32_type();
                            Ok(f32_type.const_float(var as f64).into())
                        } else {
                            Err(CompilerError::CodeGenError(
                                expression_pos,
                                format!("Could not recognise the symbol: {}", id),
                            ))
                        }
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
        position: (usize, usize),
    ) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let Some(function) = self.module.get_function(&callee) else {
            return Err(CompilerError::CodeGenError(
                position,
                format!("The function \"{}\" does not exist.", callee),
            ));
        };
        // FIXME:, verify arguments with function arguments, and this should be an option
        // The None signifying null.
        if function.count_params() != arguments.len() as u32 && !function.get_type().is_var_arg() {
            // FIXME: should be more specific with arguments
            return Err(CompilerError::CodeGenError(
                position,
                format!("The function \"{}\" with argument length of {} does not match provided argument length of {}.", callee, function.count_params(), arguments.len()),
            ));
        }

        let args = arguments
            .iter()
            .map(|f| self.build_expression(f.clone()).map(|f| f.into()))
            .collect::<Result<Vec<BasicMetadataValueEnum<'ctx>>, CompilerError>>()?;
        let value = self
            .builder
            .build_call(function, args.as_slice(), "calltmp")?;
        Ok(value
            .try_as_basic_value()
            .left_or(self.context.i32_type().const_int(0, false).into()))
    }

    fn build_binary(
        &mut self,
        lhs_expression: Expression,
        operation: Operation,
        rhs_expression: Expression,
    ) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let position = (lhs_expression.col, lhs_expression.lnum);
        let lhs = self.build_expression(lhs_expression)?;
        let rhs = self.build_expression(rhs_expression)?;
        Ok(if lhs.is_int_value() && rhs.is_int_value() {
            // Need to abstract this!
            // FIXME: pointers need to be converted before being added
            let mut lhs = lhs.into_int_value();
            let mut rhs = rhs.into_int_value();
            match lhs
                .get_type()
                .get_bit_width()
                .cmp(&rhs.get_type().get_bit_width())
            {
                std::cmp::Ordering::Less => {
                    lhs = self.builder.build_int_cast(lhs, rhs.get_type(), "cast")?;
                }
                std::cmp::Ordering::Greater => {
                    rhs = self.builder.build_int_cast(rhs, lhs.get_type(), "cast")?;
                }
                std::cmp::Ordering::Equal => {}
            }
            match operation {
                Operation::Add => self.builder.build_int_add(lhs, rhs, "add")?,
                Operation::Subtract => self.builder.build_int_sub(lhs, rhs, "sub")?,
                Operation::Multiply => self.builder.build_int_mul(lhs, rhs, "mul")?,
                Operation::Divide => self.builder.build_int_signed_div(lhs, rhs, "div")?,
                Operation::Remainder => self.builder.build_int_signed_rem(lhs, rhs, "rem")?,
                Operation::Less => {
                    self.builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "cond")?
                }
                Operation::Greater => {
                    self.builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "cond")?
                }
                Operation::Equal => {
                    self.builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "cond")?
                }
                Operation::LessThanOrEqual => {
                    self.builder
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, "cond")?
                }
                Operation::GreaterThanOrEqual => {
                    self.builder
                        .build_int_compare(IntPredicate::SGE, lhs, rhs, "cond")?
                }
                _ => {
                    return Err(CompilerError::CodeGenError(
                        position,
                        format!("Operation {:?} not yet implemented", operation,),
                    ))
                }
            }
            .into()
        } else if lhs.is_float_value() && rhs.is_float_value() {
            // Need to abstract this!
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();
            match operation {
                Operation::Add => self.builder.build_float_add(lhs, rhs, "add")?.into(),
                Operation::Subtract => self.builder.build_float_sub(lhs, rhs, "sub")?.into(),
                Operation::Multiply => self.builder.build_float_mul(lhs, rhs, "mul")?.into(),
                Operation::Divide => self.builder.build_float_div(lhs, rhs, "div")?.into(),
                Operation::Less => self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "cond")?
                    .into(),
                Operation::Greater => self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "cond")?
                    .into(),
                Operation::Equal => self
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "cond")?
                    .into(),
                _ => {
                    return Err(CompilerError::CodeGenError(
                        position,
                        format!("Operation {:?} not yet implemented", operation,),
                    ))
                }
            }
        } else {
            return Err(CompilerError::CodeGenError(
                position,
                format!(
                    "Cannot use the operation {:?} with incompatible types of: {} and {}",
                    operation,
                    lhs.get_type(),
                    rhs.get_type(),
                ),
            ));
        })
    }
}
