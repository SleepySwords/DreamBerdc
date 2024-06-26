use std::cmp::Ordering;

use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{ArrayValue, BasicMetadataValueEnum, BasicValueEnum},
    FloatPredicate, IntPredicate,
};
use itertools::Itertools;

use crate::{
    ast::{BinOperation, Expression, ExpressionKind, SourcePosition, UnaryOperation},
    compile_error::CompilerError,
    types::{Type, Value},
    utils::Mutable,
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn build_expression(
        &mut self,
        expression: Expression,
    ) -> Result<Value<'ctx>, CompilerError> {
        let expression_pos = expression.pos();
        self.emit_location_debug_info(expression_pos);
        match expression.kind {
            ExpressionKind::Binary {
                lhs,
                operation,
                rhs,
            } => Value::from_none(self.build_binary(*lhs, operation, *rhs)),
            ExpressionKind::Call { callee, arguments } => {
                Value::from_none(self.build_call(callee, arguments, expression_pos))
            }
            ExpressionKind::Assignment { lhs, rhs } => {
                // FIXME: LHS assignments could also be arrays, or pointers
                // (that are dereferenced) must implement the concept of l-values
                // This also included actually storing the value
                // rather than loading the ptr
                let Some(var) = self.symbol_table.fetch_variable(&lhs) else {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        format!("Unknown varaible: {}", lhs),
                    ));
                };
                let t = var.value_type.clone();
                if !var.mutability.contains(Mutable::Reassignable) {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        "Cannot reassign a constant variable",
                    ));
                }
                let ptr = var.pointer();
                let expression = self.build_expression(*rhs)?;
                self.builder.build_store(ptr, expression.value)?;
                Ok(Value {
                    value_type: Some(t),
                    value: self.context.i32_type().const_zero().into(),
                })
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
                // FIXME: it seems like c skips an extra step.
                // Varaibles pointing to array are not mutable.
                // So we could just store the ptr instead of
                // allocating another ptr for a variable.
                //
                // I'm literally not going to understand this
                // next time i read this...
                self.builder.build_store(ptr, value)?;
                Ok(Value {
                    value_type: Some(crate::types::Type::Array(
                        Box::new(Type::Byte),
                        string.len() as u32,
                    )),
                    value: ptr.into(),
                })
            }
            ExpressionKind::Dereference(exp) => {
                let exp = self.build_expression(*exp)?;
                if !exp.value.is_pointer_value() {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        "The value attempted to dereference is not an pointer.",
                    ));
                }
                let Some(Type::Pointer(t)) = exp.value_type else {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        "This is not a pointer type",
                    ));
                };

                Ok(Value {
                    value_type: Some(*t.clone()),
                    value: (self.builder.build_load(
                        t.basic_type_enum(self.context)
                            .ok_or(CompilerError::code_gen_error(
                                expression_pos,
                                "This value is not a valid type",
                            ))?,
                        exp.value.into_pointer_value(),
                        "dereference",
                    )?),
                })
            }
            ExpressionKind::Identifier(id) => {
                if let Some(ptr) = self.symbol_table.fetch_variable(&id) {
                    let basic_type = ptr.value_type.basic_type_enum(self.context).ok_or(
                        CompilerError::code_gen_error(expression_pos, "Invalid type"),
                    )?;
                    let t = ptr.value_type.clone();
                    let value = self.builder.build_load(basic_type, ptr.pointer(), &id);
                    Ok(Value {
                        value: (value.unwrap()),
                        value_type: Some(t),
                    })
                } else if let Some(value) = self.symbol_table.fetch_value(&id) {
                    Value::from_none(Ok(value))
                } else {
                    let var_i32 = id.parse::<i32>();
                    if let Ok(var) = var_i32 {
                        let i32_type = self.context.i32_type();
                        Ok(Value {
                            value_type: Some(Type::Int),
                            value: i32_type.const_int(var as u64, false).into(),
                        })
                    } else {
                        let var_f32 = id.parse::<f32>();
                        if let Ok(var) = var_f32 {
                            let f32_type = self.context.f32_type();
                            Ok(Value {
                                value_type: Some(Type::Float),
                                value: f32_type.const_float(var as f64).into(),
                            })
                        } else {
                            Err(CompilerError::CodeGenError(
                                expression_pos,
                                format!("Could not recognise the symbol: {}", id),
                            ))
                        }
                    }
                }
            }
            ExpressionKind::IndexOperator { expression, index } => {
                let index = self.build_expression(*index)?.value.into_int_value();
                let value = self.build_expression(*expression)?;
                let updated_index = self.builder.build_int_add(
                    index,
                    self.context.i32_type().const_int(1, false),
                    "addIndex",
                )?;
                if value.value.is_pointer_value() {
                    if let Some(Type::Array(element_t, size)) = &value.value_type {
                        let t: BasicTypeEnum = element_t
                            .basic_type_enum(self.context)
                            .unwrap()
                            .array_type(*size)
                            .into();
                        let element_t: BasicTypeEnum =
                            element_t.basic_type_enum(self.context).unwrap();
                        unsafe {
                            let array_ptr = self.builder.build_in_bounds_gep(
                                t,
                                value.value.into_pointer_value(),
                                &[self.context.i64_type().const_zero(), updated_index],
                                "build store",
                            )?;
                            Value::from_none(Ok(self.builder.build_load(
                                element_t,
                                array_ptr,
                                "dereference",
                            )?))
                        }
                    } else if let Some(Type::Pointer(element_t)) = &value.value_type {
                        let t: BasicTypeEnum = element_t.basic_type_enum(self.context).unwrap();
                        let element_t: BasicTypeEnum =
                            element_t.basic_type_enum(self.context).unwrap();
                        unsafe {
                            let reference = self.builder.build_in_bounds_gep(
                                t,
                                value.value.into_pointer_value(),
                                &[updated_index],
                                "build store",
                            )?;
                            Value::from_none(Ok(self.builder.build_load(
                                element_t,
                                reference,
                                "dereference",
                            )?))
                        }
                    } else {
                        Err(CompilerError::CodeGenError(
                            expression_pos,
                            format!("Array type expected, found {:?}", value.value_type),
                        ))
                    }
                } else {
                    Err(CompilerError::CodeGenError(
                        expression_pos,
                        format!(
                            "Attempted to index a type other than an array, found value {}",
                            value.value.get_type()
                        ),
                    ))
                }
            }
            ExpressionKind::Unary {
                operation,
                expression,
            } => match operation {
                UnaryOperation::Negation => {
                    let expression = self.build_expression(*expression)?;
                    let negative_value = if expression.value.is_float_value() {
                        self.builder
                            .build_float_neg(expression.value.into_float_value(), "negation")?
                            .into()
                    } else if expression.value.is_int_value() {
                        self.builder
                            .build_int_neg(expression.value.into_int_value(), "negation")?
                            .into()
                    } else {
                        return Err(CompilerError::CodeGenError(
                            expression_pos,
                            format!(
                                "Attempted to index a type other than an array, found value {}",
                                expression.value.get_type()
                            ),
                        ));
                    };

                    Ok(Value {
                        value_type: expression.value_type,
                        value: negative_value,
                    })
                }
                UnaryOperation::Not => todo!(),
            },
            ExpressionKind::Instantiation(inst_type) => {
                if let Type::Array(arr_type, size) = inst_type {
                    // (0..size).into_iter().map(|f| arr_type.basic_type_enum(inst))
                    let ptr = self
                        .builder
                        .build_array_alloca(
                            arr_type.basic_type_enum(self.context).unwrap(),
                            self.context.i8_type().const_int(size as u64, false),
                            "array_init",
                        )
                        .unwrap();
                    Ok(Value {
                        value_type: Some(crate::types::Type::Array(Box::new(Type::Byte), size)),
                        value: ptr.into(),
                    })
                } else {
                    todo!("Currently not supporting instantiation of other types.")
                }
            }
            _ => todo!(),
        }
    }

    pub fn build_call(
        &mut self,
        callee: String,
        arguments: Vec<Expression>,
        position: SourcePosition,
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
            .map(|f| self.build_expression(f.clone()).map(|f| f.value.into()))
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
        operation: BinOperation,
        rhs_expression: Expression,
    ) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let position = (lhs_expression.col, lhs_expression.lnum);
        let lhs = self.build_expression(lhs_expression)?;
        let rhs = self.build_expression(rhs_expression)?;
        Ok(if lhs.value.is_int_value() && rhs.value.is_int_value() {
            // Need to abstract this!
            // FIXME: pointers need to be converted before being added
            let mut lhs = lhs.value.into_int_value();
            let mut rhs = rhs.value.into_int_value();
            match lhs
                .get_type()
                .get_bit_width()
                .cmp(&rhs.get_type().get_bit_width())
            {
                Ordering::Less => {
                    lhs = self.builder.build_int_cast(lhs, rhs.get_type(), "cast")?;
                }
                Ordering::Greater => {
                    rhs = self.builder.build_int_cast(rhs, lhs.get_type(), "cast")?;
                }
                Ordering::Equal => {}
            }
            match operation {
                BinOperation::Add => self.builder.build_int_add(lhs, rhs, "add")?,
                BinOperation::Subtract => self.builder.build_int_sub(lhs, rhs, "sub")?,
                BinOperation::Multiply => self.builder.build_int_mul(lhs, rhs, "mul")?,
                BinOperation::Divide => self.builder.build_int_signed_div(lhs, rhs, "div")?,
                BinOperation::Remainder => self.builder.build_int_signed_rem(lhs, rhs, "rem")?,
                BinOperation::Less => {
                    self.builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "cond")?
                }
                BinOperation::Greater => {
                    self.builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "cond")?
                }
                BinOperation::Equal => {
                    self.builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "cond")?
                }
                BinOperation::LessThanOrEqual => {
                    self.builder
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, "cond")?
                }
                BinOperation::GreaterThanOrEqual => {
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
        } else if lhs.value.is_float_value() && rhs.value.is_float_value() {
            // Need to abstract this!
            let lhs = lhs.value.into_float_value();
            let rhs = rhs.value.into_float_value();
            match operation {
                BinOperation::Add => self.builder.build_float_add(lhs, rhs, "add")?.into(),
                BinOperation::Subtract => self.builder.build_float_sub(lhs, rhs, "sub")?.into(),
                BinOperation::Multiply => self.builder.build_float_mul(lhs, rhs, "mul")?.into(),
                BinOperation::Divide => self.builder.build_float_div(lhs, rhs, "div")?.into(),
                BinOperation::Less => self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "cond")?
                    .into(),
                BinOperation::Greater => self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "cond")?
                    .into(),
                BinOperation::Equal => self
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
                    lhs.value.get_type(),
                    rhs.value.get_type(),
                ),
            ));
        })
    }
}
