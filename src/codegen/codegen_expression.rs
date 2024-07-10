use std::cmp::Ordering;

use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{ArrayValue, BasicMetadataValueEnum, PointerValue},
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
            } => self.build_binary(*lhs, operation, *rhs),
            ExpressionKind::Call { callee, arguments } => {
                self.build_call(callee, arguments, expression_pos)
            }
            ExpressionKind::Assignment { lhs, rhs } => {
                // FIXME: LHS assignments could also be arrays, or pointers
                // (that are dereferenced) must implement the concept of l-values
                // This also included actually storing the value
                // rather than loading the ptr
                //
                // NOTE: it's debatable whether we throw the l-value error in
                // code generation or in parsing. We might also want to use a
                // different operation for assigning struct values (eg: set)
                let (lhs_ptr, lhs_type) = if let ExpressionKind::Identifier(lhs_id) = &lhs.kind {
                    let Some(var) = self.symbol_table.fetch_variable(lhs_id) else {
                        return Err(CompilerError::code_gen_error(
                            expression_pos,
                            format!("Unknown varaible: {}", lhs_id),
                        ));
                    };
                    let t = var.value_type.clone();
                    if !var.mutability.contains(Mutable::Reassignable) {
                        return Err(CompilerError::code_gen_error(
                            expression_pos,
                            "Cannot reassign a constant variable",
                        ));
                    }
                    (var.pointer(), t)
                } else if let ExpressionKind::IndexOperator { expression, index } = lhs.kind {
                    let index = self.build_expression(*index)?;
                    let value = self.build_expression(*expression)?;

                    self.get_array_ptr(value, index, expression_pos)?
                } else {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        format!("Expected an l-value, found {:?}.", lhs),
                    ));
                };

                // FIXME: The way type conversion was implemented here is
                // heavily reliant on llvm, ideally implementation details are
                // abstracted in the type, rather than here. As it will get
                // convoluted when considering other type conversion rules (ie:
                // floating point values, ints to floats, address arithemtic
                // (adding an integer to a pointer), etc...)
                let rhs_expression = self.build_expression(*rhs)?;

                let mut rhs_value = rhs_expression.value;

                if let Some(rhs_basic_type) =
                    rhs_expression.value_type.basic_type_enum(self.context)
                {
                    if let Some(lhs_basic_type) = lhs_type.basic_type_enum(self.context) {
                        if rhs_basic_type.is_int_type()
                            && lhs_basic_type.is_int_type()
                            && rhs_basic_type.into_int_type().get_bit_width()
                                != lhs_basic_type.into_int_type().get_bit_width()
                        {
                            rhs_value = self
                                .builder
                                .build_int_cast(
                                    rhs_expression.value.into_int_value(),
                                    lhs_basic_type.into_int_type(),
                                    "cast",
                                )?
                                .into();
                        }
                    }
                }

                self.builder.build_store(lhs_ptr, rhs_value)?;
                Ok(Value {
                    value_type: lhs_type,
                    value: rhs_value,
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
                    value_type: crate::types::Type::Array(
                        Box::new(Type::Byte),
                        string.len() as u32,
                    ),
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
                let Type::Pointer(t) = exp.value_type else {
                    return Err(CompilerError::code_gen_error(
                        expression_pos,
                        "This is not a pointer type",
                    ));
                };

                Ok(Value {
                    value_type: *t.clone(),
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
                        value_type: t,
                    })
                } else if let Some(value) = self.symbol_table.fetch_argument(&id) {
                    Ok(value)
                } else {
                    let var_i32 = id.parse::<i32>();
                    if let Ok(var) = var_i32 {
                        let i32_type = self.context.i32_type();
                        Ok(Value {
                            value_type: Type::Int,
                            value: i32_type.const_int(var as u64, false).into(),
                        })
                    } else {
                        let var_f32 = id.parse::<f32>();
                        if let Ok(var) = var_f32 {
                            let f32_type = self.context.f32_type();
                            Ok(Value {
                                value_type: Type::Float,
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
                let index = self.build_expression(*index)?;
                let value = self.build_expression(*expression)?;

                let (ptr, element_t) = self.get_array_ptr(value, index, expression_pos)?;
                return Ok(Value {
                    value: (self.builder.build_load(
                        element_t.basic_type_enum(self.context).unwrap(),
                        ptr,
                        "load_value",
                    )?),
                    value_type: element_t,
                });
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
                    // FIXME: might want to use alloca as an optimisation, also
                    // need to consider frees to prevent memory leaks...
                    // Might consider using reference counting or garabge
                    // collection like go
                    let ptr = self.builder.build_array_malloc(
                        arr_type.basic_type_enum(self.context).unwrap(),
                        self.context.i32_type().const_int(size as u64, false),
                        "array_init",
                    )?;
                    Ok(Value {
                        value_type: Type::Array(arr_type, size),
                        value: ptr.into(),
                    })
                } else {
                    todo!("Currently not supporting instantiation of other types.")
                }
            }
            _ => todo!(),
        }
    }

    pub fn get_array_ptr(
        &mut self,
        value: Value<'ctx>,
        index: Value<'ctx>,
        expression_pos: SourcePosition,
    ) -> Result<(PointerValue<'ctx>, Type), CompilerError> {
        let index = index.value.into_int_value();
        let updated_index = self.builder.build_int_add(
            index,
            self.context.i32_type().const_int(1, false),
            "addIndex",
        )?;
        if value.value.is_pointer_value() {
            if let Type::Array(element_t, size) = &value.value_type {
                let t: BasicTypeEnum = element_t
                    .basic_type_enum(self.context)
                    .unwrap()
                    .array_type(*size)
                    .into();
                unsafe {
                    let array_ptr = self.builder.build_in_bounds_gep(
                        t,
                        value.value.into_pointer_value(),
                        &[self.context.i64_type().const_zero(), updated_index],
                        "build store",
                    )?;
                    Ok((array_ptr, (**element_t).clone()))
                }
            } else if let Type::Pointer(element_t) = &value.value_type {
                let t: BasicTypeEnum = element_t.basic_type_enum(self.context).unwrap();
                unsafe {
                    let reference = self.builder.build_in_bounds_gep(
                        t,
                        value.value.into_pointer_value(),
                        &[updated_index],
                        "build store",
                    )?;
                    return Ok((reference, (**element_t).clone()));
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

    pub fn build_call(
        &mut self,
        callee: String,
        arguments: Vec<Expression>,
        position: SourcePosition,
    ) -> Result<Value<'ctx>, CompilerError> {
        let (Some(function), Some(function_prototype)) = (
            self.module.get_function(&callee),
            self.symbol_table.fetch_function(&callee),
        ) else {
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
            .build_call(function, args.as_slice(), "calltmp")?
            .try_as_basic_value()
            .left_or(self.context.i32_type().const_int(0, false).into());

        Ok(Value {
            value,
            value_type: function_prototype.return_type,
        })
    }

    fn build_binary(
        &mut self,
        lhs_expression: Expression,
        operation: BinOperation,
        rhs_expression: Expression,
    ) -> Result<Value<'ctx>, CompilerError> {
        let position = (lhs_expression.col, lhs_expression.lnum);
        let lhs = self.build_expression(lhs_expression)?;
        let rhs = self.build_expression(rhs_expression)?;
        let binary_value = if lhs.value.is_int_value() && rhs.value.is_int_value() {
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
        };

        // NOTE: Giving preference to the LHS is not a good idea, we should use
        // the whatever type that the operation has been coerced into.
        Ok(Value {
            value_type: lhs.value_type,
            value: binary_value,
        })
    }
}
