use itertools::Itertools;

use crate::{
    ast::{Class, Expression},
    compile_error::CompilerError,
    types::{Type, Value},
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn build_class(&mut self, class: Class) {
        let class_type = self.context.opaque_struct_type(&class.name);
        class_type.set_body(
            &class
                .fields
                .iter()
                .map(|f| f.field_type.basic_type_enum(self.context, &self.symbol_table).unwrap())
                .collect_vec(),
            false,
        );

        self.symbol_table
            .class_table
            .insert(class.name.clone(), class);
    }

    pub fn build_member(
        &mut self,
        expression: Expression,
        identifier: String,
    ) -> Result<Value<'ctx>, CompilerError> {
        let pos = expression.pos();
        let expression = self.build_expression(expression)?;
        if let Type::Class(class_name) = expression.value_type {
            let Some(pos) = self
                .symbol_table
                .class_table
                .get(&class_name)
                .and_then(|f| f.fields.iter().position(|field| field.name == identifier))
            else {
                return Err(CompilerError::code_gen_error(
                    pos,
                    format!("No such member {} in the class {}", identifier, class_name),
                ));
            };

            Ok(Value {
                value: self.builder.build_extract_value( // FIXME: this should use GEP, as the way
                                                         // we currently have it, we must load the
                                                         // entire struct before being able to
                                                         // access a value. With GEP, only the
                                                         // value needs to be loaded.
                                                         // However, this requires a pretty
                                                         // extensive refactor, loads must be
                                                         // delayed until absolutely necessary (ie:
                                                         // ops, assignments, functions calls). So 
                                                         // we are able to fetch ptrs to values to
                                                         // use it.
                                                         // Todo
                                                         // - Create a build_lvalue function that
                                                         // does all the building
                                                         // - Create a build_rvalue function that
                                                         // handles loading of lvalues in addition
                                                         // to literals, binary ops, etc..
                    expression.value.into_struct_value(),
                    pos.try_into().unwrap(),
                    "extract",
                )?,
                value_type: self.symbol_table.class_table[&class_name].fields[pos]
                    .field_type
                    .clone(),
            })
        } else {
            return Err(CompilerError::code_gen_error(
                pos,
                format!(
                    "Cannot get a member of the type {:?}",
                    expression.value_type
                ),
            ));
        }
    }
}
