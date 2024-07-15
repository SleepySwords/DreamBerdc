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

    // NOTE: maybe as an optimisation step, load the expression and get all the values using
    // extract values for members, when the majority of members has been selected?
}
