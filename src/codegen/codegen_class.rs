use itertools::Itertools;

use crate::ast::Class;

use super::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub fn build_class(&mut self, class: Class) {
        let class_type = self.context.opaque_struct_type(&class.name);
        class_type.set_body(
            &class
                .fields
                .iter()
                .map(|f| f.field_type.basic_type_enum(self.context).unwrap())
                .collect_vec(),
            false,
        );

        self.symbol_table
            .class_table
            .insert(class.name.clone(), class);
    }
}
