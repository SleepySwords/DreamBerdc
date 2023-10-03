// TODO: create scopes

use std::collections::{HashMap, VecDeque};

use inkwell::values::{AnyValueEnum, FloatValue, IntValue, PointerValue};

use crate::{types::Type, utils::Mutable};

#[derive(Default)]
pub struct SymbolTable<'ctx> {
    symbol_table: VecDeque<HashMap<String, IntValue<'ctx>>>,
    ptr_symbol_table: VecDeque<HashMap<String, Variable<'ctx>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn push_scope(&mut self) {
        self.ptr_symbol_table.push_front(HashMap::new());
        self.symbol_table.push_front(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        // We don't want to destroy the global scope.
        if self.ptr_symbol_table.len() > 1 {
            self.ptr_symbol_table.pop_front();
            self.symbol_table.pop_front();
        }
    }

    pub fn fetch_value(&mut self, name: &String) -> Option<IntValue<'a>> {
        for i in 0..self.symbol_table.len() {
            if self.symbol_table[i].contains_key(name) {
                return Some(self.symbol_table[i][name]);
            }
        }
        None
    }

    // FIX: Should return a result
    pub fn store_value(&mut self, name: String, ptr: IntValue<'a>) {
        self.symbol_table[0].insert(name, ptr);
    }

    pub fn fetch_variable_ptr(&mut self, name: &String) -> Option<PointerValue<'a>> {
        for i in 0..self.ptr_symbol_table.len() {
            if self.ptr_symbol_table[i].contains_key(name) {
                return Some(self.ptr_symbol_table[i][name].pointer_value());
            }
        }
        None
    }

    // FIX: Should return a result
    pub fn store_variable_ptr(&mut self, name: String, ptr: PointerValue<'a>) {
        let variable = Variable {
            value_type: Type::Void,
            value: ptr.into(),
            mutability: Mutable::ALL,
        };
        self.ptr_symbol_table[0].insert(name, variable);
    }
}

pub struct Variable<'ctx> {
    // value could also act as value_type
    value_type: Type,
    value: AnyValueEnum<'ctx>,
    mutability: Mutable,
}

impl<'ctx> Variable<'ctx> {
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
}
