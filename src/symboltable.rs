use std::collections::{HashMap, VecDeque};

use inkwell::values::{AnyValueEnum, BasicValueEnum, FloatValue, IntValue, PointerValue};

use crate::{types::{Type, Value}, utils::Mutable};

#[derive(Default)]
pub struct SymbolTable<'ctx> {
    symbol_table: VecDeque<HashMap<String, BasicValueEnum<'ctx>>>,
    ptr_symbol_table: VecDeque<HashMap<String, Value<'ctx>>>,
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

    pub fn fetch_value(&mut self, name: &String) -> Option<BasicValueEnum<'a>> {
        for i in 0..self.symbol_table.len() {
            if self.symbol_table[i].contains_key(name) {
                return Some(self.symbol_table[i][name]);
            }
        }
        None
    }

    // FIX: Should return a result
    pub fn store_value(&mut self, name: String, ptr: BasicValueEnum<'a>) {
        self.symbol_table[0].insert(name, ptr);
    }

    pub fn fetch_variable(&mut self, name: &String) -> Option<&Value<'a>> {
        for i in 0..self.ptr_symbol_table.len() {
            if self.ptr_symbol_table[i].contains_key(name) {
                return Some(&self.ptr_symbol_table[i][name]);
            }
        }
        None
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
    pub fn store_variable_ptr(&mut self, name: String, ptr: PointerValue<'a>, mutability: Mutable) {
        let variable = Value {
            value_type: Type::Void,
            value: ptr.into(),
            mutability,
        };
        self.ptr_symbol_table[0].insert(name, variable);
    }
}
