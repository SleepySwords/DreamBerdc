// TODO: create scopes

use std::collections::{HashMap, VecDeque};

use inkwell::values::{IntValue, PointerValue};

#[derive(Default)]
pub struct SymbolTable<'ctx> {
    symbol_table: VecDeque<HashMap<String, IntValue<'ctx>>>,
    ptr_symbol_table: VecDeque<HashMap<String, PointerValue<'ctx>>>,
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

    pub fn fetch_variable_ptr(&mut self, name: &String) -> Option<PointerValue<'a>> {
        for i in 0..self.ptr_symbol_table.len() {
            if self.ptr_symbol_table[i].contains_key(name) {
                return Some(self.ptr_symbol_table[i][name]);
            }
        }
        return None;
    }

    // FIX: Should return a result
    pub fn store_variable_ptr(&mut self, name: String, ptr: PointerValue<'a>) {
        self.ptr_symbol_table[0].insert(name, ptr);
    }
}
