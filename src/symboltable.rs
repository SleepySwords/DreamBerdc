use std::collections::{HashMap, VecDeque};

use inkwell::values::{BasicValueEnum, PointerValue};

use crate::{
    ast::Class,
    types::{Type, Variable},
    utils::Mutable,
};

pub struct SymbolTable<'ctx> {
    symbol_table: VecDeque<HashMap<String, BasicValueEnum<'ctx>>>,
    ptr_symbol_table: VecDeque<HashMap<String, Variable<'ctx>>>,
    pub class_table: HashMap<String, Class>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> SymbolTable<'a> {
        let mut symbol_table = VecDeque::new();
        symbol_table.push_front(HashMap::new());

        let mut ptr_symbol_table = VecDeque::new();
        ptr_symbol_table.push_front(HashMap::new());

        SymbolTable {
            symbol_table,
            ptr_symbol_table,
            class_table: HashMap::new(),
        }
    }

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

    pub fn fetch_value(&self, name: &str) -> Option<BasicValueEnum<'a>> {
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

    pub fn fetch_variable(&self, name: &str) -> Option<&Variable<'a>> {
        for i in 0..self.ptr_symbol_table.len() {
            if self.ptr_symbol_table[i].contains_key(name) {
                return Some(&self.ptr_symbol_table[i][name]);
            }
        }
        None
    }

    // FIX: Should return a result
    pub fn store_variable_ptr(
        &mut self,
        name: String,
        ptr: PointerValue<'a>,
        value_type: Type,
        mutability: Mutable,
    ) {
        let variable = Variable {
            value_type,
            pointer: ptr,
            mutability,
        };
        self.ptr_symbol_table[0].insert(name, variable);
    }
}
