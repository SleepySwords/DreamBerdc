use std::collections::{HashMap, VecDeque};

use inkwell::values::PointerValue;

use crate::{
    ast::{Class, Prototype},
    types::{Type, Value, Variable},
    utils::Mutable,
};

pub struct SymbolTable<'ctx> {
    arg_symbol_table: VecDeque<HashMap<String, Value<'ctx>>>,
    var_symbol_table: VecDeque<HashMap<String, Variable<'ctx>>>,
    fun_symbol_table: HashMap<String, Prototype>,
    pub class_table: HashMap<String, Class>,
}

impl<'ctx> Default for SymbolTable<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> SymbolTable<'ctx> {
        let mut symbol_table = VecDeque::new();
        symbol_table.push_front(HashMap::new());

        let mut ptr_symbol_table = VecDeque::new();
        ptr_symbol_table.push_front(HashMap::new());

        SymbolTable {
            arg_symbol_table: symbol_table,
            var_symbol_table: ptr_symbol_table,
            fun_symbol_table: HashMap::new(),
            class_table: HashMap::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.var_symbol_table.push_front(HashMap::new());
        self.arg_symbol_table.push_front(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        // We don't want to destroy the global scope.
        if self.var_symbol_table.len() > 1 {
            self.var_symbol_table.pop_front();
            self.arg_symbol_table.pop_front();
        }
    }

    pub fn fetch_function(&self, name: &str) -> Option<Prototype> {
        self.fun_symbol_table.get(name).cloned()
    }

    pub fn store_function(&mut self, name: String, value: Prototype) {
        self.fun_symbol_table.insert(name, value);
    }

    pub fn fetch_argument(&self, name: &str) -> Option<Value<'ctx>> {
        for i in 0..self.arg_symbol_table.len() {
            if self.arg_symbol_table[i].contains_key(name) {
                return Some(self.arg_symbol_table[i][name].clone());
            }
        }
        None
    }

    // FIX: Should return a result
    pub fn store_argument(&mut self, name: String, value: Value<'ctx>) {
        self.arg_symbol_table[0].insert(name, value);
    }

    pub fn fetch_variable(&self, name: &str) -> Option<&Variable<'ctx>> {
        for i in 0..self.var_symbol_table.len() {
            if self.var_symbol_table[i].contains_key(name) {
                return Some(&self.var_symbol_table[i][name]);
            }
        }
        None
    }

    // FIX: Should return a result
    pub fn store_variable_ptr(
        &mut self,
        name: String,
        ptr: PointerValue<'ctx>,
        value_type: Type,
        mutability: Mutable,
    ) {
        let variable = Variable {
            value_type,
            pointer: ptr,
            mutability,
        };
        self.var_symbol_table[0].insert(name, variable);
    }
}
