use std::collections::BTreeMap;
use crate::symbols::SymbolPath;
use inkwell::values::{PointerValue, FunctionValue};

pub struct AddressTable<'ctx> {
    pointers: BTreeMap<SymbolPath, PointerValue<'ctx>>,
    functions: BTreeMap<SymbolPath, FunctionValue<'ctx>>,
}

impl<'ctx> AddressTable<'ctx> {
    pub fn new() -> Self {
        Self{pointers: BTreeMap::new(), functions: BTreeMap::new()}
    }

    pub fn register_ptr(&mut self, path: SymbolPath, ptr: PointerValue<'ctx>) {
        self.pointers.insert(path, ptr);
    }

    pub fn register_func(&mut self, path: SymbolPath, func: FunctionValue<'ctx>) {
        self.functions.insert(path, func);
    }

    pub fn find_symbol(&self, lookup_path: &SymbolPath, name: &str) -> Option<&PointerValue<'ctx>> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let sym = self.pointers.get(&path.sub(name));
            if sym.is_some() {
                return sym;
            }
            path.truncate_to_parent();
        }

        None
    }

    pub fn find_func(&self, lookup_path: &SymbolPath, name: &str) -> Option<&FunctionValue<'ctx>> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let func = self.functions.get(&path.sub(name));
            if func.is_some() {
                return func;
            }
            path.truncate_to_parent();
        }

        None
    }
}

