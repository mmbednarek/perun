use std::collections::BTreeMap;
use crate::{error::SymbolLookupError, symbols::SymbolPath};
use crate::error::SymbolLookupResult;
use inkwell::values::{PointerValue, FunctionValue, BasicValue};

pub struct AddressTable<'ctx> {
    pointers: BTreeMap<SymbolPath, PointerValue<'ctx>>,
    functions: BTreeMap<SymbolPath, FunctionValue<'ctx>>,
    basic_values: BTreeMap<SymbolPath, Box<dyn BasicValue<'ctx> + 'ctx>>,
}

impl<'ctx> AddressTable<'ctx> {
    pub fn new() -> Self {
        Self{pointers: BTreeMap::new(), functions: BTreeMap::new(), basic_values: BTreeMap::new()}
    }

    pub fn register_ptr(&mut self, path: SymbolPath, ptr: PointerValue<'ctx>) {
        self.pointers.insert(path, ptr);
    }

    pub fn register_func(&mut self, path: SymbolPath, func: FunctionValue<'ctx>) {
        self.functions.insert(path, func);
    }

    pub fn register_basic_value(&mut self, path: SymbolPath, basic_value: Box<dyn BasicValue<'ctx> + 'ctx>) {
        self.basic_values.insert(path, basic_value);
    }

    pub fn find_symbol(&self, lookup_path: &SymbolPath, name: &str) -> SymbolLookupResult<&PointerValue<'ctx>> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let sym = self.pointers.get(&path.sub(name));
            if let Some(symbol) = sym {
                return Ok(symbol);
            }
            path.truncate_to_parent();
        }

        Err(SymbolLookupError::NoSymbolFound(name.to_string()))
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

    pub fn find_basic_value(&self, lookup_path: &SymbolPath, name: &str) -> Option<&dyn BasicValue<'ctx>> {
        let mut path = lookup_path.clone();

        while !path.is_empty() {
            let value_opt = self.basic_values.get(&path.sub(name));
            if let Some(value) = value_opt {
                return Some(value.as_ref());
            }
            path.truncate_to_parent();
        }

        None
    }
}

