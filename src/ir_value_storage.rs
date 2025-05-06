use crate::error::SymbolLookupResult;
use crate::{error::SymbolLookupError, symbols::SymbolPath};
use inkwell::types::AnyTypeEnum;
use inkwell::values::{BasicValue, FunctionValue, PointerValue};
use std::collections::BTreeMap;

pub struct IRValueStorage<'ctx> {
    pointers: BTreeMap<SymbolPath, PointerValue<'ctx>>,
    functions: BTreeMap<SymbolPath, FunctionValue<'ctx>>,
    basic_values: BTreeMap<SymbolPath, Box<dyn BasicValue<'ctx> + 'ctx>>,
    global_types: BTreeMap<SymbolPath, AnyTypeEnum<'ctx>>,
}

impl<'ctx> IRValueStorage<'ctx> {
    pub fn new() -> Self {
        Self {
            pointers: BTreeMap::new(),
            functions: BTreeMap::new(),
            basic_values: BTreeMap::new(),
            global_types: BTreeMap::new(),
        }
    }

    pub fn register_ptr(&mut self, path: SymbolPath, ptr: PointerValue<'ctx>) {
        self.pointers.insert(path, ptr);
    }

    pub fn register_func(&mut self, path: SymbolPath, func: FunctionValue<'ctx>) {
        self.functions.insert(path, func);
    }

    pub fn register_type(&mut self, path: SymbolPath, type_enum: AnyTypeEnum<'ctx>) {
        self.global_types.insert(path, type_enum);
    }

    pub fn register_basic_value(
        &mut self,
        path: SymbolPath,
        basic_value: Box<dyn BasicValue<'ctx> + 'ctx>,
    ) {
        self.basic_values.insert(path, basic_value);
    }

    pub fn find_symbol(
        &self,
        lookup_path: &SymbolPath,
        name: &str,
    ) -> SymbolLookupResult<&PointerValue<'ctx>> {
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

        loop {
            let func = self.functions.get(&path.sub(name));
            if func.is_some() {
                return func;
            }

            if path.is_empty() {
                break;
            }
            path.truncate_to_parent();
        }

        None
    }

    pub fn find_basic_value(
        &self,
        lookup_path: &SymbolPath,
        name: &str,
    ) -> Option<&dyn BasicValue<'ctx>> {
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

    pub fn find_global_type(
        &self,
        lookup_path: &SymbolPath,
        name: &str,
    ) -> Option<AnyTypeEnum<'ctx>> {
        let mut path = lookup_path.clone();

        loop {
            let value_opt = self.global_types.get(&path.sub(name));
            if let Some(value) = value_opt {
                return Some(*value);
            }

            if path.is_empty() {
                break;
            }
            path.truncate_to_parent();
        }

        None
    }
}
